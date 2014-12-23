// doubler.cpp : Defines the entry point for the application.
//

#include "stdafx.h"
#include "sewerlib.h"

#include "stdstate.h"
#include "overlapped_buffer.h"
#include "std_overlapped_io.h"
#include "ErlPipeLib.h"
#include "erl_list_iterator.h"

enum {
  PIPE_BROKER_CONNECT_TIMEOUT = 10000,
  NODEDOWN_TIMEOUT = 3000,
};

void WhenReadNextTermFromPipeComplete(OverlappedBuffer* ob,
  bool succeeded);
void WhenReadFirstTermFromPipeComplete(OverlappedBuffer* ob,
  bool succeeded);
HANDLE ConnectPipeBroker(StdState* stdstate, DWORD timeout_ms);

void WriteFatalError(StdState* stdstate, const char* error_msg) {
  OutputDebugStringA(error_msg);
  OutputDebugStringA("\n");
  ScopedTerm error(erl_format("{error, ~s}", error_msg));
  stdstate->WriteFatalToStdout(error);
}

HANDLE GetConnection(StdState* stdstate, PipeState* pipe_state,
  const char* node_name)
{
  LivePipes::iterator const found = pipe_state->live_pipes.find(node_name);
  if (found != pipe_state->live_pipes.end()) {
    return found->second.handle;
  }
  ScopedTerm getpipe(erl_format("{getpipe, ~a, ~a, ~a}",
    pipe_state->node_name.c_str(), ToCStr(pipe_state->node_visibility),
    node_name));
  vector<unsigned char> result_buffer(2048);
  DWORD bytes_read = 0;
  if (!TermCallNamedPipe(GetPipeBrokerPipeName().c_str(), getpipe, 
    &result_buffer[0], static_cast<DWORD>(result_buffer.size()),
    &bytes_read, PIPE_BROKER_CONNECT_TIMEOUT))
    return INVALID_HANDLE_VALUE;
  ScopedTerm result(erl_decode(&result_buffer[4]));
  if (NULL == result)
    return INVALID_HANDLE_VALUE;
  ScopedTerm takepipe(erl_format("{takepipe, Visibility, Handle, Pid}"));
  if (!erl_match(takepipe, result))
    return INVALID_HANDLE_VALUE;
  string visibility;
  if (!AppendAtom(&visibility, ErlVarContent(takepipe, "Visibility")))
    return INVALID_HANDLE_VALUE;
  LivePipe pipe = { INVALID_HANDLE_VALUE };
  pipe.visibility = visibility == "hidden" ? HIDDEN_NODE : VISIBLE_NODE;
  ErlVarContent handle(takepipe, "Handle");
  if (!ERL_IS_INTEGER(handle.term_))
    return INVALID_HANDLE_VALUE;
  pipe.handle = reinterpret_cast<HANDLE>(ERL_INT_VALUE(handle.term_));
  pipe.pid = erl_var_content(takepipe, "Pid");   
  pipe_state->live_pipes.insert(make_pair(node_name, pipe));
  return pipe.handle;
}

void Connect(StdState* const stdstate, PipeState* pipe_state,
  ETERM* node_name_term, ETERM* from, ETERM* ref)
{
  if (!ERL_IS_ATOM(node_name_term)) {
    ScopedTerm reply(erl_format("{call_completed, ~w, ~w, "
      "{error, \"NodeName must be an atom.\"}}", from, ref));
    return stdstate->WriteTermToStdout(reply);
  }
  const string node_name(ERL_ATOM_PTR(node_name_term),
    ERL_ATOM_SIZE(node_name_term));
  const size_t at_offset = node_name.find('@');
  if (string::npos == at_offset) {
    ScopedTerm reply(erl_format("{call_completed, ~w, ~w, "
      "{error, \"NodeName must have a @ character.\"}}", from, ref));
    return stdstate->WriteTermToStdout(reply);
  }
  HANDLE const pipe = GetConnection(stdstate, pipe_state, node_name.c_str());
  if (INVALID_HANDLE_VALUE == pipe) {
    ScopedTerm reply(erl_format("{call_completed,  ~w, ~w, "
      "{error, \"No such pipe.\"}}", from, ref));
    stdstate->WriteTermToStdout(reply);
  } else {
    ScopedTerm reply(erl_format("{call_completed, ~w, ~w, ok}", from, ref));
    return stdstate->WriteTermToStdout(reply);
  }
}

struct FromRef {
  FromRef(ETERM* afrom, ETERM* aref)
    : from(erl_copy_term(afrom)), ref(erl_copy_term(aref)) { }
  ~FromRef() {
    erl_free(from);
    erl_free(ref);
  }
  ETERM *const from, *const ref;
private:
  FromRef(const FromRef&);
};

void WhenReadNodesCommandCompletes(OverlappedBuffer* ob, bool succeeded) {
  ScopedOverlappedBuffer delete_ob(ob);
  std::auto_ptr<FromRef> fromref(reinterpret_cast<FromRef*>(ob->pvoid));
  if (!succeeded) {
    ScopedTerm reply(erl_format(
      "{call_completed, ~w, ~w, "
      "{error, \"Failed to read result from broker pipe.\"}}",
      fromref->from, fromref->ref));
    ob->stdstate->WriteTermToStdout(reply);
    return;
  }
  ScopedTerm result(erl_decode(ob->buffer));
  if (NULL == result) {
    ScopedTerm reply(erl_format(
      "{call_completed, ~w, ~w, "
      "{error, \"Pipe broker returned corrupt response.\"}}",
      fromref->from, fromref->ref));
    ob->stdstate->WriteTermToStdout(reply);
    return;
  }
  ScopedTerm call_completed(erl_format("{call_completed, ~w, ~w, ~w}",
    fromref->from, fromref->ref, result.term_));
  ob->stdstate->WriteTermToStdout(call_completed);
}

void WhenWriteNodesCommandCompletes(OverlappedBuffer* ob, bool succeeded) {
  FromRef* const fromref = reinterpret_cast<FromRef*>(ob->pvoid);
  if (!succeeded) {
    ScopedTerm reply(erl_format(
      "{call_completed, ~w, ~w, {error, \"Failed to write to broker pipe.\"}}",
      fromref->from, fromref->ref));
    ob->stdstate->WriteTermToStdout(reply);
    delete fromref;
    return;
  }
  ReadTermFromPipe(ob->pipe, &WhenReadNodesCommandCompletes, fromref,
    ob->stdstate);
}

void Nodes(StdState* const stdstate, PipeState* pipe_state,
  ETERM* node, ETERM* args, ETERM* from, ETERM* ref)
{
  HANDLE broker_pipe = ConnectPipeBroker(stdstate, PIPE_BROKER_CONNECT_TIMEOUT);
  if (INVALID_HANDLE_VALUE == broker_pipe) {
    ScopedTerm reply(erl_format(
      "{call_completed, ~w, ~w, {error, \"Could not connect to broker.\"}}",
      from, ref));
    stdstate->WriteTermToStdout(reply);
    return;
  }
  ScopedTerm command(erl_format("{nodes, ~w, ~w}", node, args));
  WriteTermToPipe(command, broker_pipe, &WhenWriteNodesCommandCompletes,
    new FromRef(from, ref), stdstate);
}

// Main function that handles commands coming from erlang.
void WhenReadNextTermFromStdinComplete(OverlappedBuffer* ob, bool succeeded) {
  StdState* const stdstate = ob->stdstate;
  PipeState* const pipe_state = reinterpret_cast<PipeState*>(ob->pvoid);
  if (!succeeded) {
    SetEvent(stdstate->done);
    DeleteOverlappedBuffer(ob);
    return;
  }
  stdstate->ReadTermFromStdin(&WhenReadNextTermFromStdinComplete, pipe_state);
  ScopedTerm term(erl_decode(ob->buffer));
  ScopedTerm send_command(erl_format("{send, Dest, ByteSize}"));
  if (erl_match(send_command, term)) {
    ErlVarContent dest(send_command, "Dest");
    ScopedTerm remote_registered_process(erl_format("{RegName, Node}"));
    const NodeNames names = erl_match(remote_registered_process, dest) ?
      SplitNodeName(ErlVarContent(remote_registered_process, "Node")) :
      SplitNodeName(ERL_PID_NODE(dest.term_));
    HANDLE const pipe = GetConnection(stdstate, pipe_state, names.node.c_str());
    if (INVALID_HANDLE_VALUE == pipe) {
      DeleteOverlappedBuffer(ob);
    } else {
      RelayTermToPipe(ob, pipe);
    }
    return;
  }
  ScopedTerm connect_command(erl_format("{connect, NodeName, From, Ref}"));
  if (erl_match(connect_command, term)) {
    Connect(stdstate, pipe_state, ErlVarContent(connect_command, "NodeName"),
      ErlVarContent(connect_command, "From"),
      ErlVarContent(connect_command, ("Ref")));
    DeleteOverlappedBuffer(ob);
    return;
  } 
  ScopedTerm nodes_command(erl_format("{nodes, Node, Args, From, Ref}"));
  if (erl_match(nodes_command, term)) {
    Nodes(stdstate, pipe_state, ErlVarContent(nodes_command, "Node"),
      ErlVarContent(nodes_command, "Args"),
      ErlVarContent(nodes_command, "From"),
      ErlVarContent(nodes_command, "Ref"));
    DeleteOverlappedBuffer(ob);
    return;
  }
  ScopedTerm error(erl_format("{bad_command, ~w}", term.term_));
  stdstate->WriteTermToStdout(error);
  DeleteOverlappedBuffer(ob);
}

struct InitOptions {
  NodeVisibility visibility;
};

void ParseInitOptions(ETERM* term, InitOptions* options) {
  for (ErlListIterator i(term), end; i != end; ++i) {
    string atom;
    if (AppendAtom(&atom, *i)) {
      if ("hidden" == atom) {
        options->visibility = HIDDEN_NODE;
      }
    }
  }
}

HANDLE ConnectPipeBroker(StdState* stdstate, DWORD timeout_ms) {
  // Try to connect to the broker.
  const tstring pipe_name = GetPipeBrokerPipeName();
  HANDLE pipe = CreateFile(pipe_name.c_str(),
    GENERIC_READ | GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
    FILE_FLAG_OVERLAPPED, NULL);
  if (INVALID_HANDLE_VALUE != pipe) {
    return pipe;
  }
  // Try launching the PipeBroker.  It lives in the same directory
  // as this module.  So start by getting the path to this module.
  PROCESS_INFORMATION proc_info = { 0 };
  STARTUPINFO start_info = { sizeof(start_info) };
  vector<wchar_t> app_path(MAX_PATH);
  DWORD app_path_len;
  while(true) {
    HMODULE sewerlib = GetModuleHandleW(L"sewerlib");
    app_path_len = GetModuleFileNameW(sewerlib, &app_path[0], app_path.size());
    if (app_path_len > 0)
      break;
    app_path.resize(app_path.size() * 2);
  }
  // Strip out the name of this module.
  BOOL stripped = PathRemoveFileSpecW(&app_path[0]);
  // And append the name of the EXE we want to run.
  static const TCHAR exe[] = _T("\\SewerPipes.exe");
  DWORD const appended_length = sizeof(exe) / sizeof(exe[0])
    + _tcslen(&app_path[0] + 1);
  if (app_path.size() < appended_length)
    app_path.resize(appended_length);
  BOOL appended = PathAppendW(&app_path[0], exe);
  TCHAR command_line[] = _T("SewerPipes");
  BOOL created = CreateProcessW(&app_path[0], NULL,
    NULL, NULL, FALSE, 0, NULL, NULL, &start_info, &proc_info);
  if (!created) {
    const DWORD error = GetLastError();
    char* error_text = NULL;
    FormatMessageA(
      FORMAT_MESSAGE_ALLOCATE_BUFFER | 
      FORMAT_MESSAGE_FROM_SYSTEM |
      FORMAT_MESSAGE_IGNORE_INSERTS,
      NULL,
      error,
      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
      (LPSTR) &error_text,
      0, NULL );
    static const char message[] = "Failed to create pipe broker %ls.\n%hs";
    vector<char> message_buffer(2 * app_path.size() + sizeof(message)
      + strlen(error_text));
    _snprintf(&message_buffer[0], message_buffer.size(), message,
      &app_path[0], error_text);
    WriteFatalError(stdstate, &message_buffer[0]);
    LocalFree(error_text);
    return pipe;
  }
  CloseHandle(proc_info.hProcess);
  CloseHandle(proc_info.hThread);
  // Wait and periodically try to connect to the pipe broker.
  ATL::CHandle const timeout(CreateWaitableTimer(NULL, TRUE, NULL));
  ATL::CHandle const retry(CreateWaitableTimer(NULL, FALSE, NULL));
  LARGE_INTEGER timeout_ftime;
  timeout_ftime.QuadPart = timeout_ms * -10000LL;
  const BOOL set_timeout =
    SetWaitableTimer(timeout, &timeout_ftime, 0, NULL, NULL, FALSE);
  LARGE_INTEGER retry_ftime;
  retry_ftime.QuadPart = 100 * -10000LL;
  const BOOL set_retry =
    SetWaitableTimer(retry, &retry_ftime, 100, NULL, NULL, FALSE);
  const HANDLE timers[] = { retry, timeout };
  DWORD waited;
  while (WAIT_OBJECT_0 == (waited =
    WaitForMultipleObjects(2, timers, FALSE, INFINITE)))
  {
    pipe = CreateFile(pipe_name.c_str(),
      GENERIC_READ | GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
      FILE_FLAG_OVERLAPPED, NULL);
    if (INVALID_HANDLE_VALUE != pipe) {
      return pipe;
    }
  }
  stringstream error;
  error << "Failed to open " << pipe_name;
  WriteFatalError(stdstate, error.str().c_str());
  return pipe;
}

void WhenReadFirstTermFromStdinComplete(OverlappedBuffer* ob, bool succeeded) {
  ScopedOverlappedBuffer scoped(ob);
  PipeState* const pipe_state = reinterpret_cast<PipeState*>(ob->pvoid);
  StdState* const stdstate = ob->stdstate;
  if (!succeeded) {
    SetEvent(stdstate->done);
    return;
  }
  ScopedTerm term(erl_decode(ob->buffer));
  ScopedTerm pattern(erl_format("{init, SewerPid, Options}"));
  if (!erl_match(pattern, term))
    return WriteFatalError(stdstate, "Expected {init, SewerPid, Options}");
  ErlVarContent sewer_pid(pattern, "SewerPid");
  if (!ERL_IS_PID(sewer_pid.term_))
    return WriteFatalError(stdstate, "Expected a pid.");
  // We got our node name.
  const char* node_name = ERL_PID_NODE(sewer_pid.term_);
  pipe_state->node_name = node_name;
  stdstate->SetNodeDownMessage(GetPipeBrokerPipeName(), pipe_state->node_name);
  InitOptions options = { VISIBLE_NODE };
  ParseInitOptions(ErlVarContent(pattern, "Options"), &options);
  pipe_state->node_visibility = options.visibility;

  const HANDLE pipe = ConnectPipeBroker(stdstate, 15000);
  if (INVALID_HANDLE_VALUE == pipe){
    return;  // Already reported error is ConnectPipeBroker.
  }
  // Send the handshake to the pipe.
  ScopedTerm handshake(erl_format("{~a, ~w}", ToCStr(options.visibility),
    sewer_pid.term_));
  WriteTermToPipe(handshake, pipe, NULL, NULL, stdstate);
  ReadTermFromPipe(pipe, &WhenReadFirstTermFromPipeComplete,
    pipe_state, stdstate);
}

void WhenReadNextTermFromPipeComplete(OverlappedBuffer* ob,
  bool succeeded)
{
  PipeState* const pipe_state = reinterpret_cast<PipeState*>(ob->pvoid);
  StdState* const stdstate = ob->stdstate;
  if (!succeeded) {
    ClosePipe(pipe_state, ob->pipe);
    DeleteOverlappedBuffer(ob);
    return;
  }
  // Read the next term.
  ReadTermFromPipe(ob->pipe, &WhenReadNextTermFromPipeComplete,
    pipe_state, stdstate);
  // Relay to stdout.
  stdstate->RelayTermToStdout(ob);
}

void WhenReadFirstTermFromPipeComplete(OverlappedBuffer* ob,
  bool succeeded)
{
  ScopedOverlappedBuffer scoped(ob);
  PipeState* const pipe_state = reinterpret_cast<PipeState*>(ob->pvoid);
  StdState* const stdstate = ob->stdstate;
  if (!succeeded) {
    CloseHandle(ob->pipe);
    return WriteFatalError(stdstate,
      "Failed to read first term from PipeBroker.");
  }
  ScopedTerm term(erl_decode(ob->buffer));
  ScopedTerm pattern(erl_format("ok"));
  if (!erl_match(pattern, term)) {
    CloseHandle(ob->pipe);
    return WriteFatalError(stdstate, "Expected 'ok' from PipeBroker.");
  }
  stdstate->WriteTermToStdout(term);
  stdstate->ReadTermFromStdin(&WhenReadNextTermFromStdinComplete, pipe_state);
  ReadTermFromPipe(ob->pipe, &WhenReadNextTermFromPipeComplete, pipe_state,
    stdstate);
}

void InnerLoop(StdState* stdstate) {
  __try {
    while (true) {
      DWORD waited = WaitForSingleObjectEx(stdstate->done, INFINITE, TRUE);
      if (WAIT_IO_COMPLETION != waited)
        break;
    }
  } __finally {
    if (stdstate->final_message)
      stdstate->SendNodeDownMessage();
  }
}

int MainLoop(StdState* stdstate) {
  erl_init(NULL, 0);
  PipeState pipe_state;
  stdstate->ReadTermFromStdin(&WhenReadFirstTermFromStdinComplete, &pipe_state);
  InnerLoop(stdstate);
  return 0;
}

void _MaybeReportPipeError(const OverlappedBuffer* ob, DWORD error_code,
  DWORD bytes_to_transfer, DWORD bytes_transferred, DWORD source_line) {
  if (error_code == 0 && bytes_to_transfer == bytes_transferred)
    return;
  string pipe_name;
  if (ob->stdstate->IsStdin(ob->pipe)) {
    pipe_name = "stdin";
  } else if (ob->stdstate->IsStdout(ob->pipe)) {
    pipe_name = "stdout";
  } else if (ob->pvoid) {
    PipeState* const pipe_state = reinterpret_cast<PipeState*>(ob->pvoid);
    for (LivePipes::iterator i = pipe_state->live_pipes.begin(),
      end = pipe_state->live_pipes.end(); i != end; ++i)
    {
      if (i->second.handle == ob->pipe) {
        pipe_name = i->first;
        break;
      }
    }
  }
  const char* const operation = ob->READ == ob->operation ? "read" : "write";
  char* error_text = NULL;
  const DWORD formatted = FormatMessageA(
    FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
    NULL, error_code, 0, (LPSTR)&error_text, 2048, NULL);
  assert(formatted);
  ATLTRACE2(
	  _T("Pipe error.\n Pipe name: %hs\n Operation: %hs\n Bytes to transfer: %d\n")
	  _T(" Bytes transferred: %d\n Error: %d: %hs Source line: %d\n"),
	  pipe_name.c_str(), operation, bytes_to_transfer, bytes_transferred,
	  error_code, error_text, source_line);
  if (pipe_name != "stdout") {
    ScopedTerm term(erl_format("{pipe_error, ~s, ~s, ~i, ~i, ~i, ~s, ~i}",
      pipe_name.c_str(), operation, bytes_to_transfer, bytes_transferred,
      error_code, error_text, source_line));
    ob->stdstate->WriteTermToStdout(term);
  }
  LocalFree((HLOCAL)error_text);
}

int ExeMain() {
  PipeStdState stdstate;
  return MainLoop(&stdstate);
}
