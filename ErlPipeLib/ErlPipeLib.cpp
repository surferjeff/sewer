#include "stdafx.h"

#include "ErlPipeLib.h"
#include "overlapped_buffer.h"
#include "strconv.h"

const PendingPipe kZeroPendingPipe = { 0 };

tstring GetUserName() {
  tstring result;
  const size_t user_name_buffer_len = UNLEN + 1;
  TCHAR user_name[user_name_buffer_len] = { 0 };
  DWORD user_name_len = user_name_buffer_len;
  if (GetUserName(user_name, &user_name_len)) {
    result.assign(user_name, user_name_len -1);
  }
  return result;
}

NodeNames SplitNodeName(const char* node_name) {
  NodeNames names;
  names.node.assign(node_name);
  const size_t at_offset = names.node.find('@');
  if (string::npos != at_offset) {
    names.host.assign(names.node, at_offset + 1, names.node.size());
  }
  return names;
}

NodeNames SplitNodeName(const ETERM* node_name_term) {
  NodeNames names;
  if (ERL_IS_ATOM(node_name_term))  {
    names.node.assign(ERL_ATOM_PTR(node_name_term),
      ERL_ATOM_SIZE(node_name_term));
    const size_t at_offset = names.node.find('@');
    if (string::npos != at_offset) {
      names.host.assign(names.node, at_offset + 1, names.node.size());
    }
  }
  return names;
}

bool CreatePendingPipe(const tstring& pipe_name, HANDLE hevent,
  bool first_instance, string* error_string, PendingPipe* pending)
{
  stringstream error;
  pending->overlapped.hEvent = hevent;
  DWORD flags = PIPE_ACCESS_DUPLEX | FILE_FLAG_OVERLAPPED;
  if (first_instance)
      flags = flags | FILE_FLAG_FIRST_PIPE_INSTANCE;

  // Create the pipe.
  pending->pipe = CreateNamedPipe( 
         pipe_name.c_str(),  // pipe name 
         flags,
         PIPE_TYPE_MESSAGE |      // byte-type pipe 
         PIPE_READMODE_BYTE |  // byte-read mode 
         PIPE_WAIT,               // blocking mode 
         PIPE_UNLIMITED_INSTANCES, // number of instances 
         PIPE_BUFSIZE,   // output buffer size 
         PIPE_BUFSIZE,   // input buffer size 
         PIPE_TIMEOUT,            // client time-out 
         NULL);                   // default security attributes 
  if (INVALID_HANDLE_VALUE == pending->pipe) {
    error << "Failed to create pipe " << pipe_name 
      << ".  GetLastError() returned " << GetLastError() << ".";
    *error_string = error.str();
    return false;
  }
  if (ConnectNamedPipe(pending->pipe, &pending->overlapped)) {
    error << "ConnectNamedPipe(" << pipe_name << ") unexpectedly "
      "returned TRUE!.";
    CloseHandle(pending->pipe);
    *error_string = error.str();
    return false;
  }
  DWORD const last_error = GetLastError();
  switch (last_error) {
  case ERROR_IO_PENDING:
    break;  // Perfect, it's waiting to connect.
  case ERROR_PIPE_CONNECTED:
    SetEvent(pending->overlapped.hEvent);
    break;
  default:
    CloseHandle(pending->pipe);
    error << "Failed to connect pipe " << pipe_name
      << ".  GetLastError() returned " << GetLastError() << ".";
    *error_string = error.str();
    return false;
  }
  return true;
}

bool CreatePendingPipe(PipeState* pipe_state, HANDLE hevent,
  bool first_instance, string* error_string)
{
  pipe_state->pending_pipes.push_back(kZeroPendingPipe);
  bool const created = CreatePendingPipe(pipe_state->pipe_name, hevent,
    first_instance, error_string, &pipe_state->pending_pipes.back());
  if (!created) {
    pipe_state->pending_pipes.pop_back();
  }
  return created;
}

tstring GetPipeBrokerPipeName() {
  tstringstream pipe_name_stream;
  pipe_name_stream << _T("\\\\.\\pipe\\erlang\\user\\") << GetUserName()
    << _T("\\sewerpipes");
  return pipe_name_stream.str();
}

ConnectionPool* FindConnectionPool(PipeState* pipe_state,
  const string& node_name)
{
  ConnectionPools::iterator const iter =
    pipe_state->connection_pools.find(node_name);
  bool const found = iter != pipe_state->connection_pools.end();
  return found ? iter->second : NULL;
}


void ClosePipe(PipeState* pipe_state, HANDLE pipe) {
  // This is O(n).
  for (LivePipes::iterator i = pipe_state->live_pipes.begin(),
    end = pipe_state->live_pipes.end(); i != end; ++i)
  {
    if (i->second.handle == pipe) {
      HiddenConnections::value_type needle;
      const string& node_name = needle.first = i->first;
      pipe_state->live_pipes.erase(i);
      CloseHandle(pipe);
      // Erase from hidden connections.
      HiddenConnections& hiddens = pipe_state->hidden_connections;
      for (HiddenConnections::iterator i = hiddens.find(needle);
        i != hiddens.end() && i->first == needle.first; hiddens.erase(i++))
      {
        hiddens.erase(make_pair(i->second, i->first));
      }
      // Erase from public pools.
      ConnectionPool* const pool = FindConnectionPool(pipe_state, node_name);
      if (pool) {
        pool->erase(node_name);
        if (pool->empty())
          delete pool;
        pipe_state->connection_pools.erase(node_name);
      }
      break;
    }
  }
}

bool AppendAtom(string* s, ETERM* atom) {
  if (NULL != atom && ERL_IS_ATOM(atom)) {
    s->append(ERL_ATOM_PTR(atom), ERL_ATOM_SIZE(atom));
    return true;
  }
  return false;
}