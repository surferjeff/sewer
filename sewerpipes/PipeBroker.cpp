// PipeBroker.cpp : Implements a daemon that connects erlang processes that want
// to talk via sewer pipes.
//
// Single threaded, uses overlapped IO.

#include "stdafx.h"
#include "PipeBroker.h"
#include "ErlPipeLib.h"
#include "overlapped_buffer.h"
#include "erl_list_iterator.h"

// Std is short for standard.  In other binaries, StdState contains pointers
// to stdin and stdout pipes.  But in the PipeBroker, we never read or write
// to stdin or stdout.
struct StdState {
  HANDLE done;  // Set when it's time for the PipeBroker to exit.
};

// Closes the pipe if the Write failed.
void WhenWriteTermToClientPipeComplete(OverlappedBuffer* ob,
  bool succeeded)
{
  PipeState* const pipe_state = reinterpret_cast<PipeState*>(ob->pvoid);
  if (!succeeded) {
    ClosePipe(pipe_state, ob->pipe);
  }
}

// When a node connects, add its pipe to the live pipe list.
// Replace any existing pipes by the same name, which should only happen in
// an error case.
// And send an "ok" to the node.
void WhenNodeConnected(StdState* stdstate, HANDLE pipe,
  PipeState* pipe_state, NodeVisibility visibility, ETERM* pid)
{
  if (!ERL_IS_PID(pid))  {
    CloseHandle(pipe);
    return;
  }
  string node_name(ERL_PID_NODE(pid));
  LivePipe live_pipe = { pipe, visibility, erl_copy_term(pid) };
  const pair<LivePipes::iterator, bool> inserted =
    pipe_state->live_pipes.insert(make_pair(node_name, live_pipe));
  if (!inserted.second) {
    LivePipes::iterator existing = inserted.first;
    CloseHandle(existing->second.handle);
    erl_free(existing->second.pid);
    existing->second = live_pipe;
  }

  static ScopedTerm ok(erl_mk_atom("ok"));
  WriteTermToPipe(ok, pipe, &WhenWriteTermToClientPipeComplete, pipe_state,
    stdstate);
}

// When a node shuts down, free resources and disconnect it from all connection pools.
// Maybe shut down this broker if there are no more pipes.
void WhenNodeDown(StdState* stdstate, HANDLE pipe, PipeState* pipe_state,
  ETERM* node_name)
{
  string snode_name;
  if (AppendAtom(&snode_name, node_name)) {
    LivePipes::iterator found = pipe_state->live_pipes.find(snode_name);
    if (found == pipe_state->live_pipes.end()) {
      // Unusual, maybe log something?
    } else {
      // Close the live pipe.
      CloseHandle(found->second.handle);
      erl_free(found->second.pid);
      pipe_state->live_pipes.erase(found);
      // Disconnect hidden connections.
      HiddenConnections::value_type needle;
      needle.first = snode_name;
      HiddenConnections& hiddens = pipe_state->hidden_connections;
      for (HiddenConnections::iterator i = hiddens.lower_bound(needle);
        i != hiddens.end() && i->first == needle.first; hiddens.erase(i++))
      {
        hiddens.erase(make_pair(i->second, i->first));
      }
      // Disconnect visible connections.
      ConnectionPool* const pool = FindConnectionPool(pipe_state, snode_name);
      if (pool) {
        pool->erase(snode_name);
        if (pool->empty()) {
          delete pool;
        }
        pipe_state->connection_pools.erase(snode_name);
      }
      // If no more live pipes, shut down pipe broker.
      if (pipe_state->live_pipes.empty()) {
        SetEvent(stdstate->done);
      }
    }
  }
  CloseHandle(pipe);
}

// Only one request per pipe, please.
void WhenWriteReturnValueToClientPipeComplete(OverlappedBuffer* ob,
  bool succeeded)
{
  CloseHandle(ob->pipe);
}

void HiddenConnect(PipeState* pipe_state, const string& node_a,
  const string& node_b)
{
  pipe_state->hidden_connections.insert(make_pair(node_a, node_b));
  pipe_state->hidden_connections.insert(make_pair(node_b, node_a));
}

void MergeConnectionPools(PipeState* pipe_state, ConnectionPool* a,
  ConnectionPool* b)
{
  if (a->size() > b->size())
    swap(a, b);
  for (ConnectionPool::iterator i = a->begin(), end = a->end(); i != end; ++i)
  {
    b->insert(*i);
    pipe_state->connection_pools[*i] = b;
  }
  delete a;
}

// Update a connection pool for this new connection.
void PublicConnect(PipeState* pipe_state, const string& node_a,
  const string& node_b)
{
  ConnectionPool *const pool_a = FindConnectionPool(pipe_state, node_a),
    *const pool_b = FindConnectionPool(pipe_state, node_b);
  if (pool_a) {
    if (pool_b) {
      // Both have their own visible connection pools.
      if (pool_a == pool_b) {
        // They're already in the same pool; nothing to do.
      } else {
        // Merge the pools.
        MergeConnectionPools(pipe_state, pool_a, pool_b);
      }
    } else {
      // Only node_a has a pool.  Add node_b.
      pool_a->insert(node_b);
      pipe_state->connection_pools.insert(make_pair(node_b, pool_a));
    }
  } else if (pool_b) {
    // Only node_b has a pool.  Add node_a.
    pool_b->insert(node_a);
    pipe_state->connection_pools.insert(make_pair(node_a, pool_b));
  } else {
    // Neither node has a visible connection pool.  Create one and add them.
    ConnectionPool* const pool = new ConnectionPool;
    pool->insert(node_a);
    pool->insert(node_b);
    pipe_state->connection_pools.insert(make_pair(node_a, pool));
    pipe_state->connection_pools.insert(make_pair(node_b, pool));
  }
}

void ReturnError(const char* anerror, StdState* stdstate, HANDLE pipe,
  PipeState* pipe_state)
{
  ScopedTerm error(erl_format("{error, ~s}", anerror));
  WriteTermToPipe(error, pipe,
    &WhenWriteReturnValueToClientPipeComplete, pipe_state, stdstate);
}

// A client has sent us a getpipe message.  We duplicate the pipe handle for
// the requested node into the requesting node's process.  And we send it a
// takepipe message with the handle.
void WhenGetPipe(StdState* stdstate, HANDLE pipe, PipeState* pipe_state,
  ETERM* requesting_node_term, ETERM* requesting_visibility_term,
  ETERM* node_name_term)
{
  LivePipe result = {INVALID_HANDLE_VALUE};
  string requesting_node, requesting_visibility, node_name;
  if (!(AppendAtom(&requesting_node, requesting_node_term) &&
    AppendAtom(&requesting_visibility, requesting_visibility_term) &&
    AppendAtom(&node_name, node_name_term)))
  {
    return ReturnError("Bad Request", stdstate, pipe, pipe_state);
  }
  LivePipes::iterator found = pipe_state->live_pipes.find(node_name);
  if (found == pipe_state->live_pipes.end()) {
    return ReturnError("No such node", stdstate, pipe, pipe_state);
  }
  ULONG process_id = 0;
  if (!GetNamedPipeClientProcessId(pipe, &process_id)) {
    return ReturnError("Failed to get pipe's client process id.", stdstate,
      pipe, pipe_state);
  }
  HANDLE hprocess = OpenProcess(PROCESS_DUP_HANDLE, false, process_id);
  if (hprocess == INVALID_HANDLE_VALUE) {
    return ReturnError("Failed to open the pipe's client process.", stdstate,
      pipe, pipe_state);
  }
  DuplicateHandle(GetCurrentProcess(), found->second.handle, hprocess,
    &result.handle, 0, FALSE, DUPLICATE_SAME_ACCESS);
  CloseHandle(hprocess);
  result.visibility = found->second.visibility;
  result.pid = found->second.pid;
  if (requesting_visibility == "visible"
    && result.visibility == VISIBLE_NODE)
  {
    PublicConnect(pipe_state, requesting_node, node_name);
  } else {
    HiddenConnect(pipe_state, requesting_node, node_name);
  }
  ScopedTerm takepipe(erl_format("{takepipe, ~a, ~i, ~w}",
    ToCStr(result.visibility), result.handle, result.pid));
  WriteTermToPipe(takepipe, pipe,
    &WhenWriteReturnValueToClientPipeComplete, pipe_state, stdstate);
}

struct NodesOptions {
  bool visible;
  bool hidden;
  bool known;
  bool this_;
};

void ParseNodesOptions(ETERM* arglist, NodesOptions* options) {
  for (ErlListIterator i(arglist), end; i != end; ++i) {
    string atom;
    if (AppendAtom(&atom, *i)) {
      if (atom == "visible") {
        options->visible = true;
      } else if (atom == "hidden") {
        options->hidden = true;
      } else if (atom == "known") {
        options->known = true;
      } else if (atom == "this") {
        options->this_ = true;
      } else if (atom == "connected") {
        options->visible = options->hidden = true;
      }
    }
  }
}

void CloseCommandPipe(OverlappedBuffer* ob, bool) {
  CloseHandle(ob->pipe);
}

// The client is asking for the list of connected nodes.
// Implements the erlang::nodes() function.
void WhenNodesCommand(StdState* stdstate, HANDLE pipe, PipeState* pipe_state,
  ETERM* node, ETERM* arglist)
{
  string node_name;
  if (!AppendAtom(&node_name, node)) {
    ScopedTerm error(erl_format("{error, \"Expected atom node name.\"}"));
    WriteTermToPipe(error, pipe, &CloseCommandPipe, NULL, stdstate);
    return;
  }
  LivePipes::iterator live_pipe = pipe_state->live_pipes.find(node_name);
  if (live_pipe == pipe_state->live_pipes.end()) {
    ScopedTerm error(erl_format("{error, \"No node named ~w.\"}", node));
    WriteTermToPipe(error, pipe, &CloseCommandPipe, NULL, stdstate);
    return;
  }
  NodesOptions options = { ERL_IS_EMPTY_LIST(arglist) };
  ParseNodesOptions(arglist, &options);
  set<string> nodes;
  if (options.known) {
    for (LivePipes::iterator i = pipe_state->live_pipes.begin(),
      end = pipe_state->live_pipes.end(); i != end; ++i)
    {
      nodes.insert(i->first);
    }
  } else {
    if (options.visible) {
      ConnectionPool* const pool = FindConnectionPool(pipe_state, node_name);
      if (pool) {
        nodes.insert(pool->begin(), pool->end());
      }
    }
    if (options.hidden) {
      HiddenConnections::value_type needle;
      needle.first = node_name;
      HiddenConnections& hiddens = pipe_state->hidden_connections;
      for (HiddenConnections::iterator i = hiddens.lower_bound(needle);
        i != hiddens.end() && i->first == needle.first; ++i)
      {
        nodes.insert(i->second);
      }
    }
  }
  if (options.this_) {
    nodes.insert(node_name);
  } else {
    nodes.erase(node_name);
  }
  ETERM* node_list = erl_mk_empty_list();
  for (set<string>::reverse_iterator i = nodes.rbegin(), end = nodes.rend();
    i != end; ++i)
  {
    ETERM* const atom = erl_mk_atom(i->c_str());
    node_list = erl_cons(atom, node_list);
  }
  WriteTermToPipe(node_list, pipe, &CloseCommandPipe, NULL, stdstate);
  erl_free_compound(node_list);
}

// Receive and parse the request from the client.
void WhenReadFirstTermFromClientPipeComplete(OverlappedBuffer* ob,
  bool succeeded)
{
  ScopedOverlappedBuffer scoped(ob);
  PipeState* const pipe_state = reinterpret_cast<PipeState*>(ob->pvoid);
  StdState* const stdstate = ob->stdstate;
  if (!succeeded) {
    CloseHandle(ob->pipe);
    return;
  }
  ScopedTerm term(erl_decode(ob->buffer));
  ScopedTerm getpipe(erl_format(
    "{getpipe, RequestingNode, RequestingVisibility, NodeName}"));
  if (erl_match(getpipe, term)) {
    return WhenGetPipe(stdstate, ob->pipe, pipe_state,
      ErlVarContent(getpipe, "RequestingNode"),
      ErlVarContent(getpipe, "RequestingVisibility"),
      ErlVarContent(getpipe, "NodeName"));
  }
  ScopedTerm visible_node(erl_format("{visible, Pid}"));
  if (erl_match(visible_node, term)) {
    return WhenNodeConnected(stdstate, ob->pipe, pipe_state, VISIBLE_NODE,
      ErlVarContent(visible_node, "Pid"));
  }
  ScopedTerm hidden_node(erl_format("{hidden, Pid}"));
  if (erl_match(hidden_node, term)) {
    return WhenNodeConnected(stdstate, ob->pipe, pipe_state, HIDDEN_NODE,
      ErlVarContent(hidden_node, "Pid"));
  }
  ScopedTerm nodedown(erl_format("{nodedown, NodeName}"));
  if (erl_match(nodedown, term)) {
    return WhenNodeDown(stdstate, ob->pipe, pipe_state,
      ErlVarContent(nodedown, "NodeName"));
  }
  ScopedTerm nodes_command(erl_format("{nodes, Node, Args}"));
  if (erl_match(nodes_command, term)) {
    return WhenNodesCommand(stdstate, ob->pipe, pipe_state,
      ErlVarContent(nodes_command, "Node"),
      ErlVarContent(nodes_command, "Args"));
  }
  CloseHandle(ob->pipe);
}

// Creates N pending pipes that are ready to accept client connections.
bool CreatePendingPipes(PipeState* pipe_state, string* error_string) {
  for (int i = 0; i < PIPE_INSTANCES; ++i) {
    HANDLE hevent = CreateEvent(NULL, TRUE, FALSE, NULL);
    if (INVALID_HANDLE_VALUE == hevent) {
      stringstream error;
      error << "Failed to create event for pipe connect."
        << "  GetLastError() returned " << GetLastError() << ".";
      *error_string = error.str();
      return false;
    }
    if (!CreatePendingPipe(pipe_state, hevent, 0 == i, error_string))
      return false;
  }
  return true;
}

// The main loop for the .exe.
int OverlappedLoop() {
  erl_init(NULL, 0);
  StdState stdstate = {
    CreateEvent(NULL, TRUE, FALSE, NULL)
  };
  PipeState pipe_state;
  Append(&pipe_state.pipe_name, GetPipeBrokerPipeName());
  string error;
  if (!CreatePendingPipes(&pipe_state, &error)) {
    return -1;
  }

  HANDLE wait_objects[MAXIMUM_WAIT_OBJECTS] = { stdstate.done };
  PendingPipes::iterator pending_iterators[MAXIMUM_WAIT_OBJECTS];
  while (true) {
    DWORD wait_object_count = 1;
    for(PendingPipes::iterator i = pipe_state.pending_pipes.begin(),
      end = pipe_state.pending_pipes.end(); i != end; ++ i) {
        pending_iterators[wait_object_count] = i;
        wait_objects[wait_object_count++] = i->overlapped.hEvent;
    }
    DWORD waited = 0;
    do {
      waited = WaitForMultipleObjectsEx(wait_object_count,
        wait_objects, FALSE, INFINITE, TRUE);
    } while (WAIT_IO_COMPLETION == waited);
    if (waited > WAIT_OBJECT_0 && waited < WAIT_OBJECT_0 + wait_object_count) {
      // A pending pipe was connected.
      DWORD index = waited - WAIT_OBJECT_0;
      PendingPipes::iterator connected_pipe = pending_iterators[index];
      ReadTermFromPipe(connected_pipe->pipe,
        &WhenReadFirstTermFromClientPipeComplete, &pipe_state, &stdstate);
      ResetEvent(connected_pipe->overlapped.hEvent);
      if (!CreatePendingPipe(&pipe_state, connected_pipe->overlapped.hEvent,
        false, &error))
      {
        CloseHandle(connected_pipe->overlapped.hEvent);
      }
      pipe_state.pending_pipes.erase(connected_pipe);
      if (pipe_state.pending_pipes.empty()) {
        return -1;
      }
    } else {
      // Either wait object 0 was signaled or there was some kind of error.
      break;  // We're done.
    }
  }
  return 0;
}


int APIENTRY _tWinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPTSTR    lpCmdLine,
                     int       nCmdShow)
{
  return OverlappedLoop();
}

void _MaybeReportPipeError(const OverlappedBuffer* ob, DWORD error_code,
  DWORD bytes_to_transfer, DWORD bytes_transferred, DWORD source_line) {
  if (error_code == 0 && bytes_to_transfer == bytes_transferred)
    return;
  DebugBreak();
}