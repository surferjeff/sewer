#pragma once

// The public interface for this pipe lib.
enum {
  PIPE_BUFSIZE = 64 * 1024,
  PIPE_TIMEOUT = 100,  // milliseconds.
  PIPE_INSTANCES = 5,
};

// A pending pipe is a Win32 Pipe with a pending overlapped operation.
struct PendingPipe {
  HANDLE pipe;
  OVERLAPPED overlapped;
};

// Better than a constructor.
extern const PendingPipe kZeroPendingPipe;

typedef std::list<PendingPipe> PendingPipes;

// Gets the current user name.
std::basic_string<TCHAR> GetUserName();

// A class that cleans up ETERMs whet they pass out of scope.
class ScopedTerm {
public:
  ScopedTerm(ETERM* term) : term_(term) { }
  ~ScopedTerm() {
    erl_free_term(term_);
  }
  operator ETERM*() const { return term_; }
  ETERM* const term_;
};

// erl_var_content returns the contents of the specified variable in an Erlang term.
// This class make it easy to all erl_var_content and then erl_free_term when
// it passes out of scope.
class ErlVarContent : public ScopedTerm {
public:
  ErlVarContent(ETERM* pattern, const char* name)
    : ScopedTerm(erl_var_content(pattern, name)) { }
};

// An erlang node name looks like mynode@localhost.  It's split on the @.
// This struct makes it convenient to store a split node name.
struct NodeNames {
  std::string host;
  std::string node;
};

NodeNames SplitNodeName(const char* node_name);
NodeNames SplitNodeName(const ETERM* node_name_term);

// Erlang nodes can be hidden or visible.  Visible nodes are automatically fully
// connected to all other visible nodes.
enum NodeVisibility {
  HIDDEN_NODE,
  VISIBLE_NODE,
};

inline const char* ToCStr(NodeVisibility visibility) {
  return HIDDEN_NODE == visibility ? "hidden" : "visible";
}

// Represents a pipe connected to a client.
struct LivePipe {
  HANDLE handle;
  NodeVisibility visibility;
  ETERM* pid;  // The pid of the process on the other end of the pipe.
};

// Maps node name to pipe handle.
typedef std::map<std::string, LivePipe> LivePipes;

// When visible nodes connect, the automatically connect to all the others'
// connections.  We represent this as connection pools.
typedef std::set<std::string> ConnectionPool;
typedef std::map<std::string, ConnectionPool*> ConnectionPools;
typedef std::set<std::pair<std::string, std::string> > HiddenConnections;

// The state for *all*the*pipes on this node.
struct PipeState {
  std::string node_name;
  NodeVisibility node_visibility;
  // We create a server pipe with multiple instances, but the same name.
  std::basic_string<TCHAR> pipe_name;
  // Pipes that are waiting for client connections.
  PendingPipes pending_pipes;
  // Pipes connected to clients.
  LivePipes live_pipes;
  // Connection pools.
  ConnectionPools connection_pools;
  HiddenConnections hidden_connections;      
};
 
// Creates a pipe that is listening for an incoming connection.
// hevent is set when something connects to the pipe.
bool CreatePendingPipe(const tstring& pipe_name, HANDLE hevent,
  bool first_instance, std::string* error_string, PendingPipe* pending);
// Calls CreatePendingPipe above and updates pipe_state.
bool CreatePendingPipe(PipeState* pipe_state, HANDLE hevent,
  bool first_instance, std::string* error_string);

ConnectionPool* FindConnectionPool(PipeState* pipe_state,
  const std::string& node_name);

std::basic_string<TCHAR> GetPipeBrokerPipeName();

// Closes the pipe and updates pipe_state.
void ClosePipe(PipeState* pipe_state, HANDLE pipe);

// Appends the contents of the atom to the string.
bool AppendAtom(string* s, ETERM* atom);

// Checks if the size of the binary == sizeof(t), and copies contents
// of t to the binary if true.
template <typename T>
bool CopyBinary(T* t, ETERM* binary) {
  if (NULL != binary && ERL_IS_BINARY(binary)
      && ERL_BIN_SIZE(binary) == sizeof(t))
  {
    memcpy(t, ERL_BIN_PTR(binary), sizeof(t));
    return true;
  }
  return false;
}

// String conversion functions.
void Append(std::string* target, const wchar_t* ws, size_t ws_len);

void Append(std::string* target, const std::wstring& ws);

std::ostream& operator << (std::ostream& stream, const std::wstring& ws);

void Append(std::wstring* target, const char* s, size_t len);

inline void Append(std::wstring* target, const std::string& s);

inline void Append(std::wstring* target, const std::wstring& ws) {
  target->append(ws);
}
