#pragma once

struct StdState;

// Stores all the state about a pending overlapped operation.
struct OverlappedBuffer {
  // READ callbacks MUST DELETE the ob.
  // WRITE callbacks MUST NOT DELETE the ob.
  typedef void (*Callback)(OverlappedBuffer* ob, bool succeeded);
  OVERLAPPED overlapped;  // The Win32 Overlapped struct.
  DWORD size;
  StdState* stdstate;
  HANDLE pipe;
  Callback callback;
  void * pvoid;
  enum Operation { READ, WRITE } operation;
  // The length of the term in network byte order.  Never used directly,
  // but must come immediately before buffer.
  DWORD __term_len__;  // Is this still necessary?
  union {
    DWORD dword;
    unsigned char buffer[1];  // Actually, has size size.
  };
  static void DefaultCallback(OverlappedBuffer* ob, bool succeeded);
  void* operator new(size_t struct_size, DWORD buffer_size);
  void operator delete(void* buffer, DWORD buffer_size);
  OverlappedBuffer(DWORD buffer_size, StdState* stdstate);
};

OverlappedBuffer* NewOverlappedBuffer(DWORD size, StdState* stdstate);

void DeleteOverlappedBuffer(OverlappedBuffer* buffer);

struct ScopedOverlappedBuffer {
  ScopedOverlappedBuffer(OverlappedBuffer* ob) : ob_(ob) { }
  ~ScopedOverlappedBuffer() { DeleteOverlappedBuffer(ob_); }
  OverlappedBuffer* const ob_;
};

void ReadTermFromPipe(HANDLE pipe, OverlappedBuffer::Callback callback,
  void* pvoid, StdState* stdstate);

void WriteTermToPipe(ETERM* term, HANDLE pipe,
  OverlappedBuffer::Callback callback, void* pvoid, StdState* stdstate);

void WriteRawToPipe(void* term, DWORD term_len, HANDLE pipe,
  OverlappedBuffer::Callback callback, void* pvoid, StdState* stdstate);

void RelayTermToPipe(OverlappedBuffer* ob, HANDLE pipe);

bool TermCallNamedPipe(const TCHAR* pipe_name, ETERM* term, void* response,
  DWORD response_len, DWORD* bytes_read, DWORD timeout);

void EncodeTerm(ETERM* term, vector<unsigned char>* buffer);