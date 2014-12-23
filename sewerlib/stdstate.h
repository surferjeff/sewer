#pragma once

#include "overlapped_buffer.h"
// Stores all the globalish state.

class FinalMessage;

struct StdState {
public:
  StdState();
  ~StdState();
  virtual void ReadTermFromStdin(OverlappedBuffer::Callback callback,
    void* pvoid) = 0;
  virtual void WriteTermToStdout(ETERM* term) = 0;
  virtual void WriteFatalToStdout(ETERM* term) = 0;
  virtual void RelayTermToStdout(OverlappedBuffer* ob) = 0;
  virtual bool IsStdin(HANDLE pipe) = 0;
  virtual bool IsStdout(HANDLE pipe) = 0;

  void SetNodeDownMessage(const tstring& pipe_name, const string& node_name);
  void SendNodeDownMessage();
public:
  const HANDLE done;
  volatile PVOID final_message;
};


