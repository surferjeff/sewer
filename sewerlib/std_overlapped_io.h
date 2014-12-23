#pragma once

#include "overlapped_buffer.h"
#include "stdstate.h"

class PipeStdState : public StdState {
public:
  PipeStdState();
  virtual void ReadTermFromStdin(OverlappedBuffer::Callback callback,
    void* pvoid);
  virtual void WriteTermToStdout(ETERM* term);
  virtual void WriteFatalToStdout(ETERM* term);
  virtual void RelayTermToStdout(OverlappedBuffer* ob);
  virtual bool IsStdin(HANDLE pipe);
  virtual bool IsStdout(HANDLE pipe);
private:
  const HANDLE in;
  const HANDLE out;
};
