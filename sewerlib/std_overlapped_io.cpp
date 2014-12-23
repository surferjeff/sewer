#include "stdafx.h"

#include "std_overlapped_io.h"
#include "stdstate.h"

void WhenWriteTermToStdoutComplete(OverlappedBuffer* ob, bool succeeded) {
  if (!succeeded) {
    // Any failure to write to stdout is fatal.
    DWORD const error = GetLastError();
    SetEvent(ob->stdstate->done);
  }
}

void WhenWriteFatalToStdoutComplete(OverlappedBuffer* ob, bool succeeded) {
  SetEvent(ob->stdstate->done);
}


void PipeStdState::WriteTermToStdout(ETERM* term) {
  WriteTermToPipe(term, this->out, WhenWriteTermToStdoutComplete,
    NULL, this);
}

void PipeStdState::WriteFatalToStdout(ETERM* term) {
  WriteTermToPipe(term, this->out, WhenWriteFatalToStdoutComplete,
    NULL, this);
}

PipeStdState::PipeStdState()
  : in(GetStdHandle(STD_INPUT_HANDLE)),
    out(GetStdHandle(STD_OUTPUT_HANDLE))
{
}

void PipeStdState::ReadTermFromStdin(OverlappedBuffer::Callback callback,
  void* pvoid)
{
  ReadTermFromPipe(in, callback, pvoid, this);
}

void PipeStdState::RelayTermToStdout(OverlappedBuffer* ob) {
  RelayTermToPipe(ob, out);
}

bool PipeStdState::IsStdin(HANDLE pipe) {
  return pipe == in;
}

bool PipeStdState::IsStdout(HANDLE pipe) {
  return pipe == out;
}
