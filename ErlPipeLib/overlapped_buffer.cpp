#include "stdafx.h"
#include "overlapped_buffer.h"

void OverlappedBuffer::DefaultCallback(OverlappedBuffer* ob, bool succeeded) {
  if (ob->READ == ob->operation)
    DeleteOverlappedBuffer(ob);
}


void* OverlappedBuffer::operator new(size_t struct_size, DWORD buffer_size) {
  size_t const size = struct_size + buffer_size; 
  void* const mem = ::operator new(size);
  memset(mem, 0, struct_size);
  return mem;
}

void OverlappedBuffer::operator delete(void* buffer, DWORD buffer_size) {
  ::delete(buffer);
}

OverlappedBuffer::OverlappedBuffer(DWORD buffer_size, StdState* astdstate)
	: size(buffer_size), callback(&OverlappedBuffer::DefaultCallback),
	stdstate(astdstate)
{
  overlapped.hEvent = this;
}

OverlappedBuffer* NewOverlappedBuffer(DWORD size, StdState* stdstate) {
  return new(size) OverlappedBuffer(size, stdstate);
}

void DeleteOverlappedBuffer(OverlappedBuffer* buffer) {
  ::delete buffer;
}

void _MaybeReportPipeError(const OverlappedBuffer* ob, DWORD error_code,
  DWORD bytes_to_transfer, DWORD bytes_transferred, DWORD source_line);

#define MaybeReportPipeError(ob, error_code, bytes_to_transfer, bytes_transferred) \
  _MaybeReportPipeError(ob, error_code, bytes_to_transfer, bytes_transferred, __LINE__)

VOID WINAPI WhenReadTermFromPipeComplete(DWORD error_code,
  DWORD bytes_read, LPOVERLAPPED overlapped)
{
  OverlappedBuffer* const ob =
    reinterpret_cast<OverlappedBuffer*>(overlapped->hEvent);
  MaybeReportPipeError(ob, error_code, ob->size, bytes_read);
  ob->callback(ob, bytes_read == ob->size);
}

VOID WINAPI WhenReadTermLengthFromPipeComplete(DWORD error_code,
  DWORD bytes_read, LPOVERLAPPED overlapped)
{
  OverlappedBuffer* new_ob = NULL;
  {  // Scope lifetime of ob before we delete it.
    OverlappedBuffer* const ob =
      reinterpret_cast<OverlappedBuffer*>(overlapped->hEvent);
    if (bytes_read < ob->size) {
      MaybeReportPipeError(ob, error_code, ob->size, bytes_read);
      ob->callback(ob, false);
      return;
    }
    DWORD const term_len = ntohl(ob->dword);
    // Read the term.
    new_ob = NewOverlappedBuffer(term_len, ob->stdstate);
    new_ob->operation = ob->READ;
    new_ob->pipe = ob->pipe;
    new_ob->callback = ob->callback;
    new_ob->pvoid = ob->pvoid;
    DeleteOverlappedBuffer(ob);
  }
  const BOOL read_succeeded =
    ReadFileEx(new_ob->pipe, new_ob->buffer, new_ob->size, &new_ob->overlapped,
    &WhenReadTermFromPipeComplete);
  if (read_succeeded) {
    // IO is pending, everything is good.
  } else {
    MaybeReportPipeError(new_ob, GetLastError(), new_ob->size, 0);
    new_ob->callback(new_ob, false);
  }
}

void ReadTermFromPipe(HANDLE pipe, OverlappedBuffer::Callback callback,
  void* pvoid, StdState* stdstate)
{
  OverlappedBuffer* const ob = NewOverlappedBuffer(sizeof(DWORD), stdstate);
  ob->pipe = pipe;
  if (callback) {
    ob->callback = callback;
  }
  ob->pvoid = pvoid;
  ob->operation = ob->READ;
  const BOOL read_succeeded =
    ReadFileEx(pipe, ob->buffer, ob->size, &ob->overlapped,
    &WhenReadTermLengthFromPipeComplete);
  if (read_succeeded) {
    // IO is pending, everything is good.
  } else {
    MaybeReportPipeError(ob, GetLastError(), ob->size, 0);
    ob->callback(ob, false);
  }
}

VOID WINAPI WhenWriteTermToPipeComplete(DWORD error_code,
  DWORD bytes_written, LPOVERLAPPED overlapped)
{
  OverlappedBuffer* const ob =
    reinterpret_cast<OverlappedBuffer*>(overlapped->hEvent);
  const bool succeeded = bytes_written == ob->size;
  MaybeReportPipeError(ob, error_code, ob->size, bytes_written);
  ob->callback(ob, succeeded);
  DeleteOverlappedBuffer(ob);
}

void WriteTermToPipe(ETERM* term, HANDLE pipe,
  OverlappedBuffer::Callback callback, void* pvoid, StdState* stdstate)
{
  const int term_len = erl_term_len(term);
  OverlappedBuffer* ob = NewOverlappedBuffer(term_len + sizeof(DWORD), stdstate);
  if (callback) {
    ob->callback = callback;
  }
  ob->dword = htonl(term_len);
  ob->pipe = pipe;
  ob->pvoid = pvoid;
  erl_encode(term, ob->buffer + sizeof(ob->dword));
  ob->operation = ob->WRITE;
  const BOOL write_succeeded = WriteFileEx(ob->pipe, ob->buffer,
    ob->size, &ob->overlapped, &WhenWriteTermToPipeComplete);
  if (!write_succeeded) {
    MaybeReportPipeError(ob, GetLastError(), ob->size, 0);
    ob->callback(ob, false);
    DeleteOverlappedBuffer(ob);
  }
}

void EncodeTerm(ETERM* term, vector<unsigned char>* buffer) {
  const int term_len = erl_term_len(term);
  buffer->resize(term_len + sizeof(DWORD));
  const DWORD size = htonl(term_len);
  memcpy(&(*buffer)[0], &size, sizeof(size));
  erl_encode(term, &(*buffer)[sizeof(size)]);
}

bool TermCallNamedPipe(const TCHAR* pipe_name, ETERM* term, void* response,
  DWORD response_len, DWORD* bytes_read, DWORD timeout) {
  vector<unsigned char> buffer;
  EncodeTerm(term, &buffer);
  BOOL called = CallNamedPipe(pipe_name, &buffer[0],
	  static_cast<DWORD>(buffer.size()), response,
      response_len, bytes_read, timeout);
  return called != 0;
}

void WriteRawToPipe(void* term, DWORD term_len, HANDLE pipe,
  OverlappedBuffer::Callback callback, void* pvoid, StdState* stdstate)
{
  OverlappedBuffer* ob = NewOverlappedBuffer(term_len, stdstate);
  if (callback) {
    ob->callback = callback;
  }
  ob->pipe = pipe;
  ob->pvoid = pvoid;
  memcpy(ob->buffer, term, term_len);
  ob->operation = ob->WRITE;
  const BOOL write_succeeded = WriteFileEx(ob->pipe, ob->buffer,
    ob->size, &ob->overlapped, &WhenWriteTermToPipeComplete);
  if (!write_succeeded) {
    MaybeReportPipeError(ob, GetLastError(), ob->size, 0);
    ob->callback(ob, false);
    DeleteOverlappedBuffer(ob);
  }
}

VOID WINAPI WhenRelayTermToPipeComplete(DWORD error_code,
  DWORD bytes_written, LPOVERLAPPED overlapped)
{
  OverlappedBuffer* const ob =
    reinterpret_cast<OverlappedBuffer*>(overlapped->hEvent);
  const bool succeeded = bytes_written == ob->size + sizeof(DWORD);
  MaybeReportPipeError(ob, error_code, ob->size + sizeof(DWORD), bytes_written);
  ob->callback(ob, succeeded);
  DeleteOverlappedBuffer(ob);
}

OVERLAPPED kZeroOverlapped = { 0 };

void RelayTermToPipe(OverlappedBuffer* ob, HANDLE pipe) {
  ob->overlapped = kZeroOverlapped;
  ob->overlapped.hEvent = ob;
  ob->operation = ob->WRITE;
  ob->callback = &ob->DefaultCallback;
  ob->pvoid = NULL;
  ob->pipe = pipe;
  DWORD* term_len = ((&ob->dword) - 1);
  *term_len = htonl(ob->size);
  const BOOL write_succeeded = WriteFileEx(ob->pipe, term_len,
    ob->size + sizeof(DWORD), &ob->overlapped, &WhenRelayTermToPipeComplete);
  if (!write_succeeded) {
    MaybeReportPipeError(ob, GetLastError(), ob->size, 0);
    ob->callback(ob, false);
    DeleteOverlappedBuffer(ob);
  }  
}
