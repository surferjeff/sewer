#include "stdafx.h"

#include "stdstate.h"
#include "ErlPipeLib.h"

enum {
  NODEDOWN_TIMEOUT = 3000,
};

StdState::StdState()
  : done(CreateEvent(NULL, TRUE, FALSE, NULL)), final_message(NULL)
{
}

StdState::~StdState() {
  CloseHandle(done);
}

class FinalMessage {
public:
  FinalMessage(const tstring& pipe_name, const string& node_name)
    : pipe_name_(pipe_name)
  {
    ScopedTerm final_message(erl_format("{nodedown, ~a}", node_name.c_str()));
    EncodeTerm(final_message, &buffer_);
  }

  void Send() {
    DWORD bytes_read = 0;
    DWORD called = CallNamedPipe(pipe_name_.c_str(), &buffer_[0],
      static_cast<DWORD>(buffer_.size()), NULL, 0, &bytes_read, NODEDOWN_TIMEOUT);
    DWORD last_error = GetLastError();
  }
private:
  const tstring pipe_name_;
  vector<unsigned char> buffer_;
};

void StdState::SetNodeDownMessage(const tstring& pipe_name, 
  const string& node_name)
{
  FinalMessage* new_message = new FinalMessage(pipe_name, node_name);
  void* const old_message = InterlockedCompareExchangePointer(&final_message, 
    new_message, NULL);
  if (old_message == NULL) {
    // Installed our new message.
  } else {
    delete new_message;
  }
}

void StdState::SendNodeDownMessage() {
  void* const pvoid =
    InterlockedExchangePointer(const_cast<void**>(&final_message), NULL);
  if (pvoid) {
    FinalMessage* const message = reinterpret_cast<FinalMessage*>(pvoid);
    message->Send();
    delete message;
  }
}

