// port_driver.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include "stdstate.h"
#include "sewerlib.h"

class SewerState : public StdState {
public:
  SewerState(ErlDrvPort aport)
    : port(aport), stdin_callback(0), stdin_callback_pvoid(0),
    io_thread(INVALID_HANDLE_VALUE),
    stdin_ready(CreateEvent(NULL, TRUE, FALSE, NULL))
  {
  }

  ~SewerState() {
    CloseHandle(io_thread);
    CloseHandle(stdin_ready);
  }

  void StartIOThread() {
    DWORD thread_id = 0;
    io_thread = CreateThread(NULL, 0, &SewerState::IOThreadMain, this, 0,
      &thread_id);
    WaitForSingleObject(stdin_ready, INFINITE);
  }

  void Stop() {
    SetEvent(this->done);
    WaitForSingleObject(io_thread, INFINITE);
  }

  void PretendFromStdin(char* buf, size_t bufflen) {
    OverlappedBuffer* ob = NewOverlappedBuffer(static_cast<DWORD>(bufflen), this);
    memcpy(ob->buffer, buf, bufflen);
    ob->operation = ob->READ;
    ob->callback = this->stdin_callback;
    ob->pvoid = this->stdin_callback_pvoid;
    DWORD queued = QueueUserAPC(&SewerState::CallStdinCallback, this->io_thread,
      reinterpret_cast<ULONG_PTR>(ob));
  }

  virtual void ReadTermFromStdin(OverlappedBuffer::Callback callback,
    void* pvoid)
  {
    const bool first_callback = NULL == stdin_callback;
    stdin_callback = callback;
    stdin_callback_pvoid = pvoid;
    if (first_callback)
      SetEvent(stdin_ready);
  }

  virtual void WriteTermToStdout(ETERM* term);
  virtual void WriteFatalToStdout(ETERM* term);
  virtual void RelayTermToStdout(OverlappedBuffer* ob);

  virtual bool IsStdin(HANDLE pipe) {
    return false;
  }
  virtual bool IsStdout(HANDLE pipe) {
    return false;
  }

private:
  static DWORD WINAPI IOThreadMain(LPVOID pvoid) {
    StdState* const stdstate = reinterpret_cast<StdState*>(pvoid);
    return MainLoop(stdstate);
  }

  static VOID CALLBACK CallStdinCallback(ULONG_PTR param) {
    OverlappedBuffer* ob = reinterpret_cast<OverlappedBuffer*>(param);
    ob->callback(ob, true);
  }

  const ErlDrvPort port;
  OverlappedBuffer::Callback stdin_callback;
  void* stdin_callback_pvoid;
  HANDLE io_thread;
  HANDLE stdin_ready;
};


static ErlDrvData sewerlib_start(ErlDrvPort port, char *buff);
static void sewerlib_stop(ErlDrvData handle);
static void sewerlib_output(ErlDrvData handle, char *buff, 
			       ErlDrvSizeT bufflen);
static int sewerlib_init();
static void sewerlib_finish();

ErlDrvEntry sewerlib_driver_entry = {
    sewerlib_init,	        /* F_PTR init, called when driver is loaded */
    sewerlib_start,		/* L_PTR start, called when port is opened */
    sewerlib_stop,		/* F_PTR stop, called when port is closed */
    sewerlib_output,		/* F_PTR output, called when erlang has sent */
    NULL,			/* F_PTR ready_input, called when input descriptor ready */
    NULL,			/* F_PTR ready_output, called when output descriptor ready */
    "sewerlib",		        /* char *driver_name, the argument to open_port */
    sewerlib_finish,            /* F_PTR finish, called when unloaded */
    NULL,                       /* void *handle, Reserved by VM */
    NULL,			/* F_PTR control, port_command callback */
    NULL,			/* F_PTR timeout, reserved */
    NULL,			/* F_PTR outputv, reserved */
    NULL,                       /* F_PTR ready_async, only for async drivers */
    NULL,                       /* F_PTR flush, called when port is about 
				   to be closed, but there is data in driver 
				   queue */
    NULL,                       /* F_PTR call, much like control, sync call
				   to driver */
    NULL,                       /* F_PTR event, called when an event selected 
				   by driver_event() occurs. */
    ERL_DRV_EXTENDED_MARKER,    /* int extended marker, Should always be 
				   set to indicate driver versioning */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be 
				       set to this value */
    ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be 
				       set to this value */
    ERL_DRV_FLAG_SOFT_BUSY,                          /* int driver_flags, see documentation */
    NULL,                       /* void *handle2, reserved for VM use */
    NULL,                       /* F_PTR process_exit, called when a 
				   monitored process dies */
    NULL                        /* F_PTR stop_select, called to close an 
				   event object */
};

DRIVER_INIT(sewerlib) /* must match name in driver_entry */
{
    return &sewerlib_driver_entry;
}

namespace {
typedef vector<SewerState*> AllStates;
AllStates all_states;

void WhenProcessExits() {
  for (AllStates::iterator i = all_states.begin(), end = all_states.end();
    i != end; ++i)
  {
    (*i)->SendNodeDownMessage();
  }
}

}  // anonymous namespace.

int sewerlib_init() {
  atexit(&WhenProcessExits);
  return 0;
}

void sewerlib_finish() {
  return;
}

static ErlDrvData sewerlib_start(ErlDrvPort port, char *buff)
{
  SewerState* sewerstate = new SewerState(port);
  all_states.push_back(sewerstate);
  sewerstate->StartIOThread();
  return (ErlDrvData)sewerstate;
}

static void sewerlib_stop(ErlDrvData handle)
{
  SewerState* const sewerstate = (SewerState*)handle;
  sewerstate->Stop();
  sewerstate->SendNodeDownMessage();
  all_states.erase(find(all_states.begin(), all_states.end(), sewerstate));
  delete sewerstate;
}

static void sewerlib_output(ErlDrvData handle, char *buff, 
			       ErlDrvSizeT bufflen)
{
  SewerState* const sewerstate = (SewerState*)handle;
  sewerstate->PretendFromStdin(buff, bufflen);
}


void SewerState::WriteTermToStdout(ETERM* term) {
  vector<unsigned char> buffer(erl_term_len(term));
  erl_encode(term, &buffer[0]);
  driver_output(this->port, reinterpret_cast<char*>(&buffer[0]), buffer.size());
}

void SewerState::WriteFatalToStdout(ETERM* term) {
  vector<unsigned char> buffer(erl_term_len(term));
  erl_encode(term, &buffer[0]);
  driver_output(this->port, reinterpret_cast<char*>(&buffer[0]), buffer.size());
}

void SewerState::RelayTermToStdout(OverlappedBuffer* ob) {
  driver_output(this->port, reinterpret_cast<char*>(ob->buffer), ob->size);
  DeleteOverlappedBuffer(ob);
}
