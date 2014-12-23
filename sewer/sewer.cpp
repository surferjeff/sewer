// sewer.cpp : Defines the entry point for the application.
//

#include "stdafx.h"
#include "sewer.h"

typedef int (*ExeMain)();

int APIENTRY _tWinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPTSTR    lpCmdLine,
                     int       nCmdShow)
{
  HMODULE sewer_lib = LoadLibrary(_T("sewerlib.dll"));
  if (!sewer_lib)
    return -1;
  ExeMain exe_main = reinterpret_cast<ExeMain>(
      GetProcAddress(sewer_lib, "ExeMain"));
  if (!exe_main)
    return -1;
  return exe_main();
}