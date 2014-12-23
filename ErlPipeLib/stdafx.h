// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once

#include "targetver.h"

#define WIN32_LEAN_AND_MEAN             // Exclude rarely-used stuff from Windows headers
#include <Windows.h>
#include <Winsock2.h>
#include <Lmcons.h>
#include <tchar.h>

// C RunTime Header Files
#include <malloc.h>

// Erlang stuff.
#define __WIN32__
#include "erl_interface.h"
#include "ei.h"

// C++.
#include <string>
#include <sstream>
#include <map>
#include <set>
#include <list>
#include <iostream>
#include <vector>

typedef std::basic_string<TCHAR> tstring;
typedef std::basic_stringstream<TCHAR> tstringstream;

using namespace std;

// TODO: reference additional headers your program requires here
