// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once
#define _CRT_SECURE_NO_WARNINGS

#include "targetver.h"

#define WIN32_LEAN_AND_MEAN             // Exclude rarely-used stuff from Windows headers
// Windows Header Files:
#include <windows.h>
#include <Lmcons.h>

// C RunTime Header Files
#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <tchar.h>
#include <io.h>
#include <assert.h>

// atl
#include <atlbase.h>
#include <atltrace.h>

// Erlang stuff.
#define __WIN32__
#include "erl_interface.h"
#include "ei.h"
#include "erl_driver.h"

// C++ headers
#include <vector>
#include <list>
#include <memory>
#include <string>
#include <sstream>
#include <map>
#include <set>
#include <algorithm>

typedef std::basic_string<TCHAR> tstring;
typedef std::basic_stringstream<TCHAR> tstringstream;

// My stuff
using namespace std;

#define XSTRINGIFY(x) #x
#define STRINGIFY(x) XSTRINGIFY(x)

// TODO: reference additional headers your program requires here
