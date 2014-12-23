// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once

#include "targetver.h"

#define WIN32_LEAN_AND_MEAN             // Exclude rarely-used stuff from Windows headers
// Windows Header Files:
#include <windows.h>

// C RunTime Header Files
#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <tchar.h>

// C++
#include <vector>
#include <list>
#include <memory>
#include <string>
#include <sstream>
#include <map>
#include <set>

typedef std::basic_string<TCHAR> tstring;
typedef std::basic_stringstream<TCHAR> tstringstream;

// Erlang stuff.
#define __WIN32__
#include "erl_interface.h"
#include "ei.h"

// Boost.
#include "boost/shared_ptr.hpp"
#include "boost/iterator/iterator_facade.hpp"

// ATL
#include <atlbase.h>

using namespace std;

// TODO: reference additional headers your program requires here
