:: Modify the paths below to point at your 32-bit installation of erlang.
set ERL32_BIN=C:\Program Files (x86)\erl6.2\bin
set ERL32_INCLUDE=c:\Program Files (x86)\erl6.2\usr\include
set ERL32_LIB=c:\Program Files (x86)\erl6.2\usr\lib

:: Run from your visual studio's command prompt so devenv is in the path.
devenv sewer\sewer.sln /build Release

:: Compile the erlang files.
sewer\sewer.bat