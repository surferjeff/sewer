#include "stdafx.h"
#include "strconv.h"

void Append(std::string* target, const wchar_t* ws, size_t ws_len) {
  int const size = WideCharToMultiByte(CP_UTF8, 0, ws,
	static_cast<int>(ws_len), 0, 0, 0, 0);
  std::vector<char> bytes(size);
  WideCharToMultiByte(CP_UTF8, 0, ws, static_cast<int>(ws_len),
	  &bytes[0], static_cast<int>(bytes.size()), 0, 0);
  target->append(bytes.begin(), bytes.end());
}

void Append(std::string* target, const std::wstring& ws) {
  Append(target, ws.data(), ws.size());
}

std::ostream& operator << (std::ostream& stream, const std::wstring& ws) {
  std::string s;
  Append(&s, ws);
  return stream << s;
}

void Append(std::wstring* target, const char* s, size_t len) {
  int const size = MultiByteToWideChar(CP_UTF8, 0, s,
	  static_cast<int>(len), 0, 0);
  std::vector<wchar_t> bytes(size);
  MultiByteToWideChar(CP_UTF8, 0, s, static_cast<int>(len), &bytes[0],
	  static_cast<int>(bytes.size()));
  target->append(bytes.begin(), bytes.end());
}

inline void Append(std::wstring* target, const std::string& s) {
  Append(target, s.data(), s.size());
}

