#include "stdafx.h"
#include "erl_list_iterator.h"

void ErlListIterator::Attach(ETERM* list) {
  if (ERL_IS_LIST(list)) {
    if (ERL_IS_EMPTY_LIST(list)) {
      list_ = empty_list();
      term_ = NULL;
    } else {
      list_ = list;
      term_ = ERL_CONS_HEAD(list);
    }
  } else {
    // Malformed list.  This must be the end.
    term_ = NULL;
    list_ = empty_list();
  }
}

ETERM* ErlListIterator::empty_list() {
  static ETERM* const empty_list_ = erl_mk_empty_list();
  return empty_list_;
}
