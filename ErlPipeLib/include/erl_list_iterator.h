#pragma once

// Transforms an ETERM* erlang list into a STL-like iterator.
class ErlListIterator : public std::iterator<std::forward_iterator_tag, ETERM*>
{
public:
  // Constructs the end iterator.
  ErlListIterator() : list_(empty_list()), term_(NULL) { }

  // Constructs an iterator beginning at the first term in the list.
  ErlListIterator(ETERM* list) : list_(NULL), term_(NULL) {
    Attach(list);
  }

  bool operator == (const ErlListIterator& that) const {
	return this->equal(that);
  }

  bool operator != (const ErlListIterator& that) const {
	return !this->equal(that);
  }

  ErlListIterator& operator ++() {
	increment();
	return *this;
  }

  ETERM* operator*() const {
	  return dereference();
  }

private:
  ETERM* dereference() const {
    return term_;
  }

  bool equal(const ErlListIterator& that) const {
    return this->list_ == that.list_;
  }

  void increment() {
    if (ERL_IS_EMPTY_LIST(list_))
      return;
    Attach(ERL_CONS_TAIL(list_));
  }

  void Attach(ETERM* list);
  static ETERM* empty_list();

  ETERM* list_;
  ETERM* term_;
};
