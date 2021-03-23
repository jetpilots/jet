

#include <cstdio>
#include <cstring>
#include <cstdlib>

void print(const char* s) { printf("%s", s); }
void print(int i) { printf("%d", i); }

template <typename T>
T max(T v1, T v2) {
    return v1 > v2 ? v1 : v2;
}
template <typename T>
T min(T v1, T v2) {
    return v1 < v2 ? v1 : v2;
}

template <class T>
class Array {
public:
    class iterator {
    public:
        iterator(T* ptr)
            : ptr(ptr) { }
        iterator operator++() {
            ++ptr;
            return *this;
        }
        bool operator!=(const iterator& other) const {
            return ptr != other.ptr;
        }
        const T& operator*() const { return *ptr; }

    private:
        T* ptr;
    };

private:
    unsigned len, cap;
    T* ref;

    // TODO: for Jet it would be nice to have a compile-time constraint on n to
    // be greater than free+used. Unless this can be proven the code shouldn't
    // compile.
    void resizeto(size_t n) {
        cap = n; // TODO: set len = min(len,cap)
        ref = (T*)realloc(ref, n * sizeof(T));
    }

public:
    Array()
        : len(0)
        , cap(0) // start with 4 items on first realloc
        , ref(NULL) { }
    iterator begin() const { return iterator(ref); }
    iterator end() const { return iterator(ref + len); }
    ~Array() { free(ref); }
    Array<T>& push(T item) {
        if (len >= cap) resizeto(cap ? cap * 2 : 4);
        ref[len++] = item;
        return *this;
    }
    Array<T>& pushn(size_t n, T items[]) {
        size_t newlen = len + n, i = 0;
        if (newlen >= cap) resizeto(cap ? cap * 2 : 4); // roundup32(newlen)
        while (len < newlen) ref[len++] = items[i++];
        return *this;
    }
    Array<T>& shift(T item) {
        if (len >= cap) resizeto(cap * 2);
        memmove(ref + 1, ref, len * sizeof(T));
        ref[0] = item;
        len++;
        return *this;
    }
    Array<T>& insert(size_t index, T item) {
        if (index > cap) index = cap; // resizeto(cap * 2);
        if (len >= cap) resizeto(cap * 2);
        memmove(
            ref + index + 1, ref + index, max(0UL, len - index) * sizeof(T));
        ref[index] = item;
        len++;
        return *this;
    }
    // Array<T>& shiftn(size_t n, T items[]) {
    //     if (len + n >= cap) resizeto(roundup32(len));
    //     memmove(ref + n  , ref, used * sizeof(T));
    //     ref[0] = item;
    // }

    T& pop() { return ref[len--]; }
    T& top() { return ref[len - 1]; }
    bool empty() { return !!len; }
    size_t count() { return len; }
    void clear() {
        cap += len;
        len = 0;
    }
    void print() {
        ::print("[");
        for (auto c = begin(); c != end(); ++c) {
            if (c != begin()) ::print(", ");
            ::print(*c);
        }
        ::print("]\n");
    }
};

template <class T>
class List {

    class iterator {
        T _item;
        iterator* _next;

    public:
        iterator(T item)
            : _item(item)
            , _next(NULL) { }
        iterator(T item, iterator* _next)
            : _item(item)
            , _next(_next) { }
        // TODO: make this private or somehow only accessible to List<T>
        iterator** pnext() { return &_next; }
        iterator operator++() { return (*this = *_next); }
        bool operator!=(const iterator& other) const {
            // caitemsul with short-circuiting logical ops
            // printf("\n*** %d %p != %d %p -> %d %d %d\n",  _item,
            //      _next, other._item, other._next,
            //      _item == other._item,  _next == other._next, ret);
            return !(_item == other._item || _next == other._next);
        }
        const T& operator*() const { return _item; }
    };

private:
    // TODO: make _end static
    iterator _end, *_first, **_lastp;
    int _count;

public:
    List()
        : _end(iterator(0))
        , _first(&_end)
        , _lastp(&_first)
        , _count(0) { }
    int count() const { return _count; }
    List<T>& pushn(size_t n, T items[]) {
        for (size_t i = 0; i < n; i++) push(items[i]);
        return *this;
    }
    List<T>& shiftn(size_t n, T items[]) {
        while (n-- > 0) shift(items[n]);
        return *this;
    }
    List<T>& push(T item) {
        *_lastp = new iterator(item, &_end);
        _lastp = (*_lastp)->pnext();
        _count++;
        return *this;
    }
    List<T>& shift(T item) {
        _first = new iterator(item, _first);
        _count++;
        return *this;
    }
    List<T>& insert(size_t index, T item) {
        if (index > _count) index = _count;
        iterator** itp = &_first;
        for (int i = index; i > 0; i--) itp = (*itp)->pnext();
        *itp = new iterator(item, *itp);
        if (index == count()) _lastp = (*itp)->pnext();
        _count++;
        return *this;
    }
    T& remove(size_t index) { }
    T& pop() { }
    iterator begin() const { return *_first; }
    iterator end() const { return _end; }
    void print() {
        ::print("[");
        for (auto c = begin(); c != end(); ++c) {
            if (c != begin()) ::print(" -> ");
            ::print(*c);
        }
        ::print("]\n");
    }
    void clear() {
        _count = 0;
        // TODO
        // for (iterator* i = _first; i; i = i->_next) delete i;
    }
};

// template <>
// void List<const char*>::print() {
//     ::print("[");
//     for (auto c = begin(); c != end(); ++c) {
//         if (c !=  begin()) ::print(", ");
//         ::print("\"");
//         ::print(*c);
//         ::print("\"");
//     }
//     ::print("]");
// }

class StringList : public Array<const char*> {
public:
    void print() {
        ::print("[");
        for (auto c = begin(); c != end(); ++c) {
            if (c != begin()) ::print(", ");
            ::print("\"");
            ::print(*c);
            ::print("\"");
        }
        ::print("]\n");
    }

    // StringList& push(const char* item) {
    //     return REINTERPRET;
    // }

    // TODO: all string allocations should leave 1 char before and 3 chars after
    // as extra chars. The 3 chars after should be NULL. 1 char before and 1
    // after serve as places to put double quotes and give an in-place quote()
    // function. 2 NULLs serve as terminators with the possibility to always do
    // 1-char lookahead while scanning the string. The final alloc size should
    // anyway be a multiple of 8 or 16 B.
    const char* join() {
        int i = 0, size = 0;
        size_t* sizes = new size_t[count()];
        for (auto c : *this) size += (sizes[i++] = strlen(c));
        char *buf = new char[size + 1], *pos = buf;
        i = 0;
        for (auto c : *this) memcpy(pos, c, sizes[i]), pos += sizes[i++];
        *pos = 0;
        delete[] sizes;
        return buf;
    }
    void snap() {
        const char* joined = join();
        clear();
        shift(joined);
    }
};

#define CARRAY(T, ...)                                                         \
    (T[]) { __VA_ARGS__ }
int foo() {
    Array<int> ls;
    ls.push(3).push(32).push(67).shift(1);
    ls.print();
    auto sted = StringList();
    sted.pushn(3, CARRAY(const char*, "hello", "there", "wowee"))
        .shift("hey")
        .insert(4, "bouk")
        .push("adsa");
    sted.print(); // YOU CAN'T TAG ON print() to the above chain!!!
    print(sted.join());
    return 0;
}

int main() { foo(); }
