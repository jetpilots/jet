

#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <cassert>

struct Collection { };

bool isalpha(char c) {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z');
}
typedef const char* cchars;
typedef const char* SmallString;

void print(cchars s) { printf("%s", s); }
void print(int i) { printf("%d", i); }

template <typename T>
T max(T v1, T v2) {
    return v1 > v2 ? v1 : v2;
}
template <typename T>
T min(T v1, T v2) {
    return v1 < v2 ? v1 : v2;
}

// template <class T>
// struct Reversed {
//     T& base;
//     struct iterator {
//         typename T::iterator& iter;
//         bool operator!=(const iterator& other) const {
//             return iter != other.iter;
//         }
//     };
//     Reversed(T& ref)
//         : base(ref) { }
//     iterator begin() const { return iterator(base.last()); }
//     iterator end() const { return iterator(base.second()); }
//     iterator operator++() { return --ptr, *this; }
// };

template <class T>
class Array : public Collection {
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
    iterator second() const { return iterator(ref + 1); }
    iterator last() const { return iterator(ref + len - 1); }
    iterator end() const { return iterator(ref + len); }
    ~Array() { free(ref); }
    Array<T>& push(T _item) {
        if (len >= cap) resizeto(cap ? cap * 2 : 4);
        ref[len++] = _item;
        return *this;
    }
    // Reversed<Array<T>> reverse() { return Reversed(*this); }
    Array<T>& pushn(size_t n, T items[]) {
        size_t newlen = len + n, i = 0;
        if (newlen >= cap) resizeto(cap ? cap * 2 : 4); // roundup32(newlen)
        while (len < newlen) ref[len++] = items[i++];
        return *this;
    }
    Array<T>& shift(T _item) {
        if (len >= cap) resizeto(cap * 2);
        memmove(ref + 1, ref, len * sizeof(T));
        ref[0] = _item;
        len++;
        return *this;
    }
    Array<T>& insert(size_t index, T _item) {
        if (index > cap) index = cap; // resizeto(cap * 2);
        if (len >= cap) resizeto(cap * 2);
        memmove(
            ref + index + 1, ref + index, max(0UL, len - index) * sizeof(T));
        ref[index] = _item;
        len++;
        return *this;
    }
    // Array<T>& shiftn(size_t n, T items[]) {
    //     if (len + n >= cap) resizeto(roundup32(len));
    //     memmove(ref + n  , ref, used * sizeof(T));
    //     ref[0] = _item;
    // }
    T& operator[](size_t idx) { return ref[idx]; }
    T& pop() { return ref[len--]; }
    T& top() { return ref[len - 1]; }
    bool empty() { return !!len; }
    size_t count() { return len; }
    void reset() {
        cap += len;
        len = 0;
    }
    void clear() {
        if (cap) free(ref);
        reset();
    }
    void print(FILE* file = stdout) {
        ::print("[", file);
        for (auto c = begin(); c != end(); ++c) {
            if (c != begin()) ::print(", ", file);
            ::print(*c, file);
        }
        ::print("]\n", file);
    }
};

template <class T>
class TagPtr {
private:
    uint64_t bits;
    TagPtr(T* ptr, short tag)
        : bits(((uint64_t)ptr) & 0x0000ffffffffffffUL | ((uint64_t)tag) << 48) {
    }
    TagPtr(T* ptr)
        : bits(((uint64_t)ptr) & 0x0000ffffffffffffUL) { }

public:
    int tag() { return bits >> 48; }
    T* operator*() { return (T*)(bits & 0x0000ffffffffffff); }
};

template <class T>
class List1 : public Collection {
    T _item;
    List1<T>* _next;

public:
    List1(T _item)
        : _item(_item)
        , _next(nullptr) { }
    List1(T _item, List1<T>* _next)
        : _item(_item)
        , _next(_next) { }
    T& operator[](int i) {
        List1<T>* iter = this;
        while (i-- > 0) iter = iter->next;
        return iter->_item;
    }
    int count() {
        int i = 0;
        List1<T>* last = this;
        while (last->_next) ++i, last = last->_next;
        return i;
    }
    List1<T>& insert(T newItem) {
        _next = new List1<T>(newItem, _next);
        return _next;
    }
    T unshift() {
        List1<T> __this = *this;
        ++this;
        delete __this._next;
        return __this._item;
    }
    T pop() {
        List1<T>* last = this;
        while (last->_next) last = last->_next;
        T __item = last->_item;
        delete last;
        return __item;
    }
    List1<T>& clear() {
        List1<T>*p = this, *n;
        while ((n = p->_next)) {
            delete p;
            p = n;
        }
    }
    List1<T>& shift(T newItem) {
        _next = new List1<T>(_item, _next), _item = newItem;
        return this;
    }
    List1<T>& push(T newItem) {
        List1<T>* last = this;
        while (last->_next) last = last->_next;
        last->insert(newItem);
    }
    List1<T> begin() { return *this; }
    List1<T> next() { return _next ? *_next : end(); }
    List1<T> end() { return List1<T>(0, nullptr); }
    bool operator!=(List1<T> other) { return _next != other._next; }
    void operator++() { *this = next(); }
    T& operator*() { return _item; }
};
int foomk() {
    int k = 9;
    List1<int&> ls(k);
    ls.push(k).push(k).push(k).shift(k);
    // ls.print();
    for (auto& x : ls) { }
    return 0;
}
template <class T>
class List : public Collection {

    class iterator {
        T _item;
        iterator* _next;

    public:
        iterator(T _item)
            : _item(_item)
            , _next(NULL) { }
        iterator(T _item, iterator* _next)
            : _item(_item)
            , _next(_next) { }
        // TODO: make this private or somehow only accessible to List<T>
        iterator** pnext() { return &_next; }
        iterator operator++() { return (*this = *_next); }
        bool operator!=(const iterator& other) const {
            // caitemsul with short-circuiting logical ops
            // printf("\n*** %d %p != %d %p -> %d %d %d\n",  _item,
            //      _next, other._item, other._next,
            //      _item == other._item,  _next == other._next, ret);
            return !(_item == other._item or _next == other._next);
        }
        const T& operator*() const { return _item; }
    };

private:
    // TODO: make _end static
    iterator *_first, **_lastp; //, _end;
    int _count;

public:
    List()
        //  _end(iterator(0)) ,
        : _first(nullptr)
        , _lastp(&_first)
        , _count(0) { }
    int count() const { return _count; }
    /* // doesnt work when T is a & to sth
    List<T>& pushn(size_t n, T items[]) {
        for (size_t i = 0; i < n; i++) push(items[i]);
        return *this;
    }
    List<T>& shiftn(size_t n, T items[]) {
        while (n-- > 0) shift(items[n]);
        return *this;
    }
    */
    List<T>& push(T _item) {
        *_lastp = new iterator(_item);
        _lastp = (*_lastp)->pnext();
        _count++;
        return *this;
    }
    List<T>& shift(T _item) {
        _first = new iterator(_item, _first);
        _count++;
        return *this;
    }
    List<T>& insert(size_t index, T _item) {
        if (index > _count) index = _count;
        iterator** itp = &_first;
        for (int i = index; i > 0; i--) itp = (*itp)->pnext();
        *itp = new iterator(_item, *itp);
        if (index == count()) _lastp = (*itp)->pnext();
        _count++;
        return *this;
    }
    T& remove(size_t index) { }
    T& pop() { }
    T& first() { return *_first; }
    iterator begin() const { return *_first; }
    iterator end() const { return iterator(0); }
    void print() {
        ::print("[");
        for (auto c = begin(); c != end(); ++c) {
            ::print(*c);
            if (c != end()) ::print(" -> ");
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
// void List<cchars>::print() {
//     ::print("[");
//     for (auto c = begin(); c != end(); ++c) {
//         if (c !=  begin()) ::print(", ");
//         ::print("\"");
//         ::print(*c);
//         ::print("\"");
//     }
//     ::print("]");
// }

class ConstCharsArray : public Collection {
    Array<cchars> arr;

public:
    // ConstCharsArray& push(cchars _item) {
    //     return REINTERPRET;
    // }

    // TODO: all string allocations should leave 1 char before and 3 chars after
    // as extra chars. The 3 chars after should be NULL. 1 char before and 1
    // after serve as places to put double quotes and give an in-place quote()
    // function. 2 NULLs serve as terminators with the possibility to always do
    // 1-char lookahead while scanning the string. The final alloc size should
    // anyway be a multiple of 8 or 16 B.
    cchars join() {
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
        cchars joined = join();
        clear();
        shift(joined);
    }

    Array<cchars>::iterator begin() const { return arr.begin(); }
    Array<cchars>::iterator end() const { return arr.end(); }
    // ~Array() { free(ref); }

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

    ConstCharsArray& push(cchars _item) { return arr.push(_item), *this; }
    ConstCharsArray& pushn(size_t n, cchars items[]) {
        return arr.pushn(n, items), *this;
    }
    ConstCharsArray& shift(cchars _item) { return arr.shift(_item), *this; }
    ConstCharsArray& insert(size_t index, cchars _item) {
        return arr.insert(index, _item), *this;
    }
    // Array<T>& shiftn(size_t n, T items[]) {
    //     if (len + n >= cap) resizeto(roundup32(len));
    //     memmove(ref + n  , ref, used * sizeof(T));
    //     ref[0] = _item;
    // }

    cchars& pop() { return arr.pop(); }
    cchars& top() { return arr.top(); }
    bool empty() { return arr.empty(); }
    size_t count() { return arr.count(); }
    void clear() { arr.clear(); }
};

#define CARRAY(T, ...)                                                         \
    (T[]) { __VA_ARGS__ }
int foo() {
    Array<int> ls;
    ls.push(3).push(32).push(67).shift(1);
    ls.print();
    auto sted = ConstCharsArray();
    sted.pushn(3, CARRAY(cchars, "hello", "there", "wowee"))
        .shift("hey")
        .insert(4, "bouk")
        .push("adsa")
        .print();
    for (auto& x : ls) { }
    // YOU CAN'T TAG ON print() to the above chain!!! if derived type
    // if you do it will call base class print() instead.
    // update:
    // NOW YOU CAN BECAUSE ALL METHODS RETURN ConstCharsArray!
    // FOR THIS YOU HAVE TO WRITE WRAPPERS.
    // COMPOSITION vs inheritance...
    print(sted.join());
    return 0;
}

int malin() { foo(); }

/* The MIT License

Copyright(c) 2008, 2009,
    2011 by Attractive Chaos<attractor @live.co.uk>(c) 2019,
    2020 The Jet Language Team<sushpa @jetpilots.dev>

    Permission is hereby granted, free of charge,
    to any person obtaining a copy of this software and associated documentation
    files(the "Software"),
    to deal in the Software without restriction,
    including without limitation the rights to use, copy, modify, merge, publish
    ,
    distribute, sublicense, and / or sell copies of the Software,
    and to permit persons to whom the Software is furnished to do so,
    subject to the following conditions :

    The above copyright notice and this permission notice shall be included in
    all copies
    or substantial portions of the Software.

       THE SOFTWARE IS PROVIDED "AS IS",
    WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
    INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
    DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
        THE SOFTWARE.
    */

#ifndef HAVE_DICT
#define HAVE_DICT

#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define _flagsSize(m) ((m) < 16 ? 1 : (m) >> 4)

#define roundUp32(x)                                                           \
    (--(x), (x) |= (x) >> 1, (x) |= (x) >> 2, (x) |= (x) >> 4,                 \
        (x) |= (x) >> 8, (x) |= (x) >> 16, ++(x))

static const double HASH_UPPER = 0.77;

// Sets do not have any values, but the type of the value ptr is char*.

// Generally it is not recommended to use   has because you call it and
// then you call   get again. So just call   get and do whatever.
// If you don't really want the index, then its fine to call   has.
// #define Dict <K, V>##K##_##V
// #define init(K, V) init_##K##_##V
// #define make(K, V) make_##K##_##V
// #define free(K, V) free_##K##_##V
// #define freedata(K, V) freedata_##K##_##V
// #define clear(K, V) clear_##K##_##V
// #define resize(K, V) resize_##K##_##V
// #define put(K, V) put_##K##_##V
// #define get(K, V) get_##K##_##V
// #define has(K, V) has_##K##_##V
// #define delete (K, V) del_##K##_##V
// #define deleteByKey(K, V) delk_##K##_##V

// // TODO: why not void instead of char?
// #define Set(K) Dict(K, char)
// #define Set_init(K) init(K, char)
// #define Set_make(K) make(K, char)
// #define Set_free(K) free(K, char)
// #define Set_freedata(K) freedata(K, char)
// #define Set_clear(K) clear(K, char)
// #define Set_resize(K) resize(K, char)
// #define Set_put(K) put(K, char)
// #define Set_get(K) get(K, char)
// #define Set_has(K) has(K, char)
// #define Set_del(K) delete (K, char)
// #define Set_delk(K) deleteByKey(K, char)

template <class K, class V, bool IsMap>
class Dict : public Collection {
    uint32_t nBuckets, size, nOccupied, upperBound;
    uint32_t* flags;
    K* keys;
    V* vals;

    // TODO: move the implementation into  runtime.h
    // #define __ IMPL( , K, V, IsMap, hash, equal)
    Dict<K, V, IsMap>* init() { return calloc(1, sizeof(Dict<K, V, IsMap>)); }
    void freedata() {
        // if (h) {
        free(keys);
        free(flags);
        if (IsMap) free(vals);
        // }
    }

    bool _empty(uint32_t i) { return (flags[i >> 4] >> ((i & 0xfU) << 1)) & 2; }
    bool _deleted(uint32_t i) {
        return (flags[i >> 4] >> ((i & 0xfU) << 1)) & 1;
    }
    bool _emptyOrDel(uint32_t i) {
        return (flags[i >> 4] >> ((i & 0xfU) << 1)) & 3;
    }

    void _setNotDeleted(uint32_t i) {
        flags[i >> 4] &= ~(1ul << ((i & 0xfU) << 1));
    }
    void _setDeleted(uint32_t i) { flags[i >> 4] |= 1ul << ((i & 0xfU) << 1); }
    void _setNotEmpty(uint32_t i) {
        flags[i >> 4] &= ~(2ul << ((i & 0xfU) << 1));
    }
    void _clearFlags(uint32_t i) {
        flags[i >> 4] &= ~(3ul << ((i & 0xfU) << 1));
    }

    // void free() { free(thi); }
    void clear() {
        if (flags) {
            memset(flags, 0xAA, _flagsSize(nBuckets) * sizeof(uint32_t));
            size = nOccupied = 0;
        }
    }
    uint32_t get(K key) {
        if (nBuckets) {
            uint32_t k, i, last, mask, step = 0;
            mask = nBuckets - 1;
            k = hash(key);
            i = k & mask;
            last = i;
            while (not _empty(i) and (_deleted(i) or !equal(keys[i], key))) {
                i = (i + (++step)) & mask;
                if (i == last) return nBuckets;
            }
            return _emptyOrDel(i) ? nBuckets : i;
        } else
            return 0;
    }
    bool has(K key) {
        uint32_t x = get(key);
        return x < nBuckets and exist(x);
    }
    int resize(uint32_t nnBuckets) {
        uint32_t* nFlags = 0;
        uint32_t j = 1;
        {
            roundUp32(nnBuckets);
            if (nnBuckets < 4) nnBuckets = 4;
            if (size >= (uint32_t)(nnBuckets * HASH_UPPER + 0.5))
                j = 0; // requested size is too small
            else { // size to be changed (shrink or expand); rehash
                nFlags = malloc(_flagsSize(nnBuckets) * sizeof(uint32_t));
                if (not nFlags) return -1;
                memset(nFlags, 0xAA, _flagsSize(nnBuckets) * sizeof(uint32_t));
                if (nBuckets < nnBuckets) { // expand
                    K* nKeys = realloc(keys, nnBuckets * sizeof(K));
                    if (not nKeys) {
                        free(nFlags);
                        return -1;
                    }
                    keys = nKeys;
                    if (IsMap) {
                        V* nVals = realloc(vals, nnBuckets * sizeof(V));
                        if (not nVals) {
                            free(nFlags);
                            return -1;
                        }
                        vals = nVals;
                    }
                } // otherwise shrink
            }
        }
        if (j) { // rehashing is needed
            for (j = 0; j != nBuckets; ++j) {
                if (_emptyOrDel(flags, j) == 0) {
                    K key = keys[j];
                    V val;
                    uint32_t new_mask;
                    new_mask = nnBuckets - 1;
                    if (IsMap) val = vals[j];
                    _setDeleted(j);
                    // kick-out process; sort of like in Cuckoo hashing
                    while (1) {
                        uint32_t k, i, step = 0;
                        k = hash(key);
                        i = k & new_mask;
                        while (not _empty(nFlags, i))
                            i = (i + (++step)) & new_mask;
                        _setNotEmpty(nFlags, i);
                        if (i < nBuckets and _emptyOrDel(i) == 0) {
                            // kick out the existing element
                            {
                                K tmp = keys[i];
                                keys[i] = key;
                                key = tmp;
                            }
                            if (IsMap) {
                                V tmp = vals[i];
                                vals[i] = val;
                                val = tmp;
                            }
                            _setDeleted(i);
                            // mark it _deleted in the old table
                        } else { // write the element and break the loop
                            keys[i] = key;
                            if (IsMap) vals[i] = val;
                            break;
                        }
                    }
                }
            }
            if (nBuckets > nnBuckets) { // shrink the hash table
                keys = realloc(keys, nnBuckets * sizeof(K));
                if (IsMap) vals = realloc(vals, nnBuckets * sizeof(V));
            }
            free(flags); // free the working space
            flags = nFlags;
            nBuckets = nnBuckets;
            nOccupied = size;
            upperBound = (uint32_t)(nBuckets * HASH_UPPER + 0.5);
        }
        return 0;
    }
    uint32_t put(K key, int* ret) {
        uint32_t x;
        if (nOccupied >= upperBound) { // update the hash table
            if (nBuckets > (size << 1)) {
                if (resize(nBuckets - 1) < 0) { // clear "_deleted" elements
                    *ret = -1;
                    return nBuckets;
                }
            } else if (resize(nBuckets + 1) < 0) { // expand the hash table
                *ret = -1;
                return nBuckets;
            }
        }
        // TODO: to implement automatically shrinking;
        // resize() already support shrinking

        uint32_t k, i, site, last, mask = nBuckets - 1, step = 0;
        x = site = nBuckets;
        k = hash(key);
        i = k & mask;
        if (_empty(i))
            x = i; // for speed up
        else {
            last = i;
            while (not _empty(i) and (_deleted(i) or not equal(keys[i], key))) {
                if (_deleted(i)) site = i;
                i = (i + (++step)) & mask;
                if (i == last) {
                    x = site;
                    break;
                }
            }
            if (x == nBuckets) {
                x = (_empty(i) and site != nBuckets) ? site : i;
            }
        }

        if (_empty(x)) { // not present at all
            keys[x] = key;
            _clearFlags(x);
            ++size;
            ++nOccupied;
            *ret = 1;
        } else if (_deleted(x)) { //   _deleted
            keys[x] = key;
            _clearFlags(x);
            ++size;
            *ret = 2;
        } else
            *ret = 0; // Don't touch  keys[x] if present and not _deleted
        return x;
    }
    void del(uint32_t x) {
        if (x != nBuckets and !_emptyOrDel(x)) {
            _setDeleted(flags, x);
            --size;
        }
    }
    void deleteByKey(K key) { del(get(key)); }
    Dict() { }
    Dict(int size, K keys[], V values[]) {
        // Dict<K, V, IsMap>* ret = init();
        int p;
        for (int i = 0; i < size; i++) {
            uint32_t idx = put(keys[i], &p);
            val(i) = values[i];
        }
        // return ret;
    }

    // #define DECLARE(K, V) __ TYPE(K, V) __ PROTOTYPES(K, V)

    // #define INIT2(K, V, IsMap, hash, equal) //     __ TYPE(K, V) __ IMPL(K,
    // V, IsMap, hash, equal)

    // #define INIT(K, V, IsMap, hash, equal) INIT2(static, K, V, IsMap, hash,
    // equal)

    // --- BEGIN OF HASH FUNCTIONS ---

    // careful, this is really just pointer equality/hash,
    // not underlying object equality/hash
    // TODO: handle 32/64 bit
    // #define Ptr_hashkey UInt64_hash((UInt64)key)
    // #define Ptr_equal(a, b) ((a) == (b))

    // #define Int64_equal(a, b) ((a) == (b))

    // #define Int64_hashkey UInt64_hash((UInt64)key)

    // #define Real64_hashkey UInt64_hash(*(UInt64*)&key)

//     static uint32_t Real64_hash(double key) {
//         UInt64* ptr = (UInt64*)&key;
//         return UInt64_hash(*ptr);
//     }

// #define Real64_equal(a, b) ((a) == (b))

// ---
#define _rotl_KAZE(x, n) (((x) << (n)) | ((x) >> (32 - (n))))
#define _PADr_KAZE(x, n) (((x) << (n)) >> (n))
#define ROLInBits 27
    // 5 in r.1; Caramba: it should be ROR by 5 not ROL, from the very
    // beginning the idea was to mix two bytes by shifting/masking the first
    // 5 'noisy' bits (ASCII 0-31 symbols).
    // CAUTION: Add 8 more bytes to the buffer being hashed, usually
    // malloc(...+8) - to prevent out of boundary reads!
    static uint32_t FNV1A_Hash_Yorikke_v3(const char* str, uint32_t wrdlen) {
        const uint32_t PRIME = 591798841;
        uint32_t hash32 = 2166136261;
        uint64_t PADDEDby8;
        const char* p = str;
        for (; wrdlen > 2 * sizeof(uint32_t);
             wrdlen -= 2 * sizeof(uint32_t), p += 2 * sizeof(uint32_t)) {
            hash32 = (_rotl_KAZE(hash32, ROLInBits) ^ (*(uint32_t*)(p + 0)))
                * PRIME;
            hash32 = (_rotl_KAZE(hash32, ROLInBits) ^ (*(uint32_t*)(p + 4)))
                * PRIME;
        }
        // Here 'wrdlen' is 1..8
        PADDEDby8 = _PADr_KAZE(*(uint64_t*)(p + 0),
            (8 - wrdlen) << 3); // when (8-8) the QWORD remains intact
        hash32 = (_rotl_KAZE(hash32, ROLInBits)
                     ^ *(uint32_t*)((char*)&PADDEDby8 + 0))
            * PRIME;
        hash32 = (_rotl_KAZE(hash32, ROLInBits)
                     ^ *(uint32_t*)((char*)&PADDEDby8 + 4))
            * PRIME;
        return hash32 ^ (hash32 >> 16);
    }
    // Last touch: 2019-Oct-03, Kaze
    // ---

    static inline uint32_t __ac_X31_hash_string(const char* s) {
        uint32_t i = (uint32_t)*s;
        if (i)
            for (++s; *s; ++s) i = (i << 5) - i + (uint32_t)*s;
        return i;
    }

    // for CStrings, assuming they MUST end in \0, you can check for
    // pointer equality to mean string equality. For Strings with lengths,
    // this does not hold since the same buffer can hold a string and any of
    // its prefixes, especially in Jet where not all strings end with '\0'.

    static inline uint32_t __ac_Wang_hash(uint32_t key) {
        key += ~(key << 15);
        key ^= (key >> 10);
        key += (key << 3);
        key ^= (key >> 6);
        key += ~(key << 11);
        key ^= (key >> 16);
        return key;
    }

#define int_hash_func2key __ac_Wang_hash((uint32_t)key)

    bool exist(int x) { return not _emptyOrDel(x); }

    K key(int x) { return keys[x]; }

    V val(int x) { return vals[x]; }

    int begin(int h) { return (uint32_t)0; }

    int end(int h) { return nBuckets; }

    // int size() { return size; }

    // int nBuckets() { return nBuckets; }
};

uint32_t hash(uint32_t key) { return key; }
uint32_t hash(int32_t key) { return key; }
uint32_t hash(int64_t key) { return hash((uint64_t)key); }
uint32_t hash(uint64_t key) { return (uint32_t)(key >> 33 ^ key ^ key << 11); }
uint32_t hash(const char* key) { return __ac_X31_hash_string(key); }

bool equal(uint32_t a, uint32_t b) { return a == b; }
bool equal(int32_t a, int32_t b) { return a == b; }
bool equal(uint64_t a, uint64_t b) { return a == b; }
bool equal(int64_t a, int64_t b) { return a == b; }
bool equal(const char* a, const char* b) { return a == b or not strcmp(a, b); }
// #define foreach(kvar, vvar, code) //     { //         for (uint32_t _i_ =
// begin(h); _i_ != end(h); ++_i_) {                   //             if (not
// exist(_i_)) continue;                                       // kvar =
// key(_i_);                                                    // vvar =
// val(_i_);                                                    // code; // } //
// }

// #define foreach_value(vvar, code) //     { //         for (uint32_t _i_ =
// begin(h); _i_ != end(h); ++_i_) {                   //             if (not
// exist(_i_)) continue;                                       // vvar =
// val(_i_);                                                    // code; // } //
// }

// #define Set_foreach(kvar, code) foreach_key(kvar, code)
// #define foreach_key(kvar, code) //     { //         for (uint32_t _i_ =
// begin(h); _i_ != end(h); ++_i_) {                   //             if (not
// exist(_i_)) continue;                                       // kvar =
// key(_i_);                                                    // code; // } //
// }

// #define MAKE_SET(T) INIT(T, char, false, T##_hash, T##_equal)
// #define MAKE_DICT <K, V> INIT(K, V, true, K##_hash, K##_equal)

// No predefined instantiations. MAKE_DICT calls will be inserted by the
// Jet compiler in every compilation unit depending on what types are found
// to have been used in dicts.
// Better yet, get rid of the macro templating and define ONLY a single
// Dict type void* -> void*; means you cannot embed structs but this is
// not too bad. void* to/from any type* needs no explicit cast in C.
// Besides you can still embed things up to 8B...

// MAKE_SET(uint32_t)
// // MAKE_SET(Real64)
// MAKE_SET(CString)
// // MAKE_SET(Ptr)

// MAKE_DICT(uint32_t, uint32_t)
// MAKE_DICT(CString, uint32_t)

// MAKE_DICT(uint32_t, Ptr)
// MAKE_DICT(CString, Ptr)
// MAKE_DICT(uint32_t, CString)
// MAKE_DICT(CString, CString)
// MAKE_DICT(UInt64, Ptr)
// MAKE_DICT(CString, Real64)

// MAKE_DICT(Ptr, UInt64)
// MAKE_DICT(Ptr, Ptr)

#endif // HAVE_DICT

template <typename C>
void testfunc(Array<C> coll, int n) {
    for (auto& item : coll) testfunc(item, n);
}
template <typename C>
void testfunc(List<C> coll, int n) {
    for (auto& item : coll) testfunc(item, n);
}

template <typename C, typename D, bool g>
void testfunc(Dict<C, D, g> coll, int n) {
    for (auto& item : coll) testfunc(item, n);
}

int f() {
    Dict<int, int, false> dt;
    Array<int> ar;
    List<int> li;
    List1<int> li1;
    testfunc(dt, 87);
    testfunc(ar, 87);
    testfunc(li, 87);
    testfunc(li1, 87);
}