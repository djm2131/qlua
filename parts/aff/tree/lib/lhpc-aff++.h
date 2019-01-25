#ifndef LHPC_AFFPP_WfMmQXsGsSygJEofBOzd
#define LHPC_AFFPP_WfMmQXsGsSygJEofBOzd

#include "lhpc-aff.h"
#include <complex>
#include <cassert>
#include <vector>
#include <algorithm>

namespace Aff {

typedef AffNodeType_e NodeType;


template<class Func>
void func_apply(struct AffNode_s *child, void *arg)
{
    ((Func *)arg)->operator()(child);
}
template<class Func>
void foreach(struct AffNode_s *n, Func &func)
{
    aff_node_foreach(n, func_apply<Func>, (void *)&func);
}

// Wrapper for AffReader_s 
struct Reader
{
private:
    Reader(const Reader&) {};
    Reader(void) : r_(NULL), errstr_(NULL) {}
public:
    typedef struct AffNode_s Node;
    Reader(const char *fname)
    {
        open(fname);
    }
    ~Reader()
    {
        if (NULL != r_)
            aff_reader_close(r_);
    }
    void close(void);
    const char *errstr(void);
    void clearerr(void)
    {
        errstr_ = NULL;
    }
    bool operator!(void)
    {
        return (NULL != errstr() || NULL != aff_reader_errstr(r_));
    }
    // tree navigation
    Node *chdir(Node *dir, const char *name)
    {
        return aff_reader_chdir(r_, dir, name);
    }
    Node *chdir_path(Node *dir, const char *name);
    Node *lookup(Node *dir, const char *name)
    {
        return aff_node_chdir(tree(), stable(), dir, 0, name);
    }
    Node *lookup_path(Node *dir, const char *name);
    Node *root(void)
    {
        return aff_reader_root(r_);
    }
    char *full_path(Node *n);
    // data read
    int get_char(Node *n, char *d, uint32_t s);
    int get_int(Node *n, uint32_t *d, uint32_t s);
    int get_complex(Node *n, std::complex<double> *d, uint32_t s);
    int get_double(Node *n, double *d, uint32_t s);
    // data read to arbitrary container
    // Container must have typedef value_type and methods resize and operator[]
    template<class Container>
    int get_char(Node *n, Container& c)
    {
        uint32_t s = size(n);
        char *buf = new char[s];
        if (NULL == buf)
        {
            errstr_ = "Not enough memory";
            return 1;
        }
        if (get_char(n, buf, s)) {
            delete [] buf;
            return 1;
        }
        c.resize(s);
        const char *cbuf = buf;
        for (size_t i = 0 ; i < s ; i++)
            c[i] = static_cast<typename Container::value_type>(
                        static_cast<signed char>(*(cbuf++)));
        delete [] buf;
        return 0;
    }
    template<class Container>
    int get_int(Node *n, Container& c)
    {
        uint32_t s = size(n);
        uint32_t *buf = new uint32_t[s];
        if (NULL == buf) {
            errstr_ = "Not enough memory";
            return 1;
        }
        if (get_int(n, buf, s)) {
            delete [] buf;
            return 1;
        }
        c.resize(s);
        const uint32_t *cbuf = buf;
        for (size_t i = 0 ; i < s ; i++)
            c[i] = static_cast<typename Container::value_type>(
                        static_cast<int32_t>(*(cbuf++)));
        delete [] buf;
        return 0;
    }
    template<class Container>
    int get_double(Node *n, Container& c)
    {
        uint32_t s = size(n);
        double *buf = new double[s];
        if (NULL == buf) {
            errstr_ = "Not enough memory";
            return 1;
        }
        if (get_double(n, buf, s)) {
            delete [] buf;
            return 1;
        }
        c.resize(s);
        const double *cbuf = buf;
        for (size_t i = 0 ; i < s ; i++)
            c[i] = static_cast<typename Container::value_type>(*(cbuf++));
        delete [] buf;
        return 0;
    }
    template<class Container>
    int get_complex(Node *n, Container& c)
    {
        uint32_t s = size(n);
        std::complex<double> *buf = new std::complex<double>[s];
        if (NULL == buf) {
            errstr_ = "Not enough memory";
            return 1;
        }
        if (get_complex(n, buf, s)) {
            delete [] buf;
            return 1;
        }
        c.resize(s);
        const std::complex<double> *cbuf = buf;
        for (size_t i = 0 ; i < s ; i++)
            c[i] = static_cast<typename Container::value_type>(*(cbuf++));
        delete [] buf;
        return 0;
    }
    // node info
    NodeType type(const Node *n) const
    {
        return aff_node_type(n);
    }
    uint32_t size(const Node *n) const
    {
        return aff_node_size(n);
    }
    const char *name(const Node *n) const
    {
        return aff_symbol_name(aff_node_name(n));
    }
protected:
    typedef struct AffTree_s Tree;
    typedef struct AffSTable_s STable;
    struct AffReader_s *r_;
    const char *errstr_;
    
    int open(const char *fname);
    Tree* tree(void)
    {
        return aff_reader_tree(r_);
    }
    STable* stable(void)
    {
        return aff_reader_stable(r_);
    }
};
#if 0
struct ReaderHandle : public Reader
{};
#endif
struct Writer
{
private:
    Writer(const Writer&) {}
    Writer(void) : w_(NULL), errstr_(NULL) {}
public:
    typedef struct AffNode_s Node;
    
    Writer(const char *fname)
    {
        open(fname);
    }
    ~Writer()
    {
        if (NULL != w_)
            aff_writer_close(w_);
    }
    int close(void);
    const char *errstr(void);
    void clearerr(void)
    {
        errstr_ = NULL;
    }
    bool operator!(void)
    {
        return (NULL != errstr_ || NULL != aff_writer_errstr(w_));
    }
    Node *mkdir(Node *dir, const char *name)
    {
        return aff_writer_mkdir(w_, dir, name);
    }
    Node *mkdir_path(Node *dir, const char *name);
    Node *root(void)
    {
        return aff_writer_root(w_);
    }
    char *full_path(Node *n);
    // write data
    int put_char(Node *n, const char *d, uint32_t s);
    int put_int(Node *n, const uint32_t *d, uint32_t s);
    int put_double(Node *n, const double *d, uint32_t s);
    int put_complex(Node *n, const std::complex<double> *d, uint32_t s);
    // write data from arbitrary container
    // Container must have methods size and operator[]
    template<class Container>
    int put_char(Node *n, const Container& c)
    {
        uint32_t s = c.size();
        char *buf = new char[s];
        if (NULL == buf) {
            errstr_ = "Not enough memory";
            return 1;
        }
        char *cbuf = buf;
        for (size_t i = 0 ; i < s ; i++)
            *(cbuf++) = c[i];
        if (put_char(n, buf, s)) {
            delete [] buf;
            return 1;
        }
        delete [] buf;
        return 0;
    }
    template<class Container>
    int put_int(Node *n, const Container& c)
    {
        uint32_t s = c.size();
        uint32_t *buf = new uint32_t[s];
        if (NULL == buf) {
            errstr_ = "Not enough memory";
            return 1;
        }
        uint32_t *cbuf = buf;
        for (size_t i = 0 ; i < s ; i++)
            *(cbuf++) = c[i];
        if (put_int(n, buf, s)) {
            delete [] buf;
            return 1;
        }
        delete [] buf;
        return 0;
    }
    template<class Container>
    int put_double(Node *n, const Container& c)
    {
        uint32_t s = c.size();
        double *buf = new double[s];
        if (NULL == buf) {
            errstr_ = "Not enough memory";
            return 1;
        }
        double *cbuf = buf;
        for (size_t i = 0 ; i < s ; i++)
            *(cbuf++) = c[i];
        if (put_double(n, buf, s)) {
            delete [] buf;
            return 1;
        }
        delete [] buf;
        return 0;
    }
    template<class Container>
    int put_complex(Node *n, const Container& c)
    {
        uint32_t s = c.size();
        std::complex<double> *buf = new std::complex<double>[s];
        if (NULL == buf) {
            errstr_ = "Not enough memory";
            return 1;
        }
        std::complex<double> *cbuf = buf;
        for (size_t i = 0 ; i < s ; i++)
            *(cbuf++) = c[i];
        if (put_complex(n, buf, s)) {
            delete [] buf;
            return 1;
        }
        delete [] buf;
        return 0;
    }

    NodeType type(const Node *n) const
    {
        return aff_node_type(n);
    }
    uint32_t size(const Node *n) const
    {
        return aff_node_size(n);
    }
    const char *name(const Node *n) const
    {
        return aff_symbol_name(aff_node_name(n));
    }

protected:
    typedef AffTree_s Tree;
    typedef AffSTable_s STable;
    struct AffWriter_s *w_;
    const char *errstr_;

    int open(const char *fname);
    Tree *tree(void)
    {
        return aff_writer_tree(w_);
    }
    STable *stable(void)
    {
        return aff_writer_stable(w_);
    }
};

struct Data {
public:
    virtual int read(Reader &r, Reader::Node *n)    = 0;
    virtual int write(Writer &w, Writer::Node *n)   = 0;
    virtual ~Data()
    {}
};

// Multidimensional array
template<class T>
struct MDArray : public Data {
public:
    MDArray() 
      : ndim_(0)
    {}
    MDArray(unsigned int ndim, unsigned int *dim)
      : ndim_(ndim), dim_(ndim)
    {
        std::copy(dim, dim + ndim, dim_.begin());
        data_.resize(size_());
    }
    MDArray(const MDArray& oth)
      : ndim_(oth.ndim_), dim_(oth.dim_), data_(oth.data_)
    {}
    unsigned int ndim(void) const
    {
        return ndim_;
    }
    const std::vector<unsigned int> &dim(void) const
    {
        return dim_;
    }
    template<class Int> 
    T &elem(unsigned int ndim, const Int *ind)
    {
        assert(ndim_ == ndim);
        return data_[linear_(ind)];
    }
    template<class Int> 
    const T& elem(unsigned int ndim, const Int *ind) const
    {
        assert(ndim_ == ndim);
        return data_[linear_(ind)];
    }
    template<class IndContainer>
    T &elem(const IndContainer& ind)
    {
        assert(ndim_ <= ind.size());
        return data_[linear_(&ind[0])];
    }
    template<class IndContainer> 
    const T &elem(const IndContainer& ind) const
    {
        assert(ndim_ <= ind.size());
        return data_[linear_(&ind[0])];
    }
protected:
    unsigned int                ndim_;
    std::vector<unsigned int>   dim_;
    std::vector<T>              data_;
    template<class Int>
    unsigned int linear_(const Int *ind) const
    {
        assert(*ind < dim_[0]);
        register unsigned int res = *(ind++);
        register const unsigned int *mul = &dim_[1];
        for (register unsigned k = ndim_ - 1 ; k-- ;) {
            assert(*ind < *mul);
            res = res * *(mul++) + *(ind++);
        }
        return res;
    }
    unsigned int size_(void) const
    {
        unsigned int size = dim_[0];
        const unsigned int *mul = &dim_[1];
        for (unsigned int k = ndim_ - 1 ; k--;)
            size *= *(mul++);
        return size;
    }
};

struct MDArrayInt : public MDArray<uint32_t> {
public:
    MDArrayInt() 
      : MDArray<uint32_t>() {}
    MDArrayInt(unsigned int ndim, unsigned int *dim)
      : MDArray<uint32_t>(ndim, dim) {}
    MDArrayInt(const MDArrayInt& oth)
      : MDArray<uint32_t>(oth) {}
    int read(Reader &r, Reader::Node *n);
    int write(Writer &w, Writer::Node *n);
};
struct MDArrayDouble : public MDArray<double> {
public:
    MDArrayDouble() 
      : MDArray<double>() {}
    MDArrayDouble(unsigned int ndim, unsigned int *dim)
      : MDArray<double>(ndim, dim) {}
    MDArrayDouble(const MDArrayDouble& oth)
      : MDArray<double>(oth) {}
    int read(Reader &r, Reader::Node *n);
    int write(Writer &w, Writer::Node *n);
};
struct MDArrayComplex : public MDArray<std::complex<double> > {
public:
    MDArrayComplex() 
      : MDArray<std::complex<double> >() {}
    MDArrayComplex(unsigned int ndim, unsigned int *dim)
      : MDArray<std::complex<double> >(ndim, dim) {}
    MDArrayComplex(const MDArrayComplex& oth)
      : MDArray<std::complex<double> >(oth) {}
    int read(Reader &r, Reader::Node *n);
    int write(Writer &w, Writer::Node *n);
};

#if 0
struct WriterHandle : public Writer
{};
#endif

} /* namespace Aff */

#endif /*LHPC_AFFPP_WfMmQXsGsSygJEofBOzd*/
