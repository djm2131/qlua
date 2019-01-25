#include "lhpc-aff++.h"
#include <string.h>

static char *
my_strsep( char *str, char **end, char delim )
{
    if( NULL == str )
        return *end = NULL;
    char *s;
    for( s = str ; delim != *s && '\0' != *s; ++s );
    if( '\0' == *s )
        *end = NULL;
    else
    {
        *s = '\0';
        *end = s + 1;
    }
    return str;
}


namespace Aff {
int 
Reader::open(const char *fname)
{
    r_ = aff_reader(fname);
    if (NULL == r_)
        return 1;
    if (NULL != (errstr_ = aff_reader_errstr(r_))) {
        close();
        return 1;
    }
    return 0;
}
void 
Reader::close(void)
{
    if (NULL == r_)
        return;
    aff_reader_close(r_);
    r_ = NULL;
}
const char *
Reader::errstr(void)
{
    if (NULL == r_)
        return "Not opened";
    else if (NULL != errstr_)
        return errstr_;
    else
        return aff_reader_errstr(r_);
}
char *
Reader::full_path(Reader::Node *n)
{
    size_t len = 0;
    if (root() != n) {
        Node *cn = n;
        while (root() != cn) {
            len += strlen(name(cn)) + 1;
            cn = aff_node_parent(cn);
        }
        char *path = new char[len + 1];
        path[len] = '\0';
        cn = n;
        char *s = path + len;
        while (root() != cn) {
            size_t slen = strlen(name(cn));
            s -= slen + 1;
            s[0] = '/';
            memcpy(s+1, name(cn), slen);
            cn = aff_node_parent(cn);
        }
        assert(path == s);
        return path;
    } else {
        char *path = new char[2];
        path[0] = '/';
        path[1] = '\0';
        return path;
    }
}
Reader::Node *
Reader::chdir_path(Reader::Node *r_node, const char *name)
{
    if (NULL == r_||
        NULL == name)
        return NULL;
    char *kpath = new char[strlen(name) + 1];
    if (NULL == kpath)
        return NULL;
    strcpy(kpath, name);
    char *s, *end = kpath;
    if ('/' == *end )
        r_node = root();
    if (NULL == r_node) {
        delete [] kpath;
        return NULL;
    }
    while (NULL != (s = my_strsep(end, &end, '/')) &&
            r_node != NULL) {
        if ('\0' == *s)
            continue;
        r_node = chdir(r_node, s);
    }
    delete [] kpath;
    return r_node;
}
Reader::Node *
Reader::lookup_path(Reader::Node *r_node, const char *name)
{
    if (NULL == r_||
        NULL == name)
        return NULL;
    char *kpath = new char[strlen(name) + 1];
    if (NULL == kpath)
        return NULL;
    strcpy(kpath, name);
    char *s, *end = kpath;
    if ('/' == *end )
        r_node = root();
    if (NULL == r_node) {
        delete [] kpath;
        return NULL;
    }
    while (NULL != (s = my_strsep(end, &end, '/')) &&
            r_node != NULL) {
        if ('\0' == *s)
            continue;
        r_node = lookup(r_node, s);
    }
    delete [] kpath;
    return r_node;
}
int 
Reader::get_char(Node *n, char *d, uint32_t s)
{
    if (affNodeChar != type(n))
    {
        errstr_ = "Wrong node type";
        return 1;
    }
    return aff_node_get_char(r_, n, d, s);
}
int 
Reader::get_int(Node *n, uint32_t *d, uint32_t s)
{
    if (affNodeInt != type(n)) {
        errstr_ = "Wrong node type";
        return 1;
    }
    return aff_node_get_int(r_, n, d, s);
}
int 
Reader::get_double(Node *n, double *d, uint32_t s)
{
    if (affNodeDouble != type(n)) {
        errstr_ = "Wrong node type";
        return 1;
    }
    return aff_node_get_double(r_, n, d, s);
}
int 
Reader::get_complex(Node *n, std::complex<double> *d, uint32_t s)
{
    if (affNodeComplex != type(n)) {
        errstr_ = "Wrong node type";
        return 1;
    }
    return aff_node_get_complex(r_, n, (__complex__ double *)d, s);
}



int
Writer::open(const char *fname)
{
    w_ = aff_writer(fname);
    if (NULL == w_)
        return 1;
    if (NULL != (errstr_ = aff_writer_errstr(w_))) {
        close();
        return 1;
    }
    return 0;
}
int 
Writer::close(void)
{
    if (NULL == w_)
        return 1;
    if (NULL != (errstr_ = aff_writer_close(w_))) {
        w_ = NULL;
        return 1;
    }
    w_ = NULL;
    return 0;
}
const char *
Writer::errstr(void)
{
    if (NULL == w_)
        return "Not opened";
    else if (NULL != errstr_)
        return errstr_;
    else
        return aff_writer_errstr(w_);
}
char *
Writer::full_path(Writer::Node *n)
{
    size_t len = 0;
    if (root() != n) {
        Node *cn = n;
        while (root() != cn) {
            len += strlen(name(cn)) + 1;
            cn = aff_node_parent(cn);
        }
        char *path = new char[len + 1];
        path[len] = '\0';
        cn = n;
        char *s = path + len;
        while (root() != cn) {
            size_t slen = strlen(name(cn));
            s -= slen + 1;
            s[0] = '/';
            memcpy(s+1, name(cn), slen);
            cn = aff_node_parent(cn);
        }
        assert(path == s);
        return path;
    } else {
        char *path = new char[2];
        path[0] = '/';
        path[1] = '\0';
        return path;
    }
}
Writer::Node *
Writer::mkdir_path(Writer::Node *w_node, const char *name)
{
    if (NULL == w_ ||
        NULL == name )
        return NULL;
    char *kpath = new char[strlen(name) + 1];
    if (NULL == kpath)
        return NULL;
    strcpy(kpath, name);
    char *s, *end = kpath;
    if ('/' == *end)
        w_node = root();
    if (NULL == w_node) {
        delete [] kpath;
        return NULL;
    }
    while (NULL != (s = my_strsep(end, &end, '/')) &&
            w_node != NULL) {
        if ('\0' == *s)
            continue;
        w_node = mkdir(w_node, s);
    }
    delete [] kpath;
    return w_node;
}
int 
Writer::put_char(Node *n, const char *d, uint32_t s)
{
    if (affNodeVoid != type(n)) {
        errstr_ = "Node not empty";
        return 1;
    }
    return aff_node_put_char(w_, n, d, s);
}
int 
Writer::put_int(Node *n, const uint32_t *d, uint32_t s)
{
    if (affNodeVoid != type(n)) {
        errstr_ = "Node not empty";
        return 1;
    }
    return aff_node_put_int(w_, n, d, s);
}
int 
Writer::put_double(Node *n, const double *d, uint32_t s)
{
    if (affNodeVoid != type(n)) {
        errstr_ = "Node not empty";
        return 1;
    }
    return aff_node_put_double(w_, n, d, s);
}
int 
Writer::put_complex(Node *n, const std::complex<double> *d, uint32_t s)
{
    if (affNodeVoid != type(n)) {
        errstr_ = "Node not empty";
        return 1;
    }
    return aff_node_put_complex(w_, n, (__complex__ double *)d, s);
}


} /* namespace Aff */
