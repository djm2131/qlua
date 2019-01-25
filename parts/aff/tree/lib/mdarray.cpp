#include "lhpc-aff++.h"

namespace Aff {

int
MDArrayInt::read(Reader &r, Reader::Node *node)
{
    if (NULL == node)
        return 1;
    Reader::Node *node_dim = r.lookup(node, "dim");
    if (NULL == node_dim)
        return 1;
    if (r.get_int(node_dim, dim_))
        return 1;
    ndim_ = dim_.size();
    if (r.get_int(node, data_))
        return 1;
    if (data_.size() != size_())
        return 1;
    return 0;
}
int 
MDArrayInt::write(Writer &w, Writer::Node *node)
{
    if (NULL == node)
        return 1;
    Writer::Node *node_dim = w.mkdir(node, "dim");
    if (NULL == node_dim)
        return 1;
    assert(ndim_ == dim_.size());
    if (w.put_int(node_dim, dim_))
        return 1;
    assert(data_.size() == size_());
    if (w.put_int(node, data_))
        return 1;
    return 0;
}

int
MDArrayDouble::read(Reader &r, Reader::Node *node)
{
    if (NULL == node)
        return 1;
    Reader::Node *node_dim = r.lookup(node, "dim");
    if (NULL == node_dim)
        return 1;
    if (r.get_int(node_dim, dim_))
        return 1;
    ndim_ = dim_.size();
    if (r.get_double(node, data_))
        return 1;
    if (data_.size() != size_())
        return 1;
    return 0;
}
int 
MDArrayDouble::write(Writer &w, Writer::Node *node)
{
    if (NULL == node)
        return 1;
    Writer::Node *node_dim = w.mkdir(node, "dim");
    if (NULL == node_dim)
        return 1;
    assert(ndim_ == dim_.size());
    if (w.put_int(node_dim, dim_))
        return 1;
    assert(data_.size() == size_());
    if (w.put_double(node, data_))
        return 1;
    return 0;
}

int
MDArrayComplex::read(Reader &r, Reader::Node *node)
{
    if (NULL == node)
        return 1;
    Reader::Node *node_dim = r.lookup(node, "dim");
    if (NULL == node_dim)
        return 1;
    if (r.get_int(node_dim, dim_))
        return 1;
    ndim_ = dim_.size();
    if (r.get_complex(node, data_))
        return 1;
    if (data_.size() != size_())
        return 1;
    return 0;
}
int 
MDArrayComplex::write(Writer &w, Writer::Node *node)
{
    if (NULL == node)
        return 1;
    Writer::Node *node_dim = w.mkdir(node, "dim");
    if (NULL == node_dim)
        return 1;
    assert(ndim_ == dim_.size());
    if (w.put_int(node_dim, dim_))
        return 1;
    assert(data_.size() == size_());
    if (w.put_complex(node, data_))
        return 1;
    return 0;
}

} /* namespace Aff */
