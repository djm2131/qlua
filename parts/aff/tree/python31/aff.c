#include <Python.h>
#include "lhpc-aff.h"
#include <complex.h>

/* AFF python interface
 */

/* exception object */
static PyObject *paff_exception;

/* reader */
typedef struct {
    PyObject_HEAD
    PyObject            *name;
    struct AffReader_s  *reader;
    struct AffNode_s    *dir;
} paff_Reader;

static void
paff_reader_dealloc(paff_Reader *self)
{
    Py_XDECREF(self->name);
    if (self->reader) {
        aff_reader_close(self->reader);
        self->reader = NULL;
    }
    Py_TYPE(self)->tp_free((PyObject *)self);
}

static PyObject *
paff_reader_str(paff_Reader *self)
{
    if (self->name == NULL) {
        return PyUnicode_FromFormat("<aff Reader without a name at %p>",
									self);
    } else if (self->reader == NULL) {
        return PyUnicode_FromFormat("<closed aff Reader('%s') at %p>",
									PyObject_REPR(self->name),
									self);
    } else {
        return PyUnicode_FromFormat("<aff Reader('%s') at %p>",
									PyObject_REPR(self->name),
									self);
    }
}

static PyObject *
paff_reader_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    static char *kwlist[] = {"name", NULL};
    paff_Reader *self = (paff_Reader *)type->tp_alloc(type, 0);
    const char *msg;
    
    if (self != NULL) {
        if (!PyArg_ParseTupleAndKeywords(args, kwds, "S", kwlist,
                                         &self->name))
            goto error;

        Py_INCREF(self->name);
        self->reader = aff_reader(PyObject_REPR(self->name));
        msg = aff_reader_errstr(self->reader);
        if (msg) {
            PyErr_SetString(paff_exception, msg);
            aff_reader_close(self->reader);
            self->reader = 0;
            Py_DECREF(self->name);
            self->name = 0;
            goto error;
        }
        self->dir = aff_reader_root(self->reader);
    }
    return (PyObject *)self;
error:
    type->tp_free((PyObject *)self);
    return NULL;
}

static PyObject *
paff_reader_close(PyObject *self, PyObject *args)
{
    paff_Reader *r = (paff_Reader *)self;
    if (r->reader) {
        aff_reader_close(r->reader);
        r->reader = 0;
    }
    Py_RETURN_NONE;
}

static PyObject *
paff_reader_name(PyObject *self, PyObject *args)
{
    paff_Reader *r = (paff_Reader *)self;
    Py_INCREF(r->name);
    return r->name;
}

static PyObject *
paff_reader_check(PyObject *self, PyObject *args)
{
    paff_Reader *r = (paff_Reader *)self;
    if (r->reader == NULL) {
        PyErr_SetString(paff_exception, "closed reader");
        return NULL;
    }
    if (aff_reader_check(r->reader)) {
        PyErr_SetString(paff_exception, aff_reader_errstr(r->reader));
        return NULL;
    }
    Py_RETURN_NONE;
}

static struct AffNode_s *
paff_reader_newdir(paff_Reader *r, PyObject *args)
{
    char *path = NULL;
    struct AffNode_s *new_dir;

    if (r->reader == NULL) {
        PyErr_SetString(paff_exception, "closed reader");
        return NULL;
    }
    if (! PyArg_ParseTuple(args, "s", &path))
        return NULL;

    new_dir = aff_reader_chpath(r->reader, r->dir, path);

    if (new_dir == NULL) {
        PyErr_SetString(paff_exception, aff_reader_errstr(r->reader));
        aff_reader_clearerr(r->reader);
    }
    return new_dir;
}

static PyObject *
paff_reader_chdir(PyObject *self, PyObject *args)
{
    paff_Reader *r = (paff_Reader *)self;
    struct AffNode_s *new_dir = paff_reader_newdir(r, args);

    if (new_dir == 0)
        return NULL;

    r->dir = new_dir;

    Py_RETURN_NONE;
}

static PyObject *
paff_to_python_type(struct AffNode_s *dir)
{
    PyObject *result = NULL;
    enum AffNodeType_e t;

    if (dir == 0)
        return NULL;

    t = aff_node_type(dir);
    switch (t) {
    case affNodeVoid:     result = (PyObject *)&PyList_Type;    break;
    case affNodeChar:     result = (PyObject *)&PyUnicode_Type;  break;
    case affNodeInt:      result = (PyObject *)&PyLong_Type;     break;
    case affNodeDouble:   result = (PyObject *)&PyFloat_Type;   break;
    case affNodeComplex:  result = (PyObject *)&PyComplex_Type; break;
    default:
        PyErr_SetString(paff_exception, "Unknown node type");
        return NULL;
    }
    Py_INCREF(result);
    return result;
}

static PyObject *
paff_reader_type(PyObject *self, PyObject *args)
{
    paff_Reader *r = (paff_Reader *)self;
    struct AffNode_s *new_dir = paff_reader_newdir(r, args);

    return paff_to_python_type(new_dir);
}

static PyObject *
paff_reader_size(PyObject *self, PyObject *args)
{
    paff_Reader *r = (paff_Reader *)self;
    struct AffNode_s *new_dir = paff_reader_newdir(r, args);

    if (new_dir == 0)
        return NULL;

    return PyLong_FromLong(aff_node_size(new_dir));
}

static void
paff_getent(struct AffNode_s *node, void *arg)
{
    PyObject **result = arg;
    const struct AffSymbol_s *sym;

    if (*result == NULL)
        return;

    sym = aff_node_name(node);
    if (PyList_Append(*result, PyUnicode_FromString(aff_symbol_name(sym)))) {
        Py_DECREF(*result);
        *result = NULL;
    }
}

static PyObject *
paff_reader_ls(PyObject *self, PyObject *args)
{
    paff_Reader *r = (paff_Reader *)self;
    struct AffNode_s *new_dir = paff_reader_newdir(r, args);
    PyObject *result = NULL;
    
    if (new_dir == 0)
        return NULL;

    result = PyList_New(0);
    aff_node_foreach(new_dir, paff_getent, &result);

    return result;
}

static PyObject *
paff_read_void(paff_Reader *r, int size, struct AffNode_s *dir)
{
    return PyList_New(0);
}

static PyObject *
paff_read_char(paff_Reader *r, int size, struct AffNode_s *node)
{
    char *buffer = malloc(size + 1);
    PyObject *result;

    if (buffer == 0)
        return PyErr_NoMemory();
    
    if (aff_node_get_char(r->reader, node, buffer, size)) {
        PyErr_SetString(paff_exception, aff_reader_errstr(r->reader));
        aff_reader_clearerr(r->reader);
        result = NULL;
    } else {
        buffer[size] = 0;
        result = PyUnicode_FromStringAndSize(buffer, size);
    }
    free(buffer);
    return result;
}

static PyObject *
paff_read_int(paff_Reader *r, int size, struct AffNode_s *node)
{
    uint32_t *buffer = malloc(size * sizeof (uint32_t));
    PyObject *result;

    if (buffer == 0)
        return PyErr_NoMemory();

    if (aff_node_get_int(r->reader, node, buffer, size)) {
        PyErr_SetString(paff_exception, aff_reader_errstr(r->reader));
        aff_reader_clearerr(r->reader);
        result = NULL;
    } else {
        int i;
        result = PyList_New(size);
        if (result == 0) {
            PyErr_NoMemory();
            goto end;
        }
        for (i = 0; i < size; i++) {
            PyObject *elem = PyLong_FromLong(buffer[i]);
            if (elem == NULL) {
                PyErr_NoMemory();
                Py_CLEAR(result);
                goto end;
            }
            PyList_SET_ITEM(result, i, elem);
        }
    }
end:
    free(buffer);
    return result;
}

static PyObject *
paff_read_double(paff_Reader *r, int size, struct AffNode_s *node)
{
    double *buffer = malloc(size * sizeof (double));
    PyObject *result;

    if (buffer == 0)
        return PyErr_NoMemory();

    if (aff_node_get_double(r->reader, node, buffer, size)) {
        PyErr_SetString(paff_exception, aff_reader_errstr(r->reader));
        aff_reader_clearerr(r->reader);
        result = NULL;
    } else {
        int i;
        result = PyList_New(size);
        if (result == 0) {
            PyErr_NoMemory();
            goto end;
        }
        for (i = 0; i < size; i++) {
            PyObject *elem = PyFloat_FromDouble(buffer[i]);
            if (elem == NULL) {
                PyErr_NoMemory();
                Py_CLEAR(result);
                goto end;
            }
            PyList_SET_ITEM(result, i, elem);
        }
    }
end:
    free(buffer);
    return result;
}

static PyObject *
paff_read_complex(paff_Reader *r, int size, struct AffNode_s *node)
{
    double _Complex *buffer = malloc(size * sizeof (double _Complex));
    PyObject *result;

    if (buffer == 0)
        return PyErr_NoMemory();

    if (aff_node_get_complex(r->reader, node, buffer, size)) {
        PyErr_SetString(paff_exception, aff_reader_errstr(r->reader));
        aff_reader_clearerr(r->reader);
        result = NULL;
    } else {
        int i;
        result = PyList_New(size);
        if (result == 0) {
            PyErr_NoMemory();
            goto end;
        }
        for (i = 0; i < size; i++) {
            PyObject *elem = PyComplex_FromDoubles(creal(buffer[i]),
                                                   cimag(buffer[i]));
            if (elem == NULL) {
                PyErr_NoMemory();
                Py_CLEAR(result);
                goto end;
            }
            PyList_SET_ITEM(result, i, elem);
        }
    }
end:
    free(buffer);
    return result;
}

static PyObject *
paff_reader_read(PyObject *self, PyObject *args)
{
    paff_Reader *r = (paff_Reader *)self;
    struct AffNode_s *new_dir = paff_reader_newdir(r, args);
    enum AffNodeType_e t;
    int size;

    if (new_dir == 0)
        return NULL;

    size = aff_node_size(new_dir);
    t = aff_node_type(new_dir);
    switch (t) {
    case affNodeVoid:     return paff_read_void(r, size, new_dir);
    case affNodeChar:     return paff_read_char(r, size, new_dir);
    case affNodeInt:      return paff_read_int(r, size, new_dir);
    case affNodeDouble:   return paff_read_double(r, size, new_dir);
    case affNodeComplex:  return paff_read_complex(r, size, new_dir);
    default:
        PyErr_SetString(paff_exception, "Unknown node type");
        return NULL;
    }
}

static PyObject *
paff_node_to_path(struct AffNode_s *dir)
{
    struct AffNode_s *parent = aff_node_parent(dir);
    const struct AffSymbol_s *symbol = aff_node_name(dir);
    PyObject *result;

    if (parent == dir)
        return PyUnicode_FromString("/");

    result = PyUnicode_FromFormat("%s", aff_symbol_name(symbol));
    if (result == 0)
        return NULL;
    for (; parent != dir; dir = parent, parent = aff_node_parent(parent)) {
        symbol = aff_node_name(parent);
        PyObject *p = PyUnicode_FromFormat("%s/%s",
										   aff_symbol_name(symbol),
										   PyObject_REPR(result));
        Py_DECREF(result);
        if (p == 0)
            return NULL;
        result = p;
    }
    return result;
}

static PyObject *
paff_reader_getcwd(PyObject *self, PyObject *args)
{
    paff_Reader *r = (paff_Reader *)self;
    
    if (r->reader == NULL) {
        PyErr_SetString(paff_exception, "closed reader");
        return NULL;
    }
    return paff_node_to_path(r->dir);
}

static PyMethodDef paff_reader_methods[] = {
    {"chdir",  paff_reader_chdir,  METH_VARARGS, "AFF reader change directory"},
    {"check",  paff_reader_check,  METH_NOARGS,  "AFF reader check"},
    {"close",  paff_reader_close,  METH_NOARGS,  "AFF reader close"},
    {"getcwd", paff_reader_getcwd, METH_NOARGS, "AFF reader current directory"},
    {"ls",     paff_reader_ls,     METH_VARARGS, "AFF reader list subkeys"},
    {"name",   paff_reader_name,   METH_NOARGS,  "AFF reader name"},
    {"read",   paff_reader_read,   METH_VARARGS, "AFF reader get data"},
    {"size",   paff_reader_size,   METH_VARARGS, "AFF reader data size"},
    {"type",   paff_reader_type,   METH_VARARGS, "AFF reader data type"},
    {NULL,    NULL,              0,            NULL}
};

static PyTypeObject paff_ReaderType = {
    PyVarObject_HEAD_INIT(NULL, 0)
};

/* writer */
typedef struct {
    PyObject_HEAD
    PyObject            *name;
    struct AffWriter_s  *writer;
    struct AffNode_s    *dir;
} paff_Writer;

static void
paff_writer_dealloc(paff_Writer *self)
{
    Py_XDECREF(self->name);
    if (self->writer) {
        aff_writer_close(self->writer);
        self->writer = NULL;
    }
    Py_TYPE(self)->tp_free((PyObject *)self);
}

static PyObject *
paff_writer_str(paff_Writer *self)
{
    if (self->name == NULL) {
        return PyUnicode_FromFormat("<aff Writer without a name at %p>",
									self);
    } else if (self->writer == NULL) {
        return PyUnicode_FromFormat("<closed aff Writer('%s') at %p>",
									PyObject_REPR(self->name),
									self);
    } else {
        return PyUnicode_FromFormat("<aff Writer('%s') at %p>",
									PyObject_REPR(self->name),
									self);
    }
}

static PyObject *
paff_writer_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    static char *kwlist[] = {"name", NULL};
    paff_Writer *self = (paff_Writer *)type->tp_alloc(type, 0);
    const char *msg;
    
    if (self != NULL) {
        if (!PyArg_ParseTupleAndKeywords(args, kwds, "S", kwlist,
                                         &self->name))
            goto error;

        Py_INCREF(self->name);
        self->writer = aff_writer(PyObject_REPR(self->name));
        msg = aff_writer_errstr(self->writer);
        if (msg) {
            PyErr_SetString(paff_exception, msg);
            aff_writer_close(self->writer);
            self->writer = 0;
            Py_DECREF(self->name);
            self->name = 0;
            goto error;
        }
        self->dir = aff_writer_root(self->writer);
    }
    return (PyObject *)self;
error:
    type->tp_free((PyObject *)self);
    return NULL;
}

static PyObject *
paff_writer_close(PyObject *self, PyObject *args)
{
    paff_Writer *w = (paff_Writer *)self;
    const char *msg;

    if (w->writer) {
        msg = aff_writer_close(w->writer);
        w->writer = 0;
        if (msg) {
            PyErr_SetString(paff_exception, msg);
            return NULL;
        }
    }
    Py_RETURN_NONE;
}

static PyObject *
paff_writer_name(PyObject *self, PyObject *args)
{
    paff_Writer *w = (paff_Writer *)self;
    Py_INCREF(w->name);
    return w->name;
}

static PyObject *
paff_writer_getcwd(PyObject *self, PyObject *args)
{
    paff_Writer *w = (paff_Writer *)self;
    
    return paff_node_to_path(w->dir);
}

static struct AffNode_s *
paff_writer_newdir(paff_Writer *w, PyObject *args, PyObject **extra)
{
    char *path = NULL;
    struct AffNode_s *new_dir;

    if (w->writer == NULL) {
        PyErr_SetString(paff_exception, "closed writer");
        return NULL;
    }
    if (extra == NULL) {
        if (!PyArg_ParseTuple(args, "s", &path))
            return NULL;
    } else {
        if (!PyArg_ParseTuple(args, "sO", &path, extra))
            return NULL;
    }
    
    new_dir = aff_writer_mkpath(w->writer, w->dir, path);
    if (new_dir == NULL) {
        PyErr_SetString(paff_exception, aff_writer_errstr(w->writer));
        aff_writer_clearerr(w->writer);
    }
    return new_dir;
}

static PyObject *
paff_writer_mkdir(PyObject *self, PyObject *args)
{
    paff_Writer *w = (paff_Writer *)self;
    struct AffNode_s *new_dir = paff_writer_newdir(w, args, NULL);

    if (new_dir == 0)
        return NULL;
    w->dir = new_dir;
    Py_RETURN_NONE;
}

static PyObject *
paff_writer_type(PyObject *self, PyObject *args)
{
    paff_Writer *w = (paff_Writer *)self;
    struct AffNode_s *new_dir = paff_writer_newdir(w, args, NULL);

    return paff_to_python_type(new_dir);
}

static PyObject *
paff_writer_size(PyObject *self, PyObject *args)
{
    paff_Writer *w = (paff_Writer *)self;
    struct AffNode_s *new_dir = paff_writer_newdir(w, args, NULL);

    if (new_dir == 0)
        return NULL;

    return PyLong_FromLong(aff_node_size(new_dir));
}

static PyObject *
paff_writer_ls(PyObject *self, PyObject *args)
{
    paff_Writer *w = (paff_Writer *)self;
    struct AffNode_s *new_dir = paff_writer_newdir(w, args, NULL);
    PyObject *result = NULL;

    if (new_dir == 0)
        return NULL;

    result = PyList_New(0);
    aff_node_foreach(new_dir, paff_getent, &result);

    return result;
}

static PyObject *
paff_write_void(struct AffNode_s *dir)
{
    Py_RETURN_NONE;
}

static PyObject *
paff_write_char(paff_Writer *w, struct AffNode_s *dir,
                int size, PyObject *data)
{
    const char *str = PyObject_REPR(data);

    if (aff_node_put_char(w->writer, dir, str, size)) {
        PyErr_SetString(paff_exception, aff_writer_errstr(w->writer));
        aff_writer_clearerr(w->writer);
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject *
paff_write_int(paff_Writer *w, struct AffNode_s *dir,
               int size, PyObject *data)
{
    uint32_t *buffer = malloc(size * sizeof (uint32_t));
    int i;

    if (buffer == NULL)
        return PyErr_NoMemory();
    
    for (i = 0; i < size; i++) {
        PyObject *elem = PyList_GET_ITEM(data, i);

        if (Py_TYPE(elem) != &PyLong_Type)
            goto bad_data_error;
        buffer[i] = PyLong_AsLong(elem);
    }
    if (aff_node_put_int(w->writer, dir, buffer, size)) {
        PyErr_SetString(paff_exception, aff_writer_errstr(w->writer));
        aff_writer_clearerr(w->writer);
        goto error;
    }
    free(buffer);
    Py_RETURN_NONE;

bad_data_error:
    PyErr_SetString(paff_exception, "List of int expected");
error:
    free(buffer);
    return NULL;
}

static PyObject *
paff_write_double(paff_Writer *w, struct AffNode_s *dir,
                  int size, PyObject *data)
{
    double *buffer = malloc(size * sizeof (double));
    int i;

    if (buffer == NULL)
        return PyErr_NoMemory();
    
    for (i = 0; i < size; i++) {
        PyObject *elem = PyList_GET_ITEM(data, i);

        if (Py_TYPE(elem) != &PyFloat_Type)
            goto bad_data_error;
        buffer[i] = PyFloat_AsDouble(elem);
    }
    if (aff_node_put_double(w->writer, dir, buffer, size)) {
        PyErr_SetString(paff_exception, aff_writer_errstr(w->writer));
        aff_writer_clearerr(w->writer);
        goto error;
    }
    free(buffer);
    Py_RETURN_NONE;

bad_data_error:
    PyErr_SetString(paff_exception, "List of float expected");
error:
    free(buffer);
    return NULL;
}

static PyObject *
paff_write_complex(paff_Writer *w, struct AffNode_s *dir,
                  int size, PyObject *data)
{
    double _Complex *buffer = malloc(size * sizeof (double _Complex));
    int i;

    if (buffer == NULL)
        return PyErr_NoMemory();
    
    for (i = 0; i < size; i++) {
        PyObject *elem = PyList_GET_ITEM(data, i);

        if (Py_TYPE(elem) != &PyComplex_Type)
            goto bad_data_error;
        buffer[i] = PyComplex_RealAsDouble(elem)
                     + I * PyComplex_ImagAsDouble(elem);
    }
    if (aff_node_put_complex(w->writer, dir, buffer, size)) {
        PyErr_SetString(paff_exception, aff_writer_errstr(w->writer));
        aff_writer_clearerr(w->writer);
        goto error;
    }
    free(buffer);
    Py_RETURN_NONE;

bad_data_error:
    PyErr_SetString(paff_exception, "List of float expected");
error:
    free(buffer);
    return NULL;
}

static PyObject *
paff_writer_write(PyObject *self, PyObject *args)
{
    paff_Writer *w = (paff_Writer *)self;
    PyObject *data;
    struct AffNode_s *new_dir = paff_writer_newdir(w, args, &data);
    int size;
    PyObject *elem;
    
    if (new_dir == NULL)
        return NULL;

    size = PyList_GET_SIZE(data);
    if (Py_TYPE(data) == &PyUnicode_Type)
        return paff_write_char(w, new_dir, size, data);

    if (Py_TYPE(data) != &PyList_Type) {
        PyErr_SetString(paff_exception, "data is not a list");
        return NULL;
    }
    if (size == 0)
        return paff_write_void(new_dir);
    elem = PyList_GET_ITEM(data, 0);
    if (Py_TYPE(elem) == &PyLong_Type)
        return paff_write_int(w, new_dir, size, data);
    if (Py_TYPE(elem) == &PyFloat_Type)
        return paff_write_double(w, new_dir, size, data);
    if (Py_TYPE(elem) == &PyComplex_Type)
        return paff_write_complex(w, new_dir, size, data);

    PyErr_SetString(paff_exception, "Unsupported data type");
    return NULL;
}

static PyMethodDef paff_writer_methods[] = {
    {"chdir",  paff_writer_mkdir,  METH_VARARGS, "AFF writer make directory"},
    {"close",  paff_writer_close,  METH_NOARGS, "AFF_write close"},
    {"getcwd", paff_writer_getcwd, METH_NOARGS, "AFF writer current directory"},
    {"ls",     paff_writer_ls,     METH_VARARGS, "AFF writer list subkeys"},
    {"name",   paff_writer_name,   METH_NOARGS,  "AFF writer name"},
    {"size",   paff_writer_size,   METH_VARARGS, "AFF writer data size"},
    {"type",   paff_writer_type,   METH_VARARGS, "AFF writer data type"},
    {"write",  paff_writer_write,  METH_VARARGS, "AFF writer put data"},
    {NULL,    NULL,              0,            NULL}
};

static PyTypeObject paff_WriterType = {
    PyVarObject_HEAD_INIT(NULL, 0)
};
/* module */

static PyObject *
paff_version(PyObject *self, PyObject *args)
{
    static PyObject *version = NULL;

    if (!version) {
        version = Py_BuildValue("s", aff_version());
        if (version == NULL)
            return NULL;
    }
    Py_XINCREF(version);
    return version;
}

static PyMethodDef paff_methods[] = {
    {"version", paff_version, METH_NOARGS, "AFF version"},
    { NULL, NULL, 0, NULL }
};


static struct PyModuleDef affmodule = {
   PyModuleDef_HEAD_INIT,
   "aff",
   "AFF interface.", 
   -1,
   paff_methods
};

PyMODINIT_FUNC
PyInit_aff(void) 
{
    static char *exception_name = "aff.Exception";
    PyObject* m;

    m = PyModule_Create(&affmodule);
    if (m == NULL)
        return NULL;

    paff_ReaderType.tp_name = "aff.Reader";
    paff_ReaderType.tp_basicsize = sizeof (paff_Reader);
    paff_ReaderType.tp_dealloc = (destructor)paff_reader_dealloc;
    paff_ReaderType.tp_str = (reprfunc)paff_reader_str;
    paff_ReaderType.tp_repr = (reprfunc)paff_reader_str;
    paff_ReaderType.tp_flags = Py_TPFLAGS_DEFAULT;
    paff_ReaderType.tp_doc = "AFF Reader objects";
    paff_ReaderType.tp_methods = paff_reader_methods;
    paff_ReaderType.tp_new = paff_reader_new;
    if (PyType_Ready(&paff_ReaderType) < 0)
        return NULL;

    Py_INCREF(&paff_ReaderType);
    if (PyModule_AddObject(m,
                           paff_ReaderType.tp_name + 4,
                           (PyObject *)&paff_ReaderType))
        return NULL;

    paff_WriterType.tp_name = "aff.Writer";
    paff_WriterType.tp_basicsize = sizeof (paff_Writer);
    paff_WriterType.tp_dealloc = (destructor)paff_writer_dealloc;
    paff_WriterType.tp_str = (reprfunc)paff_writer_str;
    paff_WriterType.tp_repr = (reprfunc)paff_writer_str;
    paff_WriterType.tp_flags = Py_TPFLAGS_DEFAULT;
    paff_WriterType.tp_doc = "AFF Writer objects";
    paff_WriterType.tp_methods = paff_writer_methods;
    paff_WriterType.tp_new = paff_writer_new;
    if (PyType_Ready(&paff_WriterType) < 0)
        return NULL;

    Py_INCREF(&paff_WriterType);
    if (PyModule_AddObject(m,
                           paff_WriterType.tp_name + 4,
                           (PyObject *)&paff_WriterType))
        return NULL;

    paff_exception = PyErr_NewException(exception_name, NULL, NULL);
    if (paff_exception == NULL)
        return NULL;

    Py_INCREF(paff_exception);
    if (PyModule_AddObject(m,
                           exception_name + 4,
                           (PyObject *)paff_exception))
        return NULL;

    return m;
}
