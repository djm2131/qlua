#ifndef MARK_8f2f88f3_916a_4206_9245_2a8f45a9768c
#define MARK_8f2f88f3_916a_4206_9245_2a8f45a9768c
#include "treap.h"

/* Definition of the hash table entry */
struct Node_s {
    const void    *key;      /* malloced, key is always copied. */
    int            key_size; /* size of the key in bytes */
    void          *value;    /* generic value */
    uint32_t       hash;     /* internal balancing value */
    struct Node_s *left;     /* left subtree */
    struct Node_s *right;    /* right subtree */
};

/* Definition of the hash table proper */
struct AffTreap_s {
    uint32_t       state;   /* random state to make hash thread safe */
    struct Node_s *root;    /* the root of the hash */
};

enum {
    RSTEP = 0x9e3779b9u, /* a random constant */
    RINIT = 314159265    /* the initial state of the random sequence */
};

#endif /* !defined(MARK_8f2f88f3_916a_4206_9245_2a8f45a9768c) */

