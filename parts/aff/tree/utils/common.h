/*! $Id: .vimrc,v 1.4 2007/03/22 18:26:06 syritsyn Exp $
 **************************************************************************
 * \file common.h
 *
 * \author Sergey N. Syritsyn
 * 
 * \date Created: 28/08/2007
 *
 ***************************************************************************/
#ifndef UTIL_COMMON_H_
#define UTIL_COMMON_H_
#include <stdio.h>
#include "lhpc-aff.h"
/* implemented in chdir_keypath.c */
struct AffNode_s *lookup_path(struct AffReader_s *r,
               struct AffNode_s *r_node, const char *key_path );

/* implemented in copy_nodes.c */
const char *copy_node( struct AffReader_s *r, struct AffNode_s *r_node,
                struct AffWriter_s *w, struct AffNode_s *w_parent,
                        struct AffNode_s **w_new, int weak );
const char *copy_node_data( struct AffReader_s *r, struct AffNode_s *r_node, 
        struct AffWriter_s *w, struct AffNode_s *w_node, int weak );
struct copy_nodes_arg
{
    struct AffReader_s  *r;
    struct AffWriter_s  *w;
    struct AffNode_s    *w_parent;
    const char          *errstr;
    int                 weak;
};
#define COPY_NODE_STRONG    0
#define COPY_NODE_WEAK      1
#define COPY_NODE_SILENT    2
void copy_nodes_recursive( struct AffNode_s *r_node, void *arg_ );

/* implemented in common.c */
void print_path( struct AffNode_s *root, struct AffNode_s *node );
void fprint_path( FILE *stream, struct AffNode_s *root, struct AffNode_s *node );
char *mk_tmp_filename( const char *mark, const char *fname );
int split_farg( char *buf, int fargc, char ** fargv );
#endif/*UTIL_COMMON_H_*/
