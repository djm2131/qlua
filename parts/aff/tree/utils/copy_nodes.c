/*! $Id: .vimrc,v 1.4 2007/03/22 18:26:06 syritsyn Exp $
 **************************************************************************
 * \file copy_nodes.c
 *      Copy nodes recursively using `foreach' interface of AFF tree
 *
 * \author Sergey N. Syritsyn
 * 
 * \date Created: 28/08/2007
 *
 ***************************************************************************/
#include "lhpc-aff.h"
#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include "common.h"

const char *copy_node( struct AffReader_s *r, struct AffNode_s *r_node, 
        struct AffWriter_s *w, struct AffNode_s *w_parent, 
        struct AffNode_s **w_new, int weak )
{
    if( NULL == r_node ||
        NULL == r || 
        NULL == w )
        return "copy_nodes: null argument";
    
    struct AffNode_s *w_node;
    if( NULL == w_parent )
        w_node = aff_writer_root( w );
    else
    {
        const char *sname = aff_symbol_name( aff_node_name( r_node ) );
        if( NULL == sname )
            return "copy_nodes: cannot get key name";
        w_node = aff_writer_mkdir( w, w_parent, sname );
    }
    if( NULL == w_node )
        return aff_writer_errstr( w );
    if( NULL != w_new )
        *w_new = w_node;
    return copy_node_data( r, r_node, w, w_node, weak );
}

/* 
   if weak==0, return error on existing non-void node;
   if weak==1, ..., do not return error, print warning;
   otherwise, ..., return no error
 */
const char *copy_node_data( struct AffReader_s *r, struct AffNode_s *r_node, 
        struct AffWriter_s *w, struct AffNode_s *w_node, int weak )
{
    size_t size = aff_node_size( r_node );
    switch( aff_node_type( w_node ) )
    {
    case affNodeVoid: break;
    case affNodeInvalid:    
        return "copy_node_data: aff_node_type returned AffNodeInvalid";
    default:
        switch( weak )
        {
        case COPY_NODE_STRONG:
            return "copy_node_data: node has data already";
        case COPY_NODE_WEAK:
            fprintf( stderr, "%s: data change in node ", __func__ );
            fprint_path( stderr, aff_writer_root( w ), w_node );
            fprintf( stderr, "\n" );
            return NULL;
        default:
            return NULL;
        }
    }
    switch( aff_node_type( r_node ) )
    {
    case affNodeInvalid: return "copy_node_data: aff_node_type returned AffNodeInvalid";
    case affNodeVoid: break;
    case affNodeChar: {
            char *ptr = (char *)malloc( size );
            if( NULL == ptr )
                return "copy_node_data: not enough memory";
            if( aff_node_get_char( r, r_node, ptr, size ) ) {
		free(ptr);
                return aff_reader_errstr( r );
	    }
            if( aff_node_put_char( w, w_node, ptr, size ) ) {
		free(ptr);
                return aff_writer_errstr( w );
	    }
            free( ptr );
        } break;
    case affNodeInt: {
            uint32_t *ptr = (uint32_t *)malloc( size * sizeof(uint32_t) );
            if( NULL == ptr )
                return "copy_node_data: not enough memory";
            if( aff_node_get_int( r, r_node, ptr, size ) ) {
		free(ptr);
                return aff_reader_errstr( r );
	    }
            if( aff_node_put_int( w, w_node, ptr, size ) ) {
		free(ptr);
                return aff_writer_errstr( w );
	    }
            free( ptr );
        } break;
    case affNodeDouble: {
            double *ptr = (double *)malloc( size * sizeof(double) );
            if( NULL == ptr )
                return "copy_node_data: not enough memory";
            if( aff_node_get_double( r, r_node, ptr, size ) ) {
		free(ptr);
                return aff_reader_errstr( r );
	    }
            if( aff_node_put_double( w, w_node, ptr, size ) ) {
		free(ptr);
                return aff_writer_errstr( w );
	    }
            free( ptr );
        } break;
    case affNodeComplex: {
            double _Complex *ptr = (double _Complex *)
                malloc( size * sizeof(double _Complex) );
            if( NULL == ptr )
                return "copy_node_data: not enough memory";
            if( aff_node_get_complex( r, r_node, ptr, size ) ) {
		free(ptr);
                return aff_reader_errstr( r );
	    }
            if( aff_node_put_complex( w, w_node, ptr, size ) ) {
		free(ptr);
                return aff_writer_errstr( w );
	    }
            free( ptr );
        } break;
    }
    return NULL;
}

void copy_nodes_recursive( struct AffNode_s *r_node, void *arg_ )
{
    struct copy_nodes_arg *arg = (struct copy_nodes_arg *)arg_;
    if( NULL != arg->errstr )
        return;
    
    struct AffNode_s *w_node;
    const char *status = copy_node( arg->r, r_node, arg->w, 
            arg->w_parent, &w_node, arg->weak );
    if( NULL != status )
    {
        arg->errstr = status;
        return;
    }
    if( NULL == w_node )
    {
        arg->errstr = "copy_nodes_recursive: NULL node";
        return;
    }
    
    struct copy_nodes_arg new_arg;
    new_arg.r = arg->r;
    new_arg.w = arg->w;
    new_arg.w_parent = w_node;
    new_arg.weak = arg->weak;
    new_arg.errstr = NULL;
    
    aff_node_foreach( r_node, copy_nodes_recursive, (void *)&new_arg );
    arg->errstr = new_arg.errstr;
}
