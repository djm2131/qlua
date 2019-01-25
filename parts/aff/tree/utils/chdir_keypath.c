/*! $Id: .vimrc,v 1.4 2007/03/22 18:26:06 syritsyn Exp $
 **************************************************************************
 * \file chdir_keypath.c
 *      change(make) key directory in AffReader(AffWriter)
 *
 * \author Sergey N. Syritsyn
 * 
 * \date Created: 28/08/2007
 *    2008/06/09 avp -- changes for 
 *                 mkdir_path() --> aff_writer_mkpath()
 *                 chdir_path() --> aff_reader_chpath()
 *
 ***************************************************************************/
#include <string.h>
#include <stdlib.h>
#include "lhpc-aff.h"
#include <aff-i.h>

#include "common.h"

static
char *my_strsep( char *str, char **end, char delim )
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

struct AffNode_s *lookup_path(struct AffReader_s *r, 
        struct AffNode_s *r_node, const char *key_path )
{
    if( NULL == r ||
        NULL == key_path )
        return NULL;
    char *kpath = malloc( strlen( key_path ) + 1 );
    char *s, *end = kpath;

    if (kpath == NULL)
        return NULL;

    strcpy( kpath, key_path );
    if( '/' == *end )
        r_node = aff_reader_root( r );
    if( NULL == r_node ) {
        free(kpath);
        return NULL;
    }
    
    while( NULL != ( s = my_strsep( end, &end, '/' ) ) &&
           r_node != NULL )
    {
        if( '\0' == *s )
            continue;
        /* FIXME replace with a lookup interface function, 
           if (as early as there is) any */
        r_node = aff_node_chdir(r->tree, r->stable, r_node, 0, s);
    }
    free( kpath );
    return r_node;
}
