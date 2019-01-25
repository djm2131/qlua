/*! $Id: .vimrc,v 1.4 2007/03/22 18:26:06 syritsyn Exp $
 **************************************************************************
 * \file import.c
 *
 * \author Sergey N. Syritsyn
 * 
 * \date Created: 23/08/2007
 *    2008/06/09 avp -- changes for 
 *                 mkdir_path() --> aff_writer_mkpath()
 *                 chdir_path() --> aff_reader_chpath()
 *
 ***************************************************************************/
#include <assert.h>
#include <unistd.h>
#include <errno.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <string.h>
#include "lhpc-aff.h"
#include "util.h"
#include "common.h"

static
int read_nonspace( char *buf, size_t max, FILE *fi )
{
    if( 0 == max )
        return 0;
    unsigned char c;
    while( isspace( c = fgetc( fi ) ) )
    { 
        if( feof( fi ) )
        {
            *buf = '\0';
            return 0;
        }
    }
    *(buf++) = c;
    size_t k = max;
    while( --k && !feof( fi ) )
    {
        if( isspace( c = fgetc( fi ) ) )
        {
            ungetc( c, fi );
            break;
        }
        *(buf++) = c;
    }
    *buf = '\0';
    return max - k ;
}

static
int read_char_list( char *buf, size_t max, FILE *fi )
{
    return fread( buf, 1, max, fi );
}

static
int read_int_list( uint32_t *buf, size_t max, FILE *fi )
{
    size_t cnt = max;
    while( cnt-- )
    {
        long int a;
        if( fscanf( fi, "%ld", &a ) < 1 )
            return cnt - max - 1;
        *(buf++) = a;
    }
    return max - cnt - 1;
}

static
int read_double_list( double *buf, size_t max, FILE *fi )
{
    size_t cnt = max;
    char strbuf[1024], *ptr;
    while( cnt-- )
    {
        if( read_nonspace( strbuf, sizeof(strbuf), fi ) <= 0 )
            return max - cnt - 1;
        *(buf++) = strtod( strbuf, &ptr );
        if( ptr == strbuf )
        {
            fprintf( stderr, "%s: cannot read as double: \"%s\"\n",
                    __func__, ptr );
            return max - cnt - 1;
        }
        if( '\0' != *ptr )
            fprintf( stderr, "%s: trailing characters after double: \"%s\"\n",
                    __func__, ptr );
    }
    return max - cnt - 1;
}

static
int read_complex_list( double _Complex *buf, size_t max, FILE *fi )
{
    size_t cnt = max;
    char strbuf[1024], *ptr;
    while( cnt-- )
    {
        double rv, iv;

        if( read_nonspace( strbuf, sizeof(strbuf), fi ) <= 0 )
            return max - cnt - 1;
        rv = strtod( strbuf, &ptr );
        if( ptr == strbuf )
        {
            fprintf( stderr, "%s: cannot read as double: \"%s\"\n",
                    __func__, ptr );
            return max - cnt - 1;
        }
        if( '\0' != *ptr )
            fprintf( stderr, "%s: trailing characters after double: \"%s\"\n",
                    __func__, ptr );

        if( read_nonspace( strbuf, sizeof(strbuf), fi ) <= 0 )
        {
            fprintf( stderr, "%s: unpaired real part of a complex number\n", __func__ );
            *buf = rv + I * 0.;
            return max - cnt;
        }
        iv = strtod( strbuf, &ptr );
        if( ptr == strbuf )
        {
            fprintf( stderr, "%s: cannot read as double: \"%s\"\n",
                    __func__, ptr );
            *buf = rv + I * 0.;
            return max - cnt ;
        }
        if( '\0' != *ptr )
            fprintf( stderr, "%s: trailing characters after double: \"%s\"\n",
                    __func__, ptr );
        *(buf++) = rv + I * iv;
    }
    return max - cnt - 1;
}


static
const char *insert_list( struct AffWriter_s *w, struct AffNode_s *w_node,
        enum AffNodeType_e type, size_t size )
{
    if( NULL == w ||
        NULL == w_node )
        return "insert_list: invalid writer or node";
    int status;
    switch( type )
    {
    case affNodeInvalid:    
        return "insert_list: invalid node type";
    case affNodeVoid:
        break;
    case affNodeChar: 
        {
            char *buf = malloc( size );
            if( NULL == buf )
                return "insert_list: not enough memory\n";
            read_char_list( buf, size, stdin );
            status = aff_node_put_char( w, w_node, buf, size );
            free( buf );
            if( status )
                return aff_writer_errstr( w );
        } break;
    case affNodeInt:
        {
            uint32_t *buf = (uint32_t *)malloc( size * sizeof(uint32_t) );
            if( NULL == buf )
                return "insert_list: not enough memory\n";
            read_int_list( buf, size, stdin );
            status = aff_node_put_int( w, w_node, buf, size );
            free( buf );
            if( status )
                return aff_writer_errstr( w );
        } break;
    case affNodeDouble:
        {
            double *buf = (double *)malloc( size * sizeof(double) );
            if( NULL == buf )
                return "insert_list: not enough memory\n";
            read_double_list( buf, size, stdin );
            status = aff_node_put_double( w, w_node, buf, size );
            free( buf );
            if( status )
                return aff_writer_errstr( w );
        } break;
    case affNodeComplex:
        {
            double _Complex *buf = (double _Complex *)
                malloc( size * sizeof(double _Complex) );
            if( NULL == buf )
                return "insert_list: not enough memory\n";
            read_complex_list( buf, size, stdin );
            status = aff_node_put_complex( w, w_node, buf, size );
            free( buf );
            if( status )
                return aff_writer_errstr( w );
        } break;
    }
    return NULL;
}


int x_import( int argc, char *argv[] )
{
    int start_empty = 0;
    const char *status;
    const char *fname = NULL;
    const char *out_fname = NULL;
    enum AffNodeType_e type = affNodeVoid;
    long int num = 1;

    if( argc < 2 )
    {
        h_import();
        return 1;
    }

    for( ; argc ; argv++, argc-- )
    {
        if( '-' != argv[0][0] )
            break;
        char *p, *q;
        for( p = argv[0] + 1 ; '\0' != *p ; ++p )
        {
            switch(*p)
            {
            case 'c': type = affNodeChar      ; break;
            case 'i': type = affNodeInt       ; break;
            case 'd': type = affNodeDouble    ; break;
            case 'x': type = affNodeComplex   ; break;
            case 'v': type = affNodeVoid      ; break;
            case 'N': 
                {
                    if( '\0' != *(p+1) || !(--argc) )
                    {
                        fprintf( stderr, "%s: -N must be followed by a number\n", __func__ );
                        return 1;
                    }
                    num = strtol( *(++argv), &q, 10 );
                    if( q == *argv || '\0' != *q )
                    {
                        fprintf( stderr, "%s: -N must be followed by a number\n", __func__ );
                        return 1;
                    }
                } break;
            case 'o': 
                {
                    if( '\0' != *(p+1) || !(--argc) )
                    {
                        fprintf( stderr, "%s: -o must be followed by an output file name\n", 
                                __func__ );
                        return 1;
                    }
                    out_fname = *(++argv);
                } break;
            case 'e': start_empty = 1; break;
            default:
                {
                    fprintf( stderr, "%s: unknown parameter -%c\n", 
                                __func__, *p );
                    return 1;
                }
            }
        }
    }
    
    if( !start_empty )
    {
        if( !(argc--) )
        {
            fprintf( stderr, "%s: no aff file name given; try 'lhpc-aff help import'\n", __func__ );
            return 1;
        }
        else
            fname = *(argv++);
        if( NULL == out_fname )
            out_fname = fname;
    }
    else
        if( NULL == out_fname )
        {
            fprintf( stderr, "%s: for empty start aff file, output file name must be specified; " 
                    "try 'lhpc-aff help import'\n", __func__ );
            return 1;
        }

    const char *key_path = NULL;
    if( !(argc--) )
    {
        fprintf( stderr, "%s: no keypath given; try 'lhpc-aff help import'\n", __func__ );
        return 1;
    }
    else
        key_path = *(argv++);
    
    char *tmp_fname;
    if( NULL == ( tmp_fname = mk_tmp_filename( ".aff-tmp.", out_fname ) ) )
    {
        fprintf( stderr, "%s: cannot create a unique writable file\n", __func__ );
        perror( __func__ );
        return 1;
    }
    
    struct AffWriter_s *w = aff_writer( tmp_fname );
    if( NULL != ( status = aff_writer_errstr( w ) ) )
    {
        fprintf( stderr, "%s: %s: %s\n", __func__, tmp_fname, aff_writer_errstr( w ) );
        goto err_clean_w;
    }
    struct AffNode_s *w_root = aff_writer_root( w );
    if( NULL == w_root )
    {
        fprintf( stderr, "%s: %s\n", __func__, aff_writer_errstr( w ) );
        goto err_clean_w;
    }
    struct AffNode_s *w_node = aff_writer_mkpath(w, w_root, key_path);
    if( NULL == w_node )
    {
        fprintf( stderr, "%s: %s: %s\n", __func__, key_path, aff_writer_errstr( w ) );
        goto err_clean_w;
    }
    if( affNodeVoid != type && aff_writer_root( w ) == w_node )
    {
        fprintf( stderr, "%s: cannot put data to the root node\n", __func__ );
        goto err_clean_w;
    }
    if( NULL != ( status = insert_list( w, w_node, type, num ) ) )
    {
        fprintf( stderr, "%s: %s\n", __func__, status );
        goto err_clean_w;
    }
    struct AffReader_s *r;
    if( !start_empty )
    {
        r = aff_reader( fname );
        if( NULL != ( status = aff_reader_errstr( r ) ) )
        {        
            fprintf( stderr, "%s: %s: %s\n", __func__, fname, aff_reader_errstr( r ) );
            goto err_clean_rw;
        }
        struct AffNode_s *r_root = aff_reader_root( r );
        if( NULL == r_root )
        {
            fprintf( stderr, "%s: %s\n", __func__, aff_reader_errstr( r ) );
            goto err_clean_rw;
        }
        // copy all nodes
        struct copy_nodes_arg arg;
        arg.r = r;
        arg.w = w;
        arg.w_parent = w_root;
        arg.weak = COPY_NODE_WEAK;
        arg.errstr = NULL;
        aff_node_foreach( r_root, copy_nodes_recursive, (void *)&arg );
        if( NULL != arg.errstr )
        {
            fprintf( stderr, "%s: %s\n", __func__, arg.errstr );
            goto err_clean_rw;
        }
        aff_reader_close( r );
    }
    if( NULL != ( status = aff_writer_close( w ) ) )
    {
        fprintf( stderr, "%s: %s\n", __func__, status );
        if( remove( tmp_fname ) )
            perror( __func__ );
        goto err_clean_tfn;
    }
    if( rename( tmp_fname, out_fname ) )
    {
        perror( __func__ );
        fprintf( stderr, "%s: output is saved to %s\n", __func__, tmp_fname );
        goto err_clean_tfn;
    }
    free( tmp_fname );
    return 0;
    
err_clean_rw:
    aff_reader_close( r ); 
err_clean_w:
    aff_writer_close( w ); 
    if( remove( tmp_fname ) )
        perror( __func__ );
err_clean_tfn:
    free( tmp_fname );
    return 1;
}

void h_import(void)
{
    printf( "Usage:\nlhpc-aff import -[cidx] [-N <N>] [-o <output>] {-e|<aff-file>}\n"
            "\t\t<key-path>\n"
            "Import whitespace-separated data or a string from standard input\n"
            "to <aff-file> under <key-path>. If no output file name given, rewrite \n"
            "the original file. New data replaces the old data.\n"
            "\t-v\tvoid input; useful to create a new empty aff-file\n"
            "\t-c\tchar array input\n"  
            "\t-i\tinteger array input\n"
            "\t-d\tdouble precision real number array input\n"
            "\t-x\tdouble precision complex number array input\n"
            "\t-N <N>\tarray of length N (default N=1)\n"
            "\t-o <output>\n\t\twrite result of import to <output>\n"
            "\t-e\tstart with an empty aff file\n"
            );
}
