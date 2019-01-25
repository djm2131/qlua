/*! $Id: .vimrc,v 1.4 2007/03/22 18:26:06 syritsyn Exp $
 **************************************************************************
 * \file insert.c
 *      Insert one aff file into another
 *
 * \author Sergey N. Syritsyn
 * 
 * \date Created: 28/08/2007
 *    2008/06/09 avp -- changes for 
 *                 mkdir_path() --> aff_writer_mkpath()
 *                 chdir_path() --> aff_reader_chpath()
 *
 ***************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "lhpc-aff.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "util.h"
#include "common.h"

static int ignore_missing = 0;

// XXX root node data is not copied!
int insert_data( struct AffWriter_s *w, 
        const char *src_fname, const char *src_kpath, const char *dst_kpath )
{
    struct AffNode_s *w_root = aff_writer_root( w );
    const char *status = NULL;
    if( NULL == w_root )
    {
        fprintf(stderr, "%s: %s\n", __func__, aff_writer_errstr( w ) );
        return 1;
    }
    struct AffNode_s *w_node = aff_writer_mkpath(w, aff_writer_root(w),
                                                 dst_kpath);
    if( NULL == w_node )
    {
        fprintf(stderr, "%s: [%s]: %s\n", __func__, dst_kpath,
                aff_writer_errstr( w ) );
        return 1;
    }
    struct stat stat_fb;
    if (stat(src_fname, &stat_fb))
    {
        perror(src_fname);
        return !ignore_missing;
    }
    struct AffReader_s *r;
    if (NULL == (r = aff_reader( src_fname )))
    {
        fprintf(stderr, "%s: not enough memory\n", __func__);
        return 1;
    }
    if(NULL != (status = aff_reader_errstr(r)))
    {
        fprintf(stderr, "%s: %s: %s\n", __func__, src_fname, status);
        goto errclean_r;
    }
    struct AffNode_s *r_root = aff_reader_root( r );
    if( NULL == r_root )
    {
        fprintf(stderr, "%s: %s: %s\n", __func__, 
                src_fname, aff_reader_errstr(r));
        goto errclean_r;
    }
    struct AffNode_s *r_node = lookup_path( r, r_root, src_kpath );
    if (NULL == r_node) {
        fprintf(stderr, "%s: %s[%s]: cannot read node\n", __func__, 
                src_fname, src_kpath);
        if (ignore_missing)
            goto clean_r;
        else
            goto errclean_r;
    }
    
    struct copy_nodes_arg arg;
    arg.r   = r;
    arg.w   = w;
    arg.w_parent = w_node;
    arg.weak = COPY_NODE_WEAK;
    arg.errstr = NULL;
    aff_node_foreach( r_node, copy_nodes_recursive, &arg );
    if( NULL != arg.errstr )
    {
        fprintf(stderr, "%s: [%s] <- %s[%s]: %s\n", __func__, dst_kpath,
                src_fname, src_kpath, arg.errstr);
        return 1;
    }
clean_r:
    aff_reader_close( r );
    return 0;
    
errclean_r:
    aff_reader_close(r);
    return 1;
}

int insert_keylist( struct AffWriter_s *w, const char *list_fname )
{
    char buf[49152];
    char *fargv[3];
    FILE *list;
    int num;
    if( 0 == strcmp( list_fname, "-" ) )
    {
        list = stdin;
        if( ferror( list ) )
        {
            fprintf( stderr, "%s: bad stdin stream\n", __func__ );
            return 1;
        }
    }
    else
    {
        if( NULL == ( list = fopen( list_fname, "r" ) ) )
        {
            fprintf( stderr, "%s: cannot open %s\n", __func__, list_fname );
            return 1;
        }
    }
    while( NULL != fgets( buf, sizeof(buf), list ) )
    {
        if( '\n' != buf[strlen(buf)-1] )
        {
            fprintf( stderr, "%s: line too long, skipping\n", __func__ );
            while( NULL != fgets( buf, sizeof(buf), list ) )
                if( '\n' == buf[strlen(buf)-1] )
                    break;
            continue;
        }
        num = split_farg( buf, 3, fargv );
        if( num < 0 )
        {
            fprintf( stderr, "%s: unexpected result of split_farg; exiting\n",
                    __func__ );
            goto errclean_r;
        }
        if( num == 0 )
            continue;
        if( num < 3 )
        {
            fprintf( stderr, "%s: syntax error: need 3 names, only %d given\n",
                     __func__, num );
            goto errclean_r;
        }
        if (insert_data(w, fargv[1], fargv[2], fargv[0]))
            goto errclean_r;
    }
    fclose( list );
    return 0;

errclean_r:
    fclose( list );
    return 1;
}

int x_insert( int argc, char *argv[] )
{
    const char *status = NULL,
               *out_fname = NULL,
               *list1_fname = NULL,
               *list2_fname = NULL;
    
    for( ; argc ; --argc, ++argv )
    {
        if( '-' != argv[0][0] ) 
            break;
        for( char *p = argv[0] + 1 ; '\0' != *p ; ++p )
        {
            switch( *p )
            {
            case 'o':
                {
                    if( '\0' != *(p+1) || !(--argc) )
                    {
                        fprintf( stderr, "%s: -o must be followed by a file name\n", __func__ );
                        return 1;
                    }
                    out_fname = *(++argv);
                } break;
            case 'i': ignore_missing = 1; break;
            case 'f':
                {
                    if( '\0' != *(p+1) || !(--argc) )
                    {
                        fprintf( stderr, "%s: -f must be followed by a file name\n", __func__ );
                        return 1;
                    }
                    list1_fname = *(++argv);
                } break;
            case 'F':
                {
                    if( '\0' != *(p+1) || !(--argc) )
                    {
                        fprintf( stderr, "%s: -F must be followed by a file name\n", __func__ );
                        return 1;
                    }
                    list2_fname = *(++argv);
                } break;
            default:
                {
                    fprintf( stderr, "%s: unknown parameter -%c\n", 
                                __func__, *p );
                    return 1;
                }
            }
        }
    }
    if( NULL != list1_fname && NULL != list2_fname &&
        0 == strcmp( list1_fname, "-" ) && 0 == strcmp( list2_fname, "-" ) )
    {
        fprintf( stderr, "%s: both pre- and post-list cannot be '-'(stdin)\n",
                 __func__ );
        return 1;
    }
    if( NULL == out_fname )
    {
        fprintf( stderr, "%s: output file name must be specified; try 'lhpc-aff help insert'\n", 
                __func__ );
        return 1;
    }
    if( argc % 3 )
    {
        fprintf( stderr, "%s: unpaired aff-file and keypath; "
                 "try 'lhpc-aff help insert'\n", __func__ );
        return 1;
    }
    char *tmp_fname;
    if( NULL == ( tmp_fname = mk_tmp_filename( ".aff-tmp.", out_fname ) ) )
    {
        fprintf( stderr, "%s: cannot create a unique writable file\n", __func__ );
        perror( __func__ );
        return 1;
    }
    struct AffWriter_s *w;
    if (NULL == (w = aff_writer( tmp_fname )))
    {
        fprintf(stderr, "%s: not enough memory\n", __func__);
        goto errclean_free;
    }
    if( NULL != aff_writer_errstr( w ) )
    {
        fprintf( stderr, "%s: %s: %s\n", __func__, tmp_fname, aff_writer_errstr( w ) );
        goto errclean_w;
    }
    // insert data from pre-list
    if( NULL != list1_fname )
    {
        if( insert_keylist( w, list1_fname ) )
            goto errclean_w;
    }
    // insert data from cmdline key list
    for( ; argc ; argc -= 3, argv += 3 ) {
        if(insert_data( w, argv[1], argv[2], argv[0]))
            goto errclean_w;
    }
    // insert data from post-list
    if( NULL != list2_fname )
    {
        if( insert_keylist( w, list2_fname ) )
            goto errclean_w;
    }
    
    if( NULL != ( status = aff_writer_close( w ) ) )
    {
        fprintf( stderr, "%s: %s\n", __func__, status );
        goto errclean_file;
    }
    if( rename( tmp_fname, out_fname ) )
    {
        perror( __func__ );
        fprintf( stderr, "%s: output is saved to %s\n", __func__, tmp_fname );
        goto errclean_free;
    }
    free( tmp_fname );
    return 0;
    
errclean_w:
    aff_writer_close( w );
errclean_file:
    if( remove( tmp_fname ) )
        perror( __func__ );
errclean_free:
    free(tmp_fname);
    return 1;
}

void h_insert(void)
{
    printf( "Usage:\nlhpc-aff insert -o <output> [-i] [-f <pre-list>] [-F <post-list>]\n"
            "\t\t[<dst-kpath> <src-file> <src-kpath>] ...\n"
            "Join data entries into one aff-file. Insertion is recursive:\n"
            "<output>[<dst-kpath>/]  <--  <src-file>[<src-kpath>/*].\n"
            "Note that the data in <src-kpath> itself is NOT copied.\n"
            "New data replaces old data, and the leftmost data takes precedence.\n"
            "Output file name may be in the list of files to insert: it will be\n"
            "replaced after the end of all insertions.\n"
            "\t-i\tignore error if <src-file> or <src-path> do not exist\n"
            "\t-o <output>\n\t\twrite result of merge to <output>.\n"
            "\t-f <pre-list>\n\t\texecute insert instructions in file <pre-list> BEFORE\n"
            "\t\tprocessing command line list.\n"
            "\t-F <post-list>\n\t\texecute insert instructions in file <post-list> AFTER\n"
            "\t\tprocessing command line list.\n"
            "\t\teach line of <list> is a separate insert instruction,\n"
            "\t\tand it must have at least three names, in order:\n"
            "\t\t<dst-kpath> <src-file> <src-kpath>\n"
            "\t\tparameter to only one of -f, -F options can be '-' (stdin).\n"
          );
}

void h_join(void)
{
    printf( "Usage:\nlhpc-aff join -o <output> [-i] [-f <pre-list>] [-F <post-list>]\n"
            "\t\t[<dst-kpath> <src-file> <src-kpath>] ...\n"
            "Join data entries into one aff-file. Insertion is recursive:\n"
            "<output>[<dst-kpath>/]  <--  <src-file>[<src-kpath>/*].\n"
            "Note that the data in <src-kpath> itself is NOT copied.\n"
            "New data replaces old data, and the leftmost data takes precedence.\n"
            "Output file name may be in the list of files to insert: it will be\n"
            "replaced after the end of all insertions.\n"
            "\t-i\tignore error if <src-file> or <src-path> do not exist\n"
            "\t-o <output>\n\t\twrite result of merge to <output>.\n"
            "\t-f <pre-list>\n\t\texecute insert instructions in file <pre-list> BEFORE\n"
            "\t\tprocessing command line list.\n"
            "\t-F <post-list>\n\t\texecute insert instructions in file <post-list> AFTER\n"
            "\t\tprocessing command line list.\n"
            "\t\teach line of <list> is a separate insert instruction,\n"
            "\t\tand it must have at least three names, in order:\n"
            "\t\t<dst-kpath> <src-file> <src-kpath>\n"
            "\t\tparameter to only one of -f, -F options can be '-' (stdin).\n"
          );
}
