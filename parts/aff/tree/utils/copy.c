/*! $Id: .vimrc,v 1.9 2007/08/29 18:08:37 syritsyn Exp $
 **************************************************************************
 * \file copy.c
 *
 * \author Sergey N. Syritsyn
 * 
 * \date Created: 30/08/2007
 *    2008/06/09 avp -- changes for 
 *                 mkdir_path() --> aff_writer_mkpath()
 *                 chdir_path() --> aff_reader_chpath()
 *
 ***************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "lhpc-aff.h"
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "util.h"
#include "common.h"

static int ignore_missing = 0;

static int do_copy( struct AffReader_s *r, struct AffWriter_s *w,
        const char *src_kpath, const char *dst_kpath, int copy_recursive )
{
    const char *status;
    struct AffNode_s *r_root = aff_reader_root(r);
    struct AffNode_s *r_node = lookup_path(r, r_root, src_kpath);
    if (NULL == r_node) {
        fprintf(stderr, "%s: [%s]: cannot read node\n", __func__, src_kpath);
        return !ignore_missing;
    }
    if( aff_reader_root( r ) == r_node )
    {
        fprintf( stderr, "%s: %s: cannot copy the root node; use extract instead\n",
                __func__, src_kpath );
        return 1;
    }
    struct AffNode_s *w_node = aff_writer_mkpath(w, aff_writer_root(w),
                                                 dst_kpath);
    if( NULL == w_node )
    {
        fprintf( stderr, "%s: cannot write to key [%s]: %s\n", 
                __func__, dst_kpath, aff_writer_errstr( w ) );
        return 1;
    }
    if( aff_writer_root( w ) == w_node )
    {
        fprintf( stderr, "%s: %s: cannot replace the root node; use extract instead\n",
                __func__, dst_kpath );
        return 1;
    }
    
    if( NULL != ( status = copy_node_data( r, r_node, w, w_node, COPY_NODE_STRONG ) ) )
    {
        fprintf( stderr, "%s: %s", __func__, status );
        return 1;
    }
    struct copy_nodes_arg arg;
    if( copy_recursive )
    {
        arg.r = r;
        arg.w = w;
        arg.w_parent = w_node;
        arg.weak = COPY_NODE_WEAK;
        arg.errstr = NULL;
        aff_node_foreach( r_node, copy_nodes_recursive, (void *)&arg );
        if( NULL != arg.errstr )
        {
            fprintf( stderr, "%s: %s\n", __func__, arg.errstr );
            return 1;
        }
    }
    return 0;
}

int copy_keylist( struct AffReader_s *r, struct AffWriter_s *w,
        const char *list_fname, int copy_recursive )
{
    FILE *list;
    char *fargv[2];
    char buf[32768];
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
        num = split_farg( buf, 2, fargv );
        if( num < 0 )
        {
            fprintf( stderr, "%s: unexpected result of split_farg; exiting\n",
                    __func__ );
            goto errclean_r;
        }
        if( num == 0 )
            continue;
        if( num < 2 )
        {
            fprintf( stderr, "%s: syntax error: expect 2 names, only %d given\n",
                     __func__, num );
            goto errclean_r;
        }
        if( do_copy( r, w, fargv[0], fargv[1], copy_recursive ) )
            goto errclean_r;
    }
    fclose( list );
    return 0;

errclean_r:
    fclose( list );
    return 1;
}

int x_copy( int argc, char **argv )
{
    int copy_recursive = 0;
    const char *src_fname = NULL,
               *dst_fname = NULL,
               *list1_fname = NULL,
               *list2_fname = NULL,
               *status = NULL;
    for( ; argc ; --argc, ++argv )
    {
        if( '-' != argv[0][0] )
            break;
        for( char *p = argv[0] + 1 ; '\0' != *p ; ++p )
        {
            switch( *p )
            {
            case 'i': ignore_missing = 1;       break;
            case 'R': copy_recursive = 1;       break;
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
                fprintf( stderr, "%s: unknown option -%c\n", __func__, *p );
                return 1;
            }
        }
    }
    if( !(argc--) )
    {
        fprintf( stderr, "%s: source aff file name should be given\n", __func__ );
        return 1;
    }
    else
        src_fname = *(argv++);
    if( !(argc--) )
    {
        fprintf( stderr, "%s: target aff file name should be given\n", __func__ );
        return 1;
    }
    else
        dst_fname = *(argv++);
    if( argc % 2 )
    {
        fprintf( stderr, "%s: unpaired src_kpath; exiting\n", __func__ );
        return 1;
    }

    struct AffReader_s *r;
    if (NULL == (r = aff_reader( src_fname )))
    {
        fprintf(stderr, "%s: not enough memory\n", __func__);
        return 1;
    }
    if( NULL != aff_reader_errstr( r ) )
    {
        fprintf( stderr, "%s: %s: %s\n", __func__, src_fname, aff_reader_errstr( r ) );
        goto errclean_r;
    }
    char *tmp_fname = mk_tmp_filename( ".aff-tmp.", dst_fname );
    if( NULL == tmp_fname )
    {
        fprintf( stderr, "%s: could not create a unique writable file\n", __func__ );
        goto errclean_r;
    }
    struct AffWriter_s *w;
    if (NULL == (w = aff_writer( tmp_fname )))
    {
        fprintf(stderr, "%s: not enough memory\n", __func__);
        goto errclean_r_free;
    }
    if( NULL != aff_writer_errstr( w ) )
    {
        fprintf( stderr, "%s: %s: %s\n", __func__, tmp_fname, aff_writer_errstr( w ) );
        goto errclean_rw;
    }
    
    if( NULL != list1_fname )
    {
        if( copy_keylist( r, w, list1_fname, copy_recursive ) )
            goto errclean_rw;
    }
    for( ; argc ; argc -= 2, argv += 2 )
    {
        if( do_copy( r, w, argv[0], argv[1], copy_recursive ) )
            goto errclean_rw;
    }
    if( NULL != list2_fname )
    {
        if( copy_keylist( r, w, list2_fname, copy_recursive ) )
            goto errclean_rw;
    }
    aff_reader_close( r );
    struct stat stat_fb;
    if (!stat(dst_fname, &stat_fb)) {
        if(NULL == (r = aff_reader( dst_fname )))
        {
            fprintf(stderr, "%s: not enough memory\n", __func__);
            goto errclean_w;
        }
        if( NULL != aff_reader_errstr( r ) )
        {
            fprintf( stderr, "%s: %s\n", __func__, aff_reader_errstr( r ) );
            goto errclean_rw;
        }
        struct copy_nodes_arg arg;
        arg.r = r;
        arg.w = w;
        arg.w_parent = aff_writer_root( w );
        arg.weak = COPY_NODE_WEAK;
        arg.errstr = NULL;
        aff_node_foreach( aff_reader_root( r ), copy_nodes_recursive, (void *)&arg );
        if( NULL != arg.errstr )
        {
            fprintf( stderr, "%s: %s\n", __func__, arg.errstr );
            goto errclean_rw;
        }
        aff_reader_close( r );
    }

    if( NULL != ( status = aff_writer_close( w ) ) )
    {
        fprintf( stderr, "%s: %s: %s\n", __func__, tmp_fname, status );
        goto errclean_file;
    }
    if( rename( tmp_fname, dst_fname ) )
    {
        perror( dst_fname );
        fprintf( stderr, "%s: output is saved to %s\n", __func__, tmp_fname );
        goto errclean_free;
    }
    free( tmp_fname );
    return 0;

errclean_rw:
    aff_reader_close( r );
errclean_w:
    aff_writer_close( w );
errclean_file:
    if( remove( tmp_fname ) )
        perror( tmp_fname );
errclean_free:
    free( tmp_fname );
    return 1;
    
errclean_r_free:
    free(tmp_fname);
errclean_r:
    aff_reader_close( r );
    return 1;
}

void h_copy(void)
{
    printf( "Usage:\n"
            "lhpc-aff cp [-iR] [-f <list>] [-F <list>] <src-file> <dst-file>\n"
            "\t\t[<src-kpath> <dst-kpath>] ...\n" 
            "Copy data from <src-kpath> of <src-file> to <dst-kpath> of <dst-file>:\n"
            "<dst-file>[<dst-kpath>]  <--  <src-file>[<src-kpath>].\n"
            "Keys <src-kpath> and <dst-kpath> cannot be root nodes, use 'extract' instead.\n"
            "New data replaces old data. If <dst-file> does not exist, it will be created.\n"
            "If <src-file> and <dst-file> are the same, nodes to copy will be put first,\n"
            "and then the rest of file will be reinserted.\n"
            "Options:\n"
            "\t-i\tignore error if <src-keypath> do not exist\n"
            "\t-R\tcopy data sub-entries recursively.\n"
            "\t-f <pre-list>\n\t\texecute copy instructions in file <pre-list> BEFORE\n"
            "\t\tprocessing command line list\n"
            "\t-F <post-list>\n\t\texecute copy instructions in file <pre-list> AFTER\n"
            "\t\tprocessing command line list\n"
            "\t\teach line of <list> is a separate insert instruction,\n"
            "\t\tand it must have at least two names, in order:\n"
            "\t\t<src-kpath> <dst-kpath>\n"
            "\t\tparameter to only one of -f, -F options can be '-' (stdin).\n"
            );
}
