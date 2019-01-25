/*! $Id: .vimrc,v 1.4 2007/03/22 18:26:06 syritsyn Exp $
 **************************************************************************
 * \file cat.c
 *
 * \author Sergey N. Syritsyn
 * 
 * \date Created: 24/08/2007
 *
 ***************************************************************************/
#include <assert.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
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

static int 
    gnuplot_spacing = 0,
    print_comment_line = 0,
    print_index = 0,
    print_all_subnodes = 0,
    print_recursive = 0,
    print_end_mark = 0,
    ignore_missing = 0;

struct cat_node_arg
{
    struct AffReader_s *r;
    int first;
    const char *errstr;
};


/* For affNodeInvalid, print error
   for affNodeVoid, print nothing */
static
void cat_single_node( struct AffNode_s *r_node, void *arg_ )
{
    if( NULL == r_node ||
        NULL == arg_ )
        return;
    struct cat_node_arg *arg = (struct cat_node_arg *)arg_;
    if( NULL != arg->errstr )
        return;
    struct AffReader_s *r = arg->r;
    uint32_t size = aff_node_size( r_node );
    enum AffNodeType_e type = aff_node_type( r_node );
    
    if( affNodeInvalid == type )
    {
        arg->errstr = "cat_single_node: invalid node\n";
        return;
    }
    if( affNodeVoid == type )       // FIXME is it right?
        return;
    if( !arg->first && gnuplot_spacing )
        printf( "\n\n" );
    arg->first = 0;
    if( print_comment_line )
    {
        printf( "# " ); 
        const char *type_str = NULL;
        switch( aff_node_type( r_node ) )
        {
        case affNodeInvalid: 
            {
                arg->errstr = "cat_single_node: invalid node\n";
                return;
            }
        case affNodeVoid:       type_str = "void";      break;
        case affNodeChar:       type_str = "char";      break;
        case affNodeInt:        type_str = "int";       break;
        case affNodeDouble:     type_str = "double";    break;
        case affNodeComplex:    type_str = "complex";   break;
        }
        printf( "%s[%d]  ", type_str, (int)size );
        print_path( aff_reader_root( r ), r_node );
        printf( "\n" );
    }
    switch( aff_node_type( r_node ) )
    {
    case affNodeInvalid: 
        {
            arg->errstr = "cat_single_node: invalid node\n";
            return;
        }
    case affNodeVoid: break;
    case affNodeChar: 
        {
            char *buf = (char *)malloc( size );
	    if (buf == 0) {
		arg->errstr = "Not enough memory";
		return;
	    }
            if( aff_node_get_char( r, r_node, buf, size ) )
            {
                arg->errstr = aff_reader_errstr( r );
		free(buf);
                return;
            }
	    for ( size_t i = 0; i < size; i++) {
		unsigned char p = buf[i];
		if (p < 32 || p >= 127 || p == '\"' || p == '\\')
		    printf("\\x%02x", p);
		else
		    printf("%c", p);
	    }
            printf( "\n" );
            free( buf );
        } break;
    case affNodeInt:
        {
            uint32_t *buf = (uint32_t *)malloc( size *sizeof(uint32_t) );
	    if (buf == 0) {
		arg->errstr = "Not enough memory";
		return;
	    }
            if( aff_node_get_int( r, r_node, buf, size ) )
            {
                arg->errstr = aff_reader_errstr( r );
		free(buf);
                return;
            }
            for( size_t i = 0 ; i < size ; ++i )
            {
                if( print_index )
                    printf( "%ld\t", (long int)i );
                printf( "%ld\n", (long int)(buf[i]) );
            }
            free( buf );
        } break;
    case affNodeDouble: 
        {
            double *buf = (double *)malloc( size *sizeof(double) );
	    if (buf == 0) {
		arg->errstr = "Not enough memory";
		return;
	    }
            if( aff_node_get_double( r, r_node, buf, size ) )
            {
                arg->errstr = aff_reader_errstr( r );
		free(buf);
                return;
            }
            for( size_t i = 0 ; i < size ; ++i )
            {
                if( print_index )
                    printf( "%ld\t", (long int)i );
                printf( "%24.16e\n", buf[i] );
            }
            free( buf );
        } break;
    case affNodeComplex: 
        {
            double _Complex *buf = (double _Complex *)
                malloc( size *sizeof(double _Complex) );
	    if (buf == 0) {
		arg->errstr = "Not enough memory";
		return;
	    }
            if( aff_node_get_complex( r, r_node, buf, size ) )
            {
                arg->errstr = aff_reader_errstr( r );
		free(buf);
                return;
            }
            for( size_t i = 0 ; i < size ; ++i )
            {
                if( print_index )
                    printf( "%ld\t", (long int)i );
                printf( "%24.16e\t%24.16e\n", creal( buf[i] ), cimag( buf[i] ) );
            }
            free( buf );
        } break;
    }
    if( print_end_mark )
        printf( "#AFF:END\n" );
}

void cat_nodes_recursive( struct AffNode_s *r_node, void *arg_ )
{
    if( NULL == r_node ||
        NULL == arg_ )
        return;
    cat_single_node( r_node, arg_ );
    struct cat_node_arg *arg = (struct cat_node_arg *)arg_;
    if( NULL != arg->errstr )
        return;
    aff_node_foreach( r_node, cat_nodes_recursive, arg_ );
}

int cat_keypath( struct AffReader_s *r, const char *keypath, 
        struct cat_node_arg *arg )
{
    struct AffNode_s *r_node = lookup_path(r, aff_reader_root(r), keypath);
    if (NULL == r_node) {
        fprintf(stderr, "%s: [%s]: cannot read node\n", __func__, keypath);
        return !ignore_missing;
    }
    
    if (print_recursive)
        cat_nodes_recursive(r_node, arg);
    else if (print_all_subnodes)
        aff_node_foreach(r_node, cat_single_node, arg);
    else
        cat_single_node(r_node, arg);
    if( NULL != arg->errstr )
    {
        fprintf(stderr, "%s: [%s]: %s\n", __func__, keypath, arg->errstr);
        return 1;
    }
    return 0;
}

int cat_keylist( struct AffReader_s *r, const char *list_fname, 
        struct cat_node_arg *arg )
{
    FILE *list = NULL;
    if(0 == strcmp(list_fname, "-")) {
        list = stdin;
        if (ferror(list)) {
            fprintf(stderr, "%s: bad stdin stream\n", __func__);
            return 1;
        }
    }
    else {
        if (NULL == (list = fopen(list_fname, "r"))) {
            fprintf(stderr, "%s: cannot open %s\n", __func__, list_fname);
            return 1;
        }
    }
    char buf[16384], *fargv[1];
    int num;
    while (NULL != fgets(buf, sizeof(buf), list)) {
        if ('\n' != buf[strlen(buf)-1]) {
            fprintf(stderr, "%s: line too long, skipping\n", __func__);
            while (NULL != fgets(buf, sizeof(buf), list))
                if ('\n' == buf[strlen(buf)-1])
                    break;
            continue;
        }
        num = split_farg( buf, 1, fargv );
        if (num < 0) {
            fprintf(stderr, "%s: unexpected result of split_farg; exiting\n",
                    __func__);
            goto errclean_r;
        }
        if (num == 0)
            continue;
        if (cat_keypath(r, fargv[0], arg)) {
            fprintf(stderr, "%s: [%s]\n", __func__, fargv[0]);
            goto errclean_r;
        }
    }
    
    fclose(list);
    return 0;
    
errclean_r:
    fclose(list);
    return 1;
}

int x_cat( int argc, char *argv[] )
{
// -g print for gnuplot
// -c comments
// -n numbering
// -a all immediate subnodes
    char *list1_fname = NULL,
         *list2_fname = NULL;
    if( argc < 1 )
    {
        h_cat();
        return 1;
    }
    for( ; argc ; --argc, ++argv )
    {
        if( '-' != argv[0][0] )
            break;
        for( char *p = argv[0] + 1; '\0' != *p ; ++p )
        {
            switch(*p)
            {
            case 'g':   gnuplot_spacing = 1;        break;
            case 'c':   print_comment_line = 1;     break;
            case 'm':   print_end_mark = 1;         break;
            case 'n':   print_index = 1;            break;
            case 'a':   print_all_subnodes = 1;     break;
            case 'R':   print_recursive = 1;        break;
            case 'i':   ignore_missing = 1;         break;
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
    if( NULL != list1_fname && NULL != list2_fname &&
        0 == strcmp( list1_fname, "-" ) && 0 == strcmp( list2_fname, "-" ) )
    {
        fprintf( stderr, "%s: both pre- and post-list cannot be '-'(stdin)\n",
                 __func__ );
        return 1;
    }
    const char *fname = NULL;
    if( argc-- )
        fname = *(argv++);
    else
    {
        fprintf( stderr, "%s: aff file name missing\n", __func__ );
        return 1;
    }
    struct AffReader_s *r = aff_reader( fname );
    if( NULL != aff_reader_errstr( r ) )
    {
        fprintf( stderr, "%s: %s\n", __func__, aff_reader_errstr( r ) );
        goto errclean_r;
    }
    struct AffNode_s *r_root = aff_reader_root( r );
    if( NULL == r_root )
    {
        fprintf( stderr, "%s: %s\n", __func__, aff_reader_errstr( r ) );
        goto errclean_r;
    }
    
    struct cat_node_arg arg;
    arg.r = r;
    arg.first = 1;
    arg.errstr = NULL;
    // TODO print root for empty key list
    if( 0 == argc && NULL == list1_fname && NULL == list2_fname )
    {
        if( cat_keypath( r, "/", &arg ) )
            goto errclean_r;
    }
    // pre-list
    if( NULL != list1_fname )
    {
        if( cat_keylist( r, list1_fname, &arg ) )
            goto errclean_r;
    }
    // cmdline list
    for( ; argc ; --argc, ++argv )
    {
        if( cat_keypath( r, *argv, &arg ) )
            goto errclean_r;
    }
    // TODO post-list
    if( NULL != list2_fname )
    {
        if( cat_keylist( r, list2_fname, &arg ) )
            goto errclean_r;
    }
    
    aff_reader_close( r );
    return 0;

errclean_r:
    aff_reader_close( r );
    return 1;
}

void h_cat(void)
{
    printf( "Usage:\n"
            "lhpc-aff cat [-acgimnR] [-f <pre-list>] [-F <post-list>] <aff-file>\n"
            "\t\t[<keypath>] ...\n"
            "Print the data associated with keypath in the file aff-file.\n"
            "If no <keypath> or -f,-F options given, list the root node.\n"
            "\t-a\tprint data of all immediate subkeys instead of\n"
            "\t\tgiven keypath itself\n"
            "\t-c\tprint a comment line starting with #\n" 
            "\t-g\tput gnuplot-style double new-line separators between data\n"
            "\t\tof different keys\n"
            "\t-i\tignore error if <keypath> does not exist\n"
            "\t-m\tprint line '#AFF:END' after each record (useful for batch processing)\n"
            "\t-n\tput the array index in a first column\n"
            "\t-R\tprint the keypath all its subkeys recursively; \n"
            "\t\tthis key takes precedence over `-a'\n"
            "\t-f <pre-list>\n\t\tprint data from key list in file <pre-list> BEFORE\n"
            "\t\tprocessing command line list\n"
            "\t-F <post-list>\n\t\tprint data from key list in file <post-list> AFTER\n"
            "\t\tprocessing command line list\n"
            "\t\tparameter to only one of -f, -F options can be '-' (stdin)\n"
            );
}
