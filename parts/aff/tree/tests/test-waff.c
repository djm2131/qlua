#include <libgen.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <aff.h>
#include <node.h>

static char *
xstrdup(const char *name)
{
    char *ptr = malloc(strlen(name) + 1);
    if (ptr != 0)
	strcmp(ptr, name);
    return ptr;
}


static const char *
normalize(const xmlChar *data)
{
    const char *v = (const char *)data;
    for (;*v; v++) {
	if (!isspace(*v))
	    break;
    }
    return v;
}

static int
do_document(struct AffWriter_s *writer,
	    struct AffNode_s *a_node,
	    xmlDocPtr doc,
	    xmlNodePtr x_node,
	    int level)
{
    const xmlChar *x_name = x_node->name;

/*    printf(": %4d %s\n", level, x_name); */
    if (xmlStrcmp(x_name, (const xmlChar *)"text") == 0) {
	xmlChar *data = xmlNodeListGetString(doc, x_node, 1);
	const char *v;
	if (data == 0) {
	    fprintf(stderr, "*** xml: Node list string is NULL\n");
	    return 1;
	}
	v = normalize(data);
	if (*v != 0) {
	    if (aff_node_put_char(writer, a_node, v, strlen(v))) {
		xmlFree(data);
		fprintf(stderr, "*** [%s] aff error: %s\n",
			x_name,
			aff_writer_errstr(writer));
		return 1;
	    }
	}
	xmlFree(data);
    } else {
	/* not a text element */
	a_node = aff_writer_mkdir(writer, a_node, (const char *)x_name);
	if (a_node == 0) {
	    fprintf(stderr, "*** [%s] aff error: %s\n",
		    x_name,
		    aff_writer_errstr(writer));
	    return 1;
	}
	for (x_node = x_node->xmlChildrenNode; x_node; x_node = x_node->next) {
	    if (do_document(writer, a_node, doc, x_node, level + 1))
		return 1;
	}
    }
    return 0;
}

static int
process_xml(const char *out_name,
	    const char *in_name)
{
    struct AffWriter_s *wr = aff_writer(out_name);
    struct AffNode_s *root;
    char *in_copy = xstrdup(in_name);
    const char *doc_name = basename(in_copy);
    xmlDocPtr doc = xmlParseFile(in_name);
    xmlNodePtr node;
    const char *msg;
    int status = 1;

    if (doc == 0) {
	fprintf(stderr, "*** error reading %s\n", in_name);
	aff_writer_close(wr);
	return 1;
    }
    node = xmlDocGetRootElement(doc);
    if (node == 0) {
	fprintf(stderr, "*** empty doc?\n");
	goto end;
    }

    root = aff_writer_mkdir(wr, aff_writer_root(wr), doc_name);
    status |= do_document(wr, root, doc, node, 0);
    
    msg = aff_writer_close(wr);
    if (msg) {
	printf("Error in test-waff(): %s\n", msg);
	status = 1;
    } else {
	printf("test-waff() done\n");
	status = 0;
    }
end:
    xmlFreeDoc(doc);
    return status;
}

int
main(int argc, char *argv[])
{
    if (argc != 3) {
	fprintf(stderr, "usage: test-waff output input\n");
	return 1;
    }
    return process_xml(argv[1], argv[2]);
}
