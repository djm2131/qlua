#include <libgen.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>

struct Path {
    const xmlChar *name;
    struct Path *prev;
};

static xmlChar *
normalize(xmlChar *ptr)
{
    if (ptr == 0)
	return 0;
    while (isspace(*ptr))
	ptr++;
    return ptr;
}

static void
print_path(struct Path *p)
{
    if (p) {
	print_path(p->prev);
	printf("/%s", p->name);
    }
}

static void
walk_tree(xmlDocPtr doc, xmlNodePtr node, struct Path *up)
{
    struct Path p;
    xmlNodePtr clds;
    xmlChar *data;

    p.prev = up;
    p.name = node->name;

    if (xmlStrcmp(node->name, (const xmlChar *)"text")) {
#if 0
	print_path(&p);
	printf("\n");
#endif
    } else {
	data = normalize(xmlNodeListGetString(doc, node, 1));
	if (data && *data) {
	    print_path(up);
	    printf(" \"%s\"\n", data);
	}
    }

    for (clds = node->xmlChildrenNode; clds; clds = clds->next)
	walk_tree(doc, clds, &p);
}

int
main(int argc, char *argv[])
{
    struct Path p;
    char *fname;
    xmlDocPtr doc;
    xmlNodePtr node;

    if (argc != 2) {
	fprintf(stderr, "Usage: grok-xml xml\n");
	return 1;
    }
    fname = argv[1];
    doc = xmlParseFile(fname);
    if (doc == 0) {
	fprintf(stderr, "Error reading XML document %s\n", fname);
	return 1;
    }
    node = xmlDocGetRootElement(doc);
    if (node == 0) {
	fprintf(stderr, "empty document\n");
	goto end;
    }
    p.prev = 0;
    p.name = (xmlChar *)basename(fname);
    walk_tree(doc, node, &p);
	
end:
    xmlFreeDoc(doc);
    doc = 0;

    return 0;
}
