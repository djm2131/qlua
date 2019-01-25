#ifndef MARK_a4d076cf_516e_4864_861a_ed8239937ca5
#define MARK_a4d076cf_516e_4864_861a_ed8239937ca5
/* Tree Nodes.
 *
 * The tree nodes are created by the tree, here we have accessors and mutators
 * for the nodes.
 * cd*() walk the tree. If the create argument is zero, missing elements of
 *       the pass will result in error (NULL is returned), otherwise, missing
 *       components will be created in the path. Freshly created components
 *       could be assigned, their initial type is affNodeVoid.
 * foreach() traverses children of the node.
 * assign() could be called only once on the node.
 * id() returns the 8 byte node id.
 * name() returns the node name as a symbol.
 * parent() returns the node's parent.
 * type() returns current node type.
 * size() returns current node size.
 * offset() returns current node offset.
 *
 * Tree traversal operations.
 *  If create argument is 0, no new elements will be inserted into the tree,
 *  otherwise, missing components will be added to the tree and symbol table.
 *
 * cda() walks through a given sequence of path elments. The list ends with NULL
 * cd()  takes a list as separate arguments, the last argument must be NULL.
 * cdv() takes arguments of cd() converted into va_list.
 * chdir() moves one step through the tree.
 *
 * There are also node manipulation routines in AFF readers and writers.
 */
struct AffNode_s;

enum AffNodeType_e {
    affNodeInvalid,
    affNodeVoid,
    affNodeChar,
    affNodeInt,
    affNodeDouble,
    affNodeComplex
};

struct AffTree_s;
struct AffSTable_s;

void aff_node_foreach(struct AffNode_s *n,
		      void (*proc)(struct AffNode_s *child,
				   void *arg),
		      void *arg);
uint64_t aff_node_id(const struct AffNode_s *tn);
const struct AffSymbol_s*aff_node_name(const struct AffNode_s *n);
struct AffNode_s *aff_node_parent(const struct AffNode_s *n);
enum AffNodeType_e aff_node_type(const struct AffNode_s *n);
uint32_t aff_node_size(const struct AffNode_s *n);
uint64_t aff_node_offset(const struct AffNode_s *tn);
int aff_node_assign(struct AffNode_s *node,
		    enum AffNodeType_e type,
		    uint32_t size,
		    uint64_t offset);

struct AffNode_s *aff_node_chdir(struct AffTree_s *tree,
				 struct AffSTable_s *stable,
				 struct AffNode_s *n,
				 int create,
				 const char *p);
struct AffNode_s *aff_node_cda(struct AffTree_s *tree,
			       struct AffSTable_s *stable,
			       struct AffNode_s *n,
			       int create,
			       const char *p[]);
struct AffNode_s *aff_node_cdv(struct AffTree_s *tree,
			       struct AffSTable_s *stable,
			       struct AffNode_s *n,
			       int create,
			       va_list va);
struct AffNode_s *aff_node_cd(struct AffTree_s *tree,
			      struct AffSTable_s *stable,
			      struct AffNode_s *n,
			      int create,
			      ...);

#endif /* !defined(MARK_a4d076cf_516e_4864_861a_ed8239937ca5) */
