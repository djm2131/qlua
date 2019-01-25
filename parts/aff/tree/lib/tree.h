#ifndef MARK_c57377cb_879b_4716_a827_b5a4428d7fbc
#define MARK_c57377cb_879b_4716_a827_b5a4428d7fbc

/* Tree external types */
struct AffNode_s;
struct AffSymbol_s;

/* Tree table implementation.
 *
 * foreach() passes elements of the table in order of ids, root is not returned.
 * insert() guarantees dense packing of indices, NULL is returned if there is a
 *          duplicate or not enough memory.
 * size() returns the largest valid Id in the tree;
 * file_size() returns the number of bytes needed to store the tree in a file.
 * root() returns an a root node of the tree.
 * lookup() returns NULL if the node does not exist.
 */
struct AffTree_s;
struct AffSTable_s;

struct AffTree_s *aff_tree_init(struct AffSTable_s *, uint64_t size);
void *aff_tree_fini(struct AffTree_s *tree);
void aff_tree_foreach(const struct AffTree_s *tree,
		      void (*proc)(struct AffNode_s *node, void *arg),
		      void *arg);
void aff_tree_print(struct AffTree_s *tree);
struct AffNode_s *aff_tree_root(const struct AffTree_s *tree);
struct AffNode_s *aff_tree_lookup(const struct AffTree_s *tree,
				  const struct AffNode_s *parent,
				  const struct AffSymbol_s *name);
struct AffNode_s *aff_tree_index(const struct AffTree_s *tree,
				 uint64_t index);
struct AffNode_s *aff_tree_insert(struct AffTree_s *tree,
				  struct AffNode_s *parent,
				  const struct AffSymbol_s *name);

#endif /* !defined(MARK_c57377cb_879b_4716_a827_b5a4428d7fbc) */
