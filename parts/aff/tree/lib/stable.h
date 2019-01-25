#ifndef MARK_0c0bd3af_a15b_47a9_b1be_747bb78298b6
#define MARK_0c0bd3af_a15b_47a9_b1be_747bb78298b6
/*
 * String tables.
 *  insert() makes a copy of the name.
 *  lookup() returns NULL if the name is not found.
 *  index() returns NULL if index is out of range.
 *  for_each() goes through indices in order and is exposed for uniformity.
 *
 * Symbol ids are assigned sequentially starting from 0.
 */
struct AffSTable_s;
struct AffSymbol_s;

struct AffSTable_s *aff_stable_init(uint64_t size);
void *aff_stable_fini(struct AffSTable_s *st);
void aff_stable_print(const struct AffSTable_s *st);
const struct AffSymbol_s *aff_stable_lookup(const struct AffSTable_s *st,
					    const char *name);
const struct AffSymbol_s *aff_stable_index(const struct AffSTable_s *st,
					   uint32_t index);
const struct AffSymbol_s *aff_stable_insert(struct AffSTable_s *st,
					    const char *name);
void aff_stable_foreach(const struct AffSTable_s *st,
			void (*proc)(const struct AffSymbol_s *sym,
				     void *arg),
			void *arg);

const char *aff_symbol_name(const struct AffSymbol_s *sym);
uint32_t aff_symbol_id(const struct AffSymbol_s *sym);

#endif /* !defined(MARK_0c0bd3af_a15b_47a9_b1be_747bb78298b6) */
