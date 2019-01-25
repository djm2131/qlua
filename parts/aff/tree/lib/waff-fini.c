#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <limits.h>
#include <stdarg.h>
#include "io.h"
#include "md5.h"
#include "node.h"
#include "tree.h"
#include "stable.h"
#include "coding.h"
#include "alloc.h"
#include "aff-i.h"

static void
out_string(const struct AffSymbol_s *symbol, void *arg)
{
    struct AffWriter_s *aff = arg;

    if (aff->error == 0) {
        const char *name = aff_symbol_name(symbol);
        int size = strlen(name) + 1;

        if (fwrite(name, size, 1, aff->file) != 1) {
            aff->error = "Symbol writing error";
            aff->fatal_error = 1;
            return;
        }
        aff_md5_update(&aff->stable_hdr.md5, (const uint8_t *)name, size);
        aff->stable_hdr.size += size;
        aff->position += size;
        aff->stable_hdr.records++;
    }
}

static void
out_node(struct AffNode_s *node, void *arg)
{
    struct AffWriter_s *aff = arg;

    if (aff->error == 0) {
        uint8_t block[1+8+4+4+8];
        uint8_t *buf;
        uint32_t size;
        enum AffNodeType_e type = aff_node_type(node);
        
        size = 0;
        buf = aff_encode_type(&block[0], sizeof (block), type);
        size = sizeof (block) - (buf - &block[0]);
        buf = aff_encode_u64(buf, size, aff_node_id(aff_node_parent(node)));
        size = sizeof (block) - (buf - &block[0]);
        buf = aff_encode_u32(buf, size, aff_symbol_id(aff_node_name(node)));
        size = sizeof (block) - (buf - &block[0]);
        if (type != affNodeVoid) {
            buf = aff_encode_u32(buf, size, aff_node_size(node));       
            size = sizeof (block) - (buf - &block[0]);
            buf = aff_encode_u64(buf, size, aff_node_offset(node));
            size = sizeof (block) - (buf - &block[0]);
        }
        if (buf == 0) {
            aff->error = "Node encoding overrun";
            aff->fatal_error = 1;
            return;
        }
        size = buf - &block[0];
        if (fwrite(block, size, 1, aff->file) != 1) {
            aff->error = "Node writing error";
            aff->fatal_error = 1;
            return;
        }
        aff_md5_update(&aff->tree_hdr.md5, block, size);
        aff->tree_hdr.size += size;
        aff->position += size;
        aff->tree_hdr.records++;
    }
}

static void
pack(struct AffWriter_s *aff, struct WSection_s *section, char *error_msg)
{
    uint8_t block[8+8+8+16];
    uint8_t *buf;
    uint32_t size;

    if (aff->error)
        return;

    size = sizeof (block);
    buf = aff_encode_u64(&block[0], size, section->start);
    size = sizeof (block) - (buf - &block[0]);
    buf = aff_encode_u64(buf, size, section->size);
    size = sizeof (block) - (buf - &block[0]);
    buf = aff_encode_u64(buf, size, section->records);
    size = (buf - &block[0]);
    if (buf == 0 || sizeof (block) - size < 16) {
        aff->error = "Section header encoding overrun";
        aff->fatal_error = 1;
        return;
    }

    aff_md5_final(buf, &section->md5);
    size += 16;
    if (fwrite(block, size, 1, aff->file) != 1) {
        aff->error = error_msg;
        aff->fatal_error = 1;
        return;
    }
    aff_md5_update(&aff->header_md5, block, size);
    aff->header_size += size;
}


const char *
aff_writer_close(struct AffWriter_s *aff)
{
    uint8_t buffer[AFF_HEADER_SIZE2];
    uint8_t *ptr;
    uint32_t size;

    if (aff == 0)
        return "NULL aff passed to aff_writer_close()";

    if (aff->error)
        goto end;

    aff->stable_hdr.start = aff->position;
    aff->stable_hdr.size = 0;
    aff->stable_hdr.records = 0;
    aff_md5_init(&aff->stable_hdr.md5);
    aff_stable_foreach(aff->stable, out_string, aff);
    if (aff->error)
        goto end;

    aff->tree_hdr.start = aff->position;
    aff->tree_hdr.size = 0;
    aff->tree_hdr.records = 0;
    aff_md5_init(&aff->tree_hdr.md5);
    aff_tree_foreach(aff->tree, out_node, aff);
    if (aff->error)
        goto end;

    if (aff_file_setpos(aff->file, 0) != 0) {
        aff->error = "AFF positioning error";
        aff->fatal_error = 1;
        goto end;
    }

    aff_md5_init(&aff->header_md5);
    {
        const uint8_t *sig = 0;
        switch (aff->version) {
        case 2: sig = (const uint8_t *)aff_signature2; break;
        case 3: sig = (const uint8_t *)aff_signature3; break;
        default:
            aff->error = "AFF internal error";
            aff->fatal_error = 1;
            goto end;
        }
        aff->header_size = strlen((char *)sig) + 1;
        if (fwrite(sig, aff->header_size, 1, aff->file) != 1) {
            aff->error = "AFF Signature writing erorr";
            aff->fatal_error = 1;
            goto end;
        }
        aff_md5_update(&aff->header_md5, sig, aff->header_size);
    }

    buffer[0] = sizeof (double) * CHAR_BIT;
    buffer[1] = FLT_RADIX;
    buffer[2] = DBL_MANT_DIG;
    ptr = aff_encode_u32(buffer + 3, sizeof (buffer) - 3,
                         (DBL_MAX_EXP << 16) | (- DBL_MIN_EXP));
    ptr = aff_encode_u32(ptr, sizeof(buffer) - (ptr - buffer),
                         AFF_HEADER_SIZE2);
    if (ptr == 0) {
        aff->error = "Can't encode DBL_MIN_EXP";
        aff->fatal_error = 1;
        goto end;
    }
    size = ptr - buffer;
    aff->header_size += size;
    if (fwrite(buffer, size, 1, aff->file) != 1) {
        aff->error = "AFF size signature writing error";
        aff->fatal_error = 1;
        goto end;
    }
    aff_md5_update(&aff->header_md5, buffer, size);

    pack(aff, &aff->data_hdr, "Data header writing error");
    pack(aff, &aff->stable_hdr, "Stable header writing error");
    pack(aff, &aff->tree_hdr, "Tree header writing error");
    if (aff->error)
        goto end;
    
    aff_md5_final(buffer, &aff->header_md5);
    aff->header_size += 16;
    if (fwrite(buffer, 16, 1, aff->file) != 1) {
        aff->error = "AFF checksum writing error";
        aff->fatal_error = 1;
        goto end;
    }

    /* This is an assert() really */
    if (aff->header_size != AFF_HEADER_SIZE2) {
        aff->error = "AFF INTERNAL ERROR: header size mismatch";
        aff->fatal_error = 1;
        goto end;
    }

end:
    {
        const char *msg = aff->error;

        if( NULL != aff->file )
            fclose(aff->file);
        aff_stable_fini(aff->stable);
        aff_tree_fini(aff->tree);
        aff_realloc(aff, 0);
        return msg;
    }
}
