#define ADD_EXPORTS
#include "main.h"

#ifndef ADD_EXPORTS
#include <stdio.h>
#endif
#include <string.h>
#include <stdlib.h>

#define RLE_NIBBLES_IN_LONG (8)
#define RLE_NIBBLES_IN_BYTE (2)
#define RLE_BITS_IN_NIBBLE  (4)

uint8 read_byte(const uint8 *buf, uint32 *offset)
{
    return buf[(*offset)++];
}

uint16 read_word_be(const uint8 *buf, uint32 *offset)
{
    uint8 b1 = read_byte(buf, offset);
    uint8 b2 = read_byte(buf, offset);

    return (b1 << 8) | b2;
}

uint32 read_dword_be(const uint8 *buf, uint32 *offset)
{
    uint16 w1 = read_word_be(buf, offset);
    uint16 w2 = read_word_be(buf, offset);

    return (w1 << 16) | w2;
}

void write_byte(uint8 *buf, uint32 *offset, uint8 b)
{
    buf[(*offset)++] = b;
}

void write_word_be(uint8 *buf, uint32 *offset, uint16 val)
{
    write_byte(buf, offset, (val >> 8) & 0xFF);
    write_byte(buf, offset, (val >> 0) & 0xFF);
}

void write_dword_be(uint8 *buf, uint32 *offset, uint32 val)
{
    write_word_be(buf, offset, (val >> 16));
    write_word_be(buf, offset, (val & 0xFFFF));
}

uint8 peek_byte(const uint8 *buf, uint32 offset)
{
    return buf[offset];
}

uint16 peek_word_be(const uint8 *buf, uint32 offset)
{
    uint8 b1 = peek_byte(buf, offset + 0);
    uint8 b2 = peek_byte(buf, offset + 1);

    return (b1 << 8) | b2;
}

uint32 peek_dword_be(const uint8 *buf, uint32 offset)
{
    uint16 w1 = peek_word_be(buf, offset + 0);
    uint16 w2 = peek_word_be(buf, offset + 2);

    return (w1 << 16) | w2;
}

int ADDCALL rle_decompress(const uint8 *input, uint8 *output, int offset, int *input_size)
{
    uint32 read_off = offset;
    uint32 write_off = 0;
	*input_size = 0;

    int out_size = read_dword_be(input, &read_off);

    int longs_to_write = out_size / 4;

    uint32 long_to_write = 0;
    uint8 nibbles_in_long = RLE_NIBBLES_IN_LONG;

    while (longs_to_write > 0)
    {
        uint8 b = read_byte(input, &read_off);

        uint8 nib_count = b & 0xF;
        uint8 nibble = b >> 4;

        int nibbles_to_write = 0;

        if (nib_count < 0xD)
            nibbles_to_write = nib_count;
        else if (nib_count == 0xD)
            nibbles_to_write = read_byte(input, &read_off);
        else
        {
            nibbles_to_write = read_word_be(input, &read_off);

            if (nib_count != 0xE)
                nibbles_to_write += 0x10000;
        }

        for (int i = 0; i < nibbles_to_write + 1; ++i)
        {
            long_to_write = (long_to_write << 4) | nibble;

            if (--nibbles_in_long == 0)
            {
                write_dword_be(output, &write_off, long_to_write);
                longs_to_write--;

                nibbles_in_long = RLE_NIBBLES_IN_LONG;

                if (longs_to_write == 0)
                    break;
            }
        }
    }

	*input_size = (read_off - offset);

    return write_off;
}

int ADDCALL rle_compress(const uint8 *input, uint8 *output, int size)
{
    int read_off = 0;
    int write_off = 0;

    write_dword_be(output, &write_off, size);

    int longs_size = size / 4;
    uint8 *nibbles = (uint8*)malloc(longs_size * RLE_NIBBLES_IN_LONG);

    while (read_off < size)
    {
        uint32 l = peek_dword_be(input, read_off);

        for (int j = 0; j < RLE_NIBBLES_IN_LONG; ++j)
            nibbles[read_off * RLE_NIBBLES_IN_BYTE + j] = (l >> ((RLE_NIBBLES_IN_LONG - j - 1) * RLE_BITS_IN_NIBBLE)) & 0xF;

        read_off += RLE_NIBBLES_IN_LONG / RLE_NIBBLES_IN_BYTE;
    }

    int i = 0;
    int nib_count = 1;
    for (i = 0; i < (longs_size * RLE_NIBBLES_IN_LONG);)
    {
        uint8 nibble = nibbles[i];
        nib_count = 1;

        while (i + nib_count < (longs_size * RLE_NIBBLES_IN_LONG) && nibble == nibbles[i + nib_count])
            nib_count++;

        uint8 token = nibble << 4;
        nib_count--;

        if (nib_count < 0xD)
        {
            token |= nib_count & 0xF;
            write_byte(output, &write_off, token);
        }
        else if (nib_count >= 0xD && nib_count <= 0xFF)
        {
            token |= 0xD;
            write_byte(output, &write_off, nib_count);
        }
        else if (nib_count >= 0x100 && nib_count <= 0xFFFF)
        {
            token |= 0xE;
            write_word_be(output, &write_off, nib_count);
        }
        else
        {
            token |= 0xF;
            write_word_be(output, &write_off, nib_count - 0x10000);
        }

        i += (nib_count + 1);
    }

    if ((write_off & 1) != 0)
        write_byte(output, &write_off, 0xFF);

    free(nibbles);

    return write_off;
}

int ADDCALL rle_compressed_size(uint8 *input)
{
    int read_off = 0;

    int out_size = read_dword_be(input, &read_off);

    int longs_to_write = out_size / 4;

    uint32 long_to_write = 0;
    uint8 nibbles_in_long = RLE_NIBBLES_IN_LONG;

    while (longs_to_write > 0)
    {
        uint8 b = read_byte(input, &read_off);

        uint8 nib_count = b & 0xF;
        uint8 nibble = b >> 4;

        int nibbles_to_write = 0;

        if (nib_count < 0xD)
            nibbles_to_write = nib_count;
        else if (nib_count == 0xD)
            nibbles_to_write = read_byte(input, &read_off);
        else
        {
            nibbles_to_write = read_word_be(input, &read_off);

            if (nib_count != 0xE)
                nibbles_to_write += 0x10000;
        }

        for (int i = 0; i < nibbles_to_write + 1; ++i)
        {
            long_to_write = (long_to_write << 4) | nibble;

            if (--nibbles_in_long == 0)
            {
                longs_to_write--;

                nibbles_in_long = RLE_NIBBLES_IN_LONG;

                if (longs_to_write == 0)
                    break;
            }
        }
    }

    return (read_off & 1) ? read_off + 1 : read_off;
}

#define LZH_N (4096) /* buffer size */
#define LZH_F (60) /* lookahead buffer size */
#define LZH_THRESHOLD (2)
#define LZH_NIL (LZH_N) /* leaf of tree */

#define LZH_N_CHAR (256 - LZH_THRESHOLD + LZH_F) /* kinds of characters (character code = 0..N_CHAR-1) */
#define LZH_T (LZH_N_CHAR * 2 - 1) /* size of table */
#define LZH_R (LZH_T - 1) /* position of root */
#define LZH_MAX_FREQ (0x8000) /* updates tree when the root frequency comes to this value. */

/* table for encoding and decoding the upper 6 bits of position */

/* for encoding */

static uint8 p_len[64] = {
    0x03, 0x04, 0x04, 0x04, 0x05, 0x05, 0x05, 0x05,
    0x05, 0x05, 0x05, 0x05, 0x06, 0x06, 0x06, 0x06,
    0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06,
    0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
    0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
    0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
    0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08,
    0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08
};

static uint8 p_code[64] = {
    0x00, 0x20, 0x30, 0x40, 0x50, 0x58, 0x60, 0x68,
    0x70, 0x78, 0x80, 0x88, 0x90, 0x94, 0x98, 0x9C,
    0xA0, 0xA4, 0xA8, 0xAC, 0xB0, 0xB4, 0xB8, 0xBC,
    0xC0, 0xC2, 0xC4, 0xC6, 0xC8, 0xCA, 0xCC, 0xCE,
    0xD0, 0xD2, 0xD4, 0xD6, 0xD8, 0xDA, 0xDC, 0xDE,
    0xE0, 0xE2, 0xE4, 0xE6, 0xE8, 0xEA, 0xEC, 0xEE,
    0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7,
    0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF
};

/* for decoding */

static uint8 d_code[256] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
    0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
    0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
    0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
    0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03,
    0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03,
    0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,
    0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06,
    0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
    0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08,
    0x09, 0x09, 0x09, 0x09, 0x09, 0x09, 0x09, 0x09,
    0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A,
    0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B,
    0x0C, 0x0C, 0x0C, 0x0C, 0x0D, 0x0D, 0x0D, 0x0D,
    0x0E, 0x0E, 0x0E, 0x0E, 0x0F, 0x0F, 0x0F, 0x0F,
    0x10, 0x10, 0x10, 0x10, 0x11, 0x11, 0x11, 0x11,
    0x12, 0x12, 0x12, 0x12, 0x13, 0x13, 0x13, 0x13,
    0x14, 0x14, 0x14, 0x14, 0x15, 0x15, 0x15, 0x15,
    0x16, 0x16, 0x16, 0x16, 0x17, 0x17, 0x17, 0x17,
    0x18, 0x18, 0x19, 0x19, 0x1A, 0x1A, 0x1B, 0x1B,
    0x1C, 0x1C, 0x1D, 0x1D, 0x1E, 0x1E, 0x1F, 0x1F,
    0x20, 0x20, 0x21, 0x21, 0x22, 0x22, 0x23, 0x23,
    0x24, 0x24, 0x25, 0x25, 0x26, 0x26, 0x27, 0x27,
    0x28, 0x28, 0x29, 0x29, 0x2A, 0x2A, 0x2B, 0x2B,
    0x2C, 0x2C, 0x2D, 0x2D, 0x2E, 0x2E, 0x2F, 0x2F,
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
    0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F,
};

static uint8 d_len[256] = {
    0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03,
    0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03,
    0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03,
    0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03,
    0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,
    0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,
    0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,
    0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,
    0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,
    0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,
    0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,
    0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,
    0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06,
    0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06,
    0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06,
    0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06,
    0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06,
    0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06,
    0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
    0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
    0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
    0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
    0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
    0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
    0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08,
    0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08,
};

typedef struct lzh_vars_s {
    uint8 *input, *output;
    int input_offset, output_offset;
    int output_size;

    int textsize;
    int codesize;

    uint32 freq[LZH_T + 1]; /* frequency table */
    int prnt[LZH_T + LZH_N_CHAR]; /* pointers to parent nodes, except for the elements [T..T + N_CHAR - 1] which are used to get
                                   * the positions of leaves corresponding to the codes. */
    int son[LZH_T]; /* pointers to child nodes (son[], son[] + 1) */
    uint32 getbuf;
    uint8 getlen;

    uint8 text_buf[LZH_N + LZH_F - 1];
    int lson[LZH_N + 1];
    int rson[LZH_N + 257];
    int dad[LZH_N + 1];
    int match_position, match_length;

    uint32 putbuf;
    uint8 putlen;
} lzh_vars_t;

void lzh_init_compressor(lzh_vars_t *v, uint8 *input, uint8 *output, int offset, int output_size)
{
    for (int i = 0; i < _countof(v->freq); i++) v->freq[i] = 0;
    for (int i = 0; i < _countof(v->prnt); i++) v->prnt[i] = 0;
    for (int i = 0; i < _countof(v->text_buf); i++) v->text_buf[i] = 0;
    for (int i = 0; i < _countof(v->lson); i++) v->lson[i] = 0;
    for (int i = 0; i < _countof(v->dad); i++) v->dad[i] = 0;
    for (int i = 0; i < _countof(v->rson); i++) v->rson[i] = 0;

    v->textsize = v->codesize = v->match_length = v->match_position = 0;
    v->getbuf = v->putbuf = 0;
    v->getlen = v->putlen = 0;

    v->input = input;
    v->output = output;
    v->input_offset = v->output_offset = 0;
    v->output_size = output_size;
}

void lzh_init_tree(lzh_vars_t *v) /* initialize trees */
{
    int i;

    for (i = LZH_N + 1; i <= LZH_N + 256; i++)
        v->rson[i] = LZH_NIL; /* root */
    for (i = 0; i < LZH_N; i++)
        v->dad[i] = LZH_NIL; /* node */
}

void lzh_insert_node(lzh_vars_t *v, int r) /* insert to tree */
{
    int i, p;
    uint32 c;
    uint8 *key;

    key = &v->text_buf[r];
    p = LZH_N + 1 + key[0];
    v->rson[r] = v->lson[r] = LZH_NIL;
    v->match_length = 0;
    for (;;)
    {
        if (v->rson[p] != LZH_NIL)
            p = v->rson[p];
        else
        {
            v->rson[p] = r;
            v->dad[r] = p;
            return;
        }

        for (i = 1; i < LZH_F; i++)
            if ((key[i] - v->text_buf[p + i]) != 0)
                break;
        if (i > LZH_THRESHOLD)
        {
            if (i > v->match_length)
            {
                v->match_position = ((r - p) & (LZH_N - 1)) - 1;
                if ((v->match_length = i) >= LZH_F)
                    break;
            }
            if (i == v->match_length)
            {
                c = (uint32)((r - p) & (LZH_N - 1)) - 1;
                if (c < (uint32)v->match_position)
                    v->match_position = (int)c;
            }
        }
    }
    v->dad[r] = v->dad[p];
    v->lson[r] = v->lson[p];
    v->rson[r] = v->rson[p];
    v->dad[v->lson[p]] = r;
    v->dad[v->rson[p]] = r;
    if (v->rson[v->dad[p]] == p)
        v->rson[v->dad[p]] = r;
    else
        v->lson[v->dad[p]] = r;
    v->dad[p] = LZH_NIL; /* remove p */
}

void lzh_delete_node(lzh_vars_t *v, int p)
{
    int q;

    if (v->dad[p] == LZH_NIL)
        return;         /* not registered */
    if (v->rson[p] == LZH_NIL)
        q = v->lson[p];
    else
        if (v->lson[p] == LZH_NIL)
            q = v->rson[p];
        else
        {
            q = v->lson[p];
            if (v->rson[q] != LZH_NIL)
            {
                do
                {
                    q = v->rson[q];
                } while (v->rson[q] != LZH_NIL);
                v->rson[v->dad[q]] = v->lson[q];
                v->dad[v->lson[q]] = v->dad[q];
                v->lson[q] = v->lson[p];
                v->dad[v->lson[p]] = q;
            }
            v->rson[q] = v->rson[p];
            v->dad[v->rson[p]] = q;
        }
    v->dad[q] = v->dad[p];
    if (v->rson[v->dad[p]] == p)
        v->rson[v->dad[p]] = q;
    else
        v->lson[v->dad[p]] = q;
    v->dad[p] = LZH_NIL;
}

int lzh_get_bit(lzh_vars_t *v) /* get one bit */
{
    uint32 i;

    while (v->getlen <= 8)
    {
        if (v->input_offset < v->output_size)
            i = read_byte(v->input, &v->input_offset);
        else
            i = 0;

        v->getbuf |= i << (8 - v->getlen);
        v->getlen += 8;
    }
    i = v->getbuf;
    v->getbuf <<= 1;
    v->getlen--;
    return (int)((i & 0x8000) >> 15);
}

int lzh_get_byte(lzh_vars_t *v) /* get one byte */
{
    uint32 i;

    while (v->getlen <= 8)
    {
        if (v->input_offset < v->output_size)
            i = read_byte(v->input, &v->input_offset);
        else
            i = 0;

        v->getbuf |= i << (8 - v->getlen);
        v->getlen += 8;
    }
    i = v->getbuf;
    v->getbuf <<= 8;
    v->getlen -= 8;
    return (int)((i & 0xff00) >> 8);
}

void lzh_putcode(lzh_vars_t *v, int l, uint32 c) /* output c bits of code */
{
    v->putbuf |= c >> v->putlen;
    if ((v->putlen += (uint8)l) >= 8)
    {
        write_byte(v->output, &v->output_offset, (uint8)(v->putbuf >> 8));
        if ((v->putlen -= 8) >= 8)
        {
            write_byte(v->output, &v->output_offset, (uint8)v->putbuf);
            v->codesize += 2;
            v->putlen -= 8;
            v->putbuf = c << (l - v->putlen);
        }
        else
        {
            v->putbuf <<= 8;
            v->codesize++;
        }
    }
}

/* initialization of tree */

void lzh_start_huff(lzh_vars_t *v)
{
    int i, j;

    for (i = 0; i < LZH_N_CHAR; i++)
    {
        v->freq[i] = 1;
        v->son[i] = i + LZH_T;
        v->prnt[i + LZH_T] = i;
    }

    i = 0; j = LZH_N_CHAR;
    while (j <= LZH_R)
    {
        v->freq[j] = v->freq[i] + v->freq[i + 1];
        v->son[j] = i;
        v->prnt[i] = v->prnt[i + 1] = j;
        i += 2; j++;
    }

    v->freq[LZH_T] = 0xffff;
    v->prnt[LZH_R] = 0;
}

/* reconstruction of tree */

void lzh_reconst(lzh_vars_t *v)
{
    int i, j, k;
    uint32 f, l;

    /* collect leaf nodes in the first half of the table */
    /* and replace the freq by (freq + 1) / 2. */
    j = 0;
    for (i = 0; i < LZH_T; i++)
    {
        if (v->son[i] >= LZH_T)
        {
            v->freq[j] = (v->freq[i] + 1) / 2;
            v->son[j] = v->son[i];
            j++;
        }
    }
    /* begin constructing tree by connecting sons */
    for (i = 0, j = LZH_N_CHAR; j < LZH_T; i += 2, j++)
    {
        k = i + 1;
        f = v->freq[j] = v->freq[i] + v->freq[k];
        for (k = j - 1; f < v->freq[k]; k--);
        k++;
        l = (uint32)((j - k) * 2);
        memmove(&v->freq[k + 1], &v->freq[k], l);
        v->freq[k] = f;
        memmove(&v->son[k + 1], &v->son[k], l);
        v->son[k] = i;
    }
    /* connect prnt */
    for (i = 0; i < LZH_T; i++)
    {
        if ((k = v->son[i]) >= LZH_T)
            v->prnt[k] = i;
        else
            v->prnt[k] = v->prnt[k + 1] = i;
    }
}

/* increment frequency of given code by one, and update tree */

void lzh_update(lzh_vars_t *v, int c)
{
    int i, j, k, l;

    if (v->freq[LZH_R] == LZH_MAX_FREQ)
        lzh_reconst(v);

    c = v->prnt[c + LZH_T];
    do
    {
        k = (int)(++v->freq[c]);

        /* if the order is disturbed, exchange nodes */
        if ((uint32)k > v->freq[l = c + 1])
        {
            while ((uint32)k > v->freq[++l]);
            l--;
            v->freq[c] = v->freq[l];
            v->freq[l] = (uint32)k;

            i = v->son[c];
            v->prnt[i] = l;
            if (i < LZH_T) v->prnt[i + 1] = l;

            j = v->son[l];
            v->son[l] = i;

            v->prnt[j] = c;
            if (j < LZH_T) v->prnt[j + 1] = c;
            v->son[c] = j;

            c = l;
        }
    } while ((c = v->prnt[c]) != 0); /* repeat up to root */
}

void lzh_encode_char(lzh_vars_t *v, uint32 c)
{
    uint32 i;
    int j, k;

    i = 0;
    j = 0;
    k = v->prnt[c + LZH_T];

    /* travel from leaf to root */
    do
    {
        i >>= 1;

        /* if node's address is odd-numbered, choose bigger brother node */
        if ((k & 1) == 1) i += 0x8000;

        j++;
    } while ((k = v->prnt[k]) != LZH_R);
    lzh_putcode(v, j, i);
    lzh_update(v, (int)c);
}

void lzh_encode_position(lzh_vars_t *v, uint32 c)
{
    uint32 i;

    /* output upper 6 bits by table lookup */
    i = c >> 6;
    lzh_putcode(v, p_len[i], (uint32)p_code[i] << 8);

    /* output lower 6 bits verbatim */
    lzh_putcode(v, 6, (c & 0x3f) << 10);
}

void lzh_encode_end(lzh_vars_t *v)
{
    if (v->putlen != 0)
    {
        write_byte(v->output, &v->output_offset, (uint8)(v->putbuf >> 8));
        v->codesize++;
    }
}

int lzh_decode_char(lzh_vars_t *v)
{
    uint32 c;

    c = (uint32)v->son[LZH_R];

    /* travel from root to leaf, */
    /* choosing the smaller child node (son[]) if the read bit is 0, */
    /* the bigger (son[]+1} if 1 */
    while (c < LZH_T)
    {
        c += (uint32)lzh_get_bit(v);
        c = (uint32)v->son[c];
    }
    c -= LZH_T;
    lzh_update(v, (int)c);

    return (int)c;
}

int lzh_decode_position(lzh_vars_t *v)
{
    uint32 i, j, c;

    /* recover upper 6 bits from table */
    i = (uint32)lzh_get_byte(v);
    c = (uint32)d_code[i] << 6;
    j = d_len[i];

    /* read lower 6 bits verbatim */
    j -= 2;
    while (j-- != 0)
        i = (uint32)((i << 1) + lzh_get_bit(v));

    return (int)(c | (i & 0x3f));
}

/* compression */

int ADDCALL lzh_compress(uint8 *input, uint8 *output, int size)
{
    lzh_vars_t *v = (lzh_vars_t*)malloc(sizeof(lzh_vars_t));
    lzh_init_compressor(v, input, output, 0, size);

    int i, c, len, r, s, last_match_length;

    v->textsize = 0;           /* rewind and re-read */
    lzh_start_huff(v);
    lzh_init_tree(v);
    s = 0;
    r = LZH_N - LZH_F;
    for (i = s; i < r; i++)
        v->text_buf[i] = 0x20;
    for (len = 0; len < LZH_F && v->input_offset < size; len++)
    {
        c = read_byte(v->input, &v->input_offset);
        v->text_buf[r + len] = (uint8)c;
    }
    v->textsize = len;
    for (i = 1; i <= LZH_F; i++)
        lzh_insert_node(v, r - i);
    lzh_insert_node(v, r);

    do
    {
        if (v->match_length > len)
            v->match_length = len;
        if (v->match_length <= LZH_THRESHOLD)
        {
            v->match_length = 1;
            lzh_encode_char(v, v->text_buf[r]);
        }
        else
        {
            lzh_encode_char(v, (uint32)(255 - LZH_THRESHOLD + v->match_length));
            lzh_encode_position(v, (uint32)(v->match_position));
        }
        last_match_length = v->match_length;
        for (i = 0; i < last_match_length && v->input_offset < size; i++)
        {
            c = read_byte(v->input, &v->input_offset);
            lzh_delete_node(v, s);
            v->text_buf[s] = (uint8)c;
            if (s < LZH_F - 1)
                v->text_buf[s + LZH_N] = (uint8)c;
            s = (s + 1) & (LZH_N - 1);
            r = (r + 1) & (LZH_N - 1);
            lzh_insert_node(v, r);
        }
        v->textsize += i;

        while (i++ < last_match_length)
        {
            lzh_delete_node(v, s);
            s = (s + 1) & (LZH_N - 1);
            r = (r + 1) & (LZH_N - 1);
            if (--len != 0) lzh_insert_node(v, r);
        }
    } while (len > 0);
    lzh_encode_end(v);

    free(v);

    return v->output_offset;
}

void ADDCALL lzh_decompress(uint8 *input, uint8 *output, int offset, int output_size, int *input_size)
{
    lzh_vars_t *v = (lzh_vars_t*)malloc(sizeof(lzh_vars_t));
    lzh_init_compressor(v, input, output, offset, output_size);

	*input_size = 0;

    int i, j, k, r, c;
    int count;

    lzh_start_huff(v);
    for (i = 0; i < LZH_N - LZH_F; i++)
        v->text_buf[i] = 0x20;
    r = LZH_N - LZH_F;
    for (count = 0; count < output_size;)
    {
        c = lzh_decode_char(v);
        if (c < 256)
        {
            write_byte(v->output, &v->output_offset, (uint8)c);
            v->text_buf[r++] = (uint8)c;
            r &= (LZH_N - 1);
            count++;
        }
        else
        {
            i = (r - lzh_decode_position(v) - 1) & (LZH_N - 1);
            j = c - 255 + LZH_THRESHOLD;
            for (k = 0; k < j; k++)
            {
                c = v->text_buf[(i + k) & (LZH_N - 1)];
                write_byte(v->output, &v->output_offset, (uint8)c);
                v->text_buf[r++] = (uint8)c;
                r &= (LZH_N - 1);
                count++;
            }
        }
    }

	*input_size = (v->input_offset - offset);

    free(v);
}

#ifndef ADD_EXPORTS
int main(int argc, char *argv[])
{
    uint8 *input, *output;

	if (argc < 4)
	{
		printf("Usage: tf3cmp.exe input.bin output.bin <type> <offset> [<lzh_out_size>]\n"
			"<type>:\n"
			"\t- dr = decompress RLE\n"
			"\t- dl = decompress LZH (requires <lzh_out_size>)\n"
			"\t- cr = compress RLE\n"
			"\t- cl = compress LZH\n"
			"<offset>: hex offset in the input data\n"
			"<lzh_out_size>: output size of the decompressed data"
			"\n\n");
		return;
	}

    FILE *inf = fopen(argv[1], "rb");

    input = (uint8*)malloc(0x10000);
    output = (uint8*)malloc(0x10000);

    char mode = (argv[3][0]);
    char type = (argv[3][1]);

    if (mode == 'd')
    {
        long offset = strtol(argv[4], NULL, 16);
        fseek(inf, offset, SEEK_SET);
    }

    fread(&input[0], 1, 0x10000, inf);

    int dest_size;
	int input_size;
    if (mode == 'd')
    {
		if (type == 'r')
		{
			dest_size = rle_decompress(input, output, 0, &input_size);
			printf("Decompressing RLE...\n");
		}
        else
        {
            dest_size = (int)strtol(argv[5], NULL, 16);
            lzh_decompress(input, output, 0, dest_size, &input_size);

			printf("Decompressing LZH...\n");
        }
    }
    else
    {
        fseek(inf, 0, SEEK_END);
        int dec_size = ftell(inf);

		if (type == 'r')
		{
			dest_size = rle_compress(input, output, dec_size);
			printf("Compressing RLE...\n");
		}
		else
		{
			dest_size = lzh_compress(input, output, dec_size);
			printf("Compressing LZH...\n");
		}
    }

    if (dest_size != 0)
    {
        FILE *outf = fopen(argv[2], "wb");
        fwrite(&output[0], 1, dest_size, outf);
        fclose(outf);

		printf("Source/Dest: %d->%d", input_size, dest_size);
    }
	else
	{
		printf("Error decompressing data!\n");
	}

    fclose(inf);

    free(input);
    free(output);

    return 0;
}
#endif
