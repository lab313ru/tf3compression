#pragma once

typedef unsigned char uint8;
typedef unsigned short uint16;
typedef unsigned int uint32;

/* You should define ADD_EXPORTS *only* when building the DLL. */
#ifdef ADD_EXPORTS
#define ADDAPI __declspec(dllexport)
#else
#define ADDAPI
#endif

/* Define calling convention in one place, for convenience. */
#define ADDCALL __cdecl

ADDAPI int ADDCALL rle_decompress(const uint8 *input, uint8 *output, int offset);
ADDAPI int ADDCALL rle_compress(const uint8 *input, uint8 *output, int size);
ADDAPI int ADDCALL rle_compressed_size(uint8 *input);

ADDAPI int ADDCALL lzh_compress(uint8 *input, uint8 *output, int size);
ADDAPI void ADDCALL lzh_decompress(uint8 *input, uint8 *output, int output_size);

