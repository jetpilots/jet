#ifndef __TINYC__
#include "../deps/blake3.c"
#endif

int file_hash_equal(char* file1, char* file2) {
#ifdef __TINYC__
  return 0;
#else
  String data1 = slurp(file1);
  String data2 = slurp(file2);
  blake3_hasher hasher;
  uint8_t output1[BLAKE3_OUT_LEN], output2[BLAKE3_OUT_LEN];

  blake3_hasher_init(&hasher);
  blake3_hasher_update(&hasher, data1.ref, data1.len);
  blake3_hasher_finalize(&hasher, output1, BLAKE3_OUT_LEN);

  blake3_hasher_init(&hasher);
  blake3_hasher_update(&hasher, data2.ref, data2.len);
  blake3_hasher_finalize(&hasher, output2, BLAKE3_OUT_LEN);

  uint64_t *u1 = (uint64_t*)output1, *u2 = (uint64_t*)output2;

  return !((u1[0] ^ u2[0]) //
      | (u1[1] ^ u2[1]) //
      | (u1[2] ^ u2[2]) //
      | (u1[3] ^ u2[3]));
#endif
}
