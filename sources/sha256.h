/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// MIT license applies only to the extent that this file has been modified
// from the public domain version, which, as it happens, is barely at all.
//
// The original header with claritications from its github readme is
// included here including the public domain nature of the original code,
// and attribution to the original author.

/*********************************************************************
* Filename:   sha256.h
* Author:     Brad Conte (brad AT bradconte.com)
* Release:    This code is released into the public domain free of any
*             restrictions. The author requests acknowledgement if the
*             code is used, but does not require it.
*             This code is provided free of any liability and without
*             any quality claims by the author.
* Disclaimer: This code is presented "as is" without any guarantees.
* Details:    Defines the API for the corresponding SHA1 implementation.
*********************************************************************/

#ifndef SHA256_H
#define SHA256_H

/*************************** HEADER FILES ***************************/
#include <stddef.h>
#include <inttypes.h>

/****************************** MACROS ******************************/
#define SHA256_BLOCK_SIZE 32            // SHA256 outputs a 32 byte digest

/**************************** DATA TYPES ****************************/
typedef uint8_t SHA256_BYTE;   // 8-bit byte
typedef uint32_t SHA256_WORD;   // 32-bit word (portable type)

typedef struct {
	SHA256_BYTE data[64];
	SHA256_WORD datalen;
	uint64_t bitlen;
	SHA256_WORD state[8];
} SHA256_CTX;

/*********************** FUNCTION DECLARATIONS **********************/
void sha256_init(SHA256_CTX *ctx);
void sha256_update(SHA256_CTX *ctx, const SHA256_BYTE data[], size_t len);
void sha256_final(SHA256_CTX *ctx, SHA256_BYTE hash[]);

#endif   // SHA256_H
