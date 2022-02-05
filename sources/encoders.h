/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

// string literal swizzlers

#define PRETTY_QUOTE_SINGLE_LINE 0
#define PRETTY_QUOTE_MULTI_LINE 1
#define PRETTY_QUOTE_C 0
#define PRETTY_QUOTE_JSON 2

cql_noexport void cg_decode_string_literal(CSTR _Nonnull str, charbuf *_Nonnull output);
cql_noexport void cg_encode_string_literal(CSTR _Nonnull str, charbuf *_Nonnull output);
cql_noexport void cg_encode_char_as_c_string_literal(char c, charbuf *_Nonnull output);
cql_noexport void cg_encode_char_as_json_string_literal(char c, charbuf *_Nonnull output);

cql_noexport void cg_encode_json_string_literal(CSTR _Nonnull str, charbuf *_Nonnull output);
cql_noexport void cg_encode_c_string_literal(CSTR _Nonnull str, charbuf *_Nonnull output);
cql_noexport void cg_decode_c_string_literal(CSTR _Nonnull str, charbuf *_Nonnull output);
cql_noexport void cg_pretty_quote_plaintext(CSTR _Nonnull str, charbuf *_Nonnull output, uint32_t flags);
cql_noexport void cg_remove_star_slash(charbuf *_Nonnull b);
