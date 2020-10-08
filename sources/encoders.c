/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cql.h"
#include "charbuf.h"
#include "encoders.h"

// This converts from SQL string literal format to plain output
// Note that SQL string literal have no escapes except for double quote
cql_noexport void cg_decode_string_literal(CSTR str, charbuf *output) {
  const char quote = '\'';
  CSTR p = str+1;  // the first character is the quote itself

  while (p[0]) {
    if (p[0] == quote && p[1] == quote) {
      bputc(output, quote);
      p++;
    }
    else if (p[0] == quote) {
      break;
    }
    else {
      bputc(output, p[0]);
    }
    p++;
  }
}

// This converts from a plain string to sql string literal
// Note SQL string literals have no escape sequences other than '' -> '
cql_noexport void cg_encode_string_literal(CSTR str, charbuf *output) {
  const char quote = '\'';
  const char *p = str;

  bputc(output, quote);

  for ( ;p[0]; p++) {
    if (p[0] == quote) bputc(output, quote);
    bputc(output, p[0]);
  }

  bputc(output, quote);
}

static void emit_hex_digit(uint32_t ch, charbuf *output) {
  Contract(ch >= 0 && ch <= 15);
  if (ch < 10) {
    bputc(output, (char)(ch + '0'));
  }
  else {
    bputc(output, (char)(ch - 10 + 'a'));
  }
}

// This converts from a plain string to C string literal
cql_noexport void cg_encode_char_as_c_string_literal(char c, charbuf *output) {
  const char quote = '"';
  const char backslash = '\\';

  switch (c) {
    case '\"':  bputc(output, backslash); bputc(output, quote); break;
    case '\a':  bputc(output, backslash); bputc(output, 'a'); break;
    case '\b':  bputc(output, backslash); bputc(output, 'b'); break;
    case '\f':  bputc(output, backslash); bputc(output, 'f'); break;
    case '\n':  bputc(output, backslash); bputc(output, 'n'); break;
    case '\r':  bputc(output, backslash); bputc(output, 'r'); break;
    case '\t':  bputc(output, backslash); bputc(output, 't'); break;
    case '\v':  bputc(output, backslash); bputc(output, 'v'); break;
    case '\\':  bputc(output, c); bputc(output, c); break;
    default  :
      // note: 0x80 - 0xff will be negative and are hence covered by this test
      if (c < 32) {
        uint32_t ch = (uint32_t)c;
        ch &= 0xff;
        bprintf(output, "\\x");
        emit_hex_digit(ch >> 4, output);
        emit_hex_digit(ch & 0xf, output);
      }
      else {
        bputc(output, c);
      }
  }
}

// This converts from a plain string to json string literal (fewer escapes available/needed)
//
// From the spec, the valid single escape characters are
// SingleEscapeCharacter :: one of
//      ' " \ b f n r t v
//
// \v should be legal but it is avoided because the python validator
// doesn't support it. We generate all of the others if needed
// but \' is never needed as we always use double quotes.
//
// likewise the spec says:
//
// UnicodexEscapeSequence ::
//      u HexDigit HexDigit HexDigit HexDigit
//
cql_noexport void cg_encode_char_as_json_string_literal(char c, charbuf *output) {
  const char quote = '"';
  const char backslash = '\\';

  switch (c) {
    case '\"':  bputc(output, backslash); bputc(output, quote); break;
    case '\\':  bputc(output, c); bputc(output, c); break;
    case '\b':  bputc(output, backslash); bputc(output, 'b'); break;
    case '\f':  bputc(output, backslash); bputc(output, 'f'); break;
    case '\n':  bputc(output, backslash); bputc(output, 'n'); break;
    case '\r':  bputc(output, backslash); bputc(output, 'r'); break;
    case '\t':  bputc(output, backslash); bputc(output, 't'); break;
    default  :
      if (c > 0 && c < 32) {
        uint32_t ch = (uint32_t)c;
        ch &= 0xff;
        bprintf(output, "\\u00");
        emit_hex_digit(ch >> 4, output);
        emit_hex_digit(ch & 0xf, output);
      }
      else {
        bputc(output, c);
      }
  }
}

// This converts from a plain string to C string literal
cql_noexport void cg_encode_c_string_literal(CSTR str, charbuf *output) {
  const char quote = '"';
  const char *p = str;

  bputc(output, quote);

  for ( ;p[0]; p++) {
    cg_encode_char_as_c_string_literal(p[0], output);
  }
  bputc(output, quote);
}

// This converts from a plain string to JSON string literal
cql_noexport void cg_encode_json_string_literal(CSTR str, charbuf *output) {
  const char quote = '"';
  const char *p = str;

  bputc(output, quote);

  for ( ;p[0]; p++) {
    cg_encode_char_as_json_string_literal(p[0], output);
  }
  bputc(output, quote);
}


// convert a single hex character to an integer
static uint32_t hex_to_int(char c) {
  uint32_t ch = (uint32_t)(unsigned char)c;
  if (ch >= '0' && ch <= '9')
    return ch - '0';

  if (ch >= 'a' && ch <= 'f')
     return ch - 'a' + 10;

  // this is all that's left
  Contract(ch >= 'A' && ch <= 'F');
  return ch - 'A' + 10;
}

static void decode_hex_escape(CSTR *pstr, charbuf *output) {
  Contract(pstr);
  Contract(**pstr == 'x');
  CSTR p = *pstr;
  p++; // skip the 'x'

  // the escape sequence is not interpreted as hex if not well formed
  if (isxdigit(p[0]) && isxdigit(p[1])) {
    char ch = (char)(hex_to_int(p[0]) * 16 + hex_to_int(p[1]));

    // No embedded nulls, all the strings are null terminated so this will just screw everything up.
    if (ch != 0) {
      bputc(output, ch);
    }
    // note, the main loop will skip an additional character as a matter of course
    // so the second byte we do not pass over
    p++;

    // the input will be left on the 'x' if it wasn't well formed, which is the skipped as usual
    *pstr = p;
  }
}

cql_noexport void cg_decode_c_string_literal(CSTR str, charbuf *output) {
  // don't call me with strings that are not properly "" delimited
  const char quote = '"';
  const char backslash = '\\';

  Contract(str[0] == quote);
  CSTR p = str + 1;

  for ( ;p[0]; p++) {
    if (p[0] == quote) {
      break;
    }

    if (p[0] != backslash) {
      bputc(output, p[0]);
      continue;
    }

    p++;
    switch (p[0]) {
      case 'a': bputc(output, '\a'); break;
      case 'b': bputc(output, '\b'); break;
      case 'f': bputc(output, '\f'); break;
      case 'n': bputc(output, '\n'); break;
      case 'r': bputc(output, '\r'); break;
      case 't': bputc(output, '\t'); break;
      case 'v': bputc(output, '\v'); break;
      case 'x': decode_hex_escape(&p, output); break;
      default : bputc(output, p[0]); break;
    }
  }

  // don't call me with strings that are not properly "" delimited
  Contract(p[0] == quote);
}

// When we need to execute SQL, we get the text of the SQL from the gen_ functions.
// Those functions return plaintext.  We need to quote that text so it can appear
// in a C string literal.  To do this we need to:
//  * put quotes around it
//  * do C string processing
//  * turn linefeeds into spaces (we break the string here for readability)
//    * or remove the unquoted linefeeds and indentation
cql_noexport void cg_pretty_quote_plaintext(CSTR str, charbuf *output, uint32_t flags) {
  Contract(str);

  const char squote = '\'';
  bool_t inQuote = 0;
  bool_t multi_line = !!(flags & PRETTY_QUOTE_MULTI_LINE);
  bool_t for_json = !!(flags & PRETTY_QUOTE_JSON);

  bputc(output, '"');
  for (CSTR p = str; p[0]; p++) {
    // figure out if we're in quoted sql text, if we are then any newlines we see
    // are part of the string not part of our multi-line formatting.  They have to be escaped.
    if (!inQuote && p[0] == squote) {
      inQuote = 1;
      bprintf(output, "'");
    }
    else if (inQuote && p[0] == squote && p[1] == squote) {
      // escaped '' is escaped quote, stay in quoted mode
      bprintf(output, "''");
      // gobble the second quote since we just emitted it already
      // this way it has no way to fool us into leaving quoted mode (a previous bug)
      p++;
    }
    else if (inQuote && p[0] == squote) {
      inQuote = 0;
      bprintf(output, "'");
    }
    else if (!inQuote && p[0] == '\n') {
      if (multi_line) {
        // convert the newline to a space, break the string into multi-part literal
        bprintf(output, " \"\n  ");

        // use the embedded spaces to indent the string literal not to make the string fatter
        while (p[1] == ' ') {
          p++;
          bputc(output, ' ');
        }
        bputc(output, '"');
      }
      else {
        // emit the newline as a single space
        bputc(output, ' ');

        // eat any spaces that follow the newline
        while (p[1] == ' ') {
          p++;
        }
      }
    }
    else {
      if (for_json) {
        cg_encode_char_as_json_string_literal(p[0], output);
      }
      else {
        cg_encode_char_as_c_string_literal(p[0], output);
      }
    }
  }
  bputc(output, '"');
}
