/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#if defined(CQL_AMALGAM_LEAN) && !defined(CQL_AMALGAM_SEM)

// stubs to avoid link errors (none needed)

#else

#include "printf.h"

// Declares the single-character C string `name` given a character `c`.
#define CSTR_OF_CHAR(name, c) \
  char name[2] = {0}; \
  name[0] = c;

// There are seven options flags that can be associated with a substitution in a
// SQLite format string.
typedef enum {
  PRINTF_FLAGS_NONE  = 0,
  PRINTF_FLAGS_MINUS = 1 << 0,
  PRINTF_FLAGS_PLUS  = 1 << 1,
  PRINTF_FLAGS_SPACE = 1 << 2,
  PRINTF_FLAGS_ZERO  = 1 << 3,
  PRINTF_FLAGS_HASH  = 1 << 4,
  PRINTF_FLAGS_COMMA = 1 << 5,
  PRINTF_FLAGS_BANG  = 1 << 6,
} printf_flags;

// Parsing a format string involves the use of a state machine with the
// following states. The name of each state indicates what we're looking for
// next, e.g., the start of a substitution, a percent sign following the start
// ('%%'), a flag, a width specification, et cetera.
typedef enum {
  PRINTF_STATE_START,
  PRINTF_STATE_PERCENT,
  PRINTF_STATE_FLAG,
  PRINTF_STATE_WIDTH,
  PRINTF_STATE_WIDTH_NUMERIC,
  PRINTF_STATE_DOT,
  PRINTF_STATE_PRECISION,
  PRINTF_STATE_LENGTH_LONG,
  PRINTF_STATE_LENGTH_LONG_LONG,
  PRINTF_STATE_TYPE,
} printf_state;

// A width in SQLite can be either numeric or '*'. In the latter case, two
// arguments are required for the substition, the first of which will be the
// width.
typedef enum {
  PRINTF_WIDTH_NONE,
  PRINTF_WIDTH_NUMERIC,
  PRINTF_WIDTH_STAR,
} printf_width;

// A length specifier can be absent (indicating the default, 32-bit length), 'l'
// (which does nothing in SQLite and therefore also indicates 32 bits), or 'll'
// (indicating 64 bits).
typedef enum {
  PRINTF_LENGTH_DEFAULT,
  PRINTF_LENGTH_LONG,
  PRINTF_LENGTH_LONG_LONG,
} printf_length;

struct printf_iterator {
  // The AST to use for error reporting. This should be the string literal
  // `ast_node` itself so that problems with the format string are reported with
  // the correct location.
  ast_node *ast;
  // The flags associated with the current substitution.
  printf_flags flags;
  // A pointer into the null-terminated format string that is being parsed. This
  // string must NOT simply be the string extracted from the `ast_node`, but
  // rather a decoded version without the leading and trailing quotes.
  CSTR format_string;
  // The length associated with the current substitution.
  printf_length length;
  // The core type associated with the type specifier of the current
  // substitution (e.g., `SEM_TYPE_INTEGER`), `SEM_TYPE_PENDING` if we're
  // currently in the middle of parsing a substitution, `SEM_TYPE_OK` if we've
  // finished parsing the format string successfully, or `SEM_TYPE_ERROR` if we
  // encountered an error. In the lattermost two cases, the iterator is finished
  // and `printf_iterator_next` must not be called again.
  sem_t sem_type;
  // The current state of parsing for the current substitution.
  printf_state state;
  // The width associated with the current substitution.
  printf_width width;
};

// We need to provide `sizeof_printf_iterator` because `printf_iterator` is
// abstract in the header.
size_t sizeof_printf_iterator = sizeof(printf_iterator);

// Initializes a `printf_iterator`.
cql_noexport void printf_iterator_init(printf_iterator *iterator, ast_node *format_strlit, CSTR format_string) {
  Contract(!format_strlit || is_strlit(format_strlit));

  iterator->ast = format_strlit;
  iterator->flags = PRINTF_FLAGS_NONE;
  iterator->format_string = format_string;
  iterator->length = PRINTF_LENGTH_DEFAULT;
  iterator->sem_type = SEM_TYPE_PENDING;
  iterator->state = PRINTF_STATE_START;
  iterator->width = PRINTF_WIDTH_NONE;
}

// Given a character, returns the associated flag (or `PRINTF_FLAGS_NONE` if the
// character does not correspond to a flag).
static printf_flags printf_flag_of_char(char c) {
  switch (c) {
    case '-':
      return PRINTF_FLAGS_MINUS;
    case '+':
      return PRINTF_FLAGS_PLUS;
    case ' ':
      return PRINTF_FLAGS_SPACE;
    case '0':
      return PRINTF_FLAGS_ZERO;
    case '#':
      return PRINTF_FLAGS_HASH;
    case ',':
      return PRINTF_FLAGS_COMMA;
    case '!':
      return PRINTF_FLAGS_BANG;
    default:
      return PRINTF_FLAGS_NONE;
  }
}

// Indicates an error in the format string and sets `SEM_TYPE_ERROR`.
static void printf_iterator_error(printf_iterator *iterator, CSTR msg, CSTR subject) {
  if (iterator->ast) {
    report_error(iterator->ast, msg, subject);
    record_error(iterator->ast);
  }
  iterator->sem_type = SEM_TYPE_ERROR;
}

// Returns `true` if the character corresponds to one of the seven possible
// flags, else `false`.
static bool_t printf_is_flag_char(char c) {
  return printf_flag_of_char(c) != PRINTF_FLAGS_NONE;
}

// Records that a character corresponding to one of the seven possible flags is
// associated with the current substituion. This must not be called with a
// character that does not correspond to a flag. Sets `SEM_TYPE_ERROR` if the
// flag character is a duplicate or if there is an invalid combination of flags.
static void printf_iterator_add_flag_char(printf_iterator *iterator, char c) {
  Contract(iterator);
  Contract(iterator->sem_type == SEM_TYPE_PENDING);
  Contract(iterator->state == PRINTF_STATE_FLAG);

  printf_flags flag = printf_flag_of_char(c);
  Invariant(flag != PRINTF_FLAGS_NONE);

  if (iterator->flags & flag) {
    CSTR_OF_CHAR(flag_string, c);
    printf_iterator_error(iterator, "CQL0411: duplicate flag in substitution", flag_string);
    return;
  }

  printf_flags plus_or_space = PRINTF_FLAGS_PLUS | PRINTF_FLAGS_SPACE;
  if ((iterator->flags & plus_or_space) && (flag & plus_or_space)) {
    // We already had a plus or space, and we just got a plus or space, and we
    // know the one we just got is not a duplicate of what we already had
    // because we just checked, so now we have both.
    printf_iterator_error(iterator, "CQL0412: cannot combine '+' flag with space flag", NULL);
    return;
  }

  iterator->flags |= flag;
}

// Records the width specifier for the current substition. Sets `SEM_TYPE_ERROR`
// if the substitution has no width but one is required for a previously
// recorded flag to make sense.
static void printf_set_width(printf_iterator *iterator, printf_width width) {
  Contract(iterator);
  Contract(iterator->sem_type == SEM_TYPE_PENDING);
  Contract(iterator->state == PRINTF_STATE_WIDTH);
  Contract(iterator->width == PRINTF_WIDTH_NONE);

  switch (width) {
    case PRINTF_WIDTH_NONE:
      if ((iterator->flags & (PRINTF_FLAGS_MINUS | PRINTF_FLAGS_ZERO))) {
        CSTR flag_string = (iterator->flags & PRINTF_FLAGS_MINUS) ? "-" : "0";
        printf_iterator_error(iterator, "CQL0413: width required when using flag in substitution", flag_string);
        return;
      }
      break;
    case PRINTF_WIDTH_NUMERIC:
      break;
    case PRINTF_WIDTH_STAR:
      break; 
  }

  iterator->width = width;
}

// Sets the length specifier for the current substitution. Sets `SEM_TYPE_ERROR`
// if the specifier is `PRINTF_STATE_LENGTH_LONG` (as 'l' serves no purpose in
// SQLite) or if a length specifier has been combined with a flag that doesn't
// make sense with a length specifier.
static void printf_set_length(printf_iterator *iterator, printf_length length) {
  Contract(iterator);
  Contract(iterator->sem_type == SEM_TYPE_PENDING);
  Contract(iterator->state == PRINTF_STATE_LENGTH_LONG || iterator->state == PRINTF_STATE_LENGTH_LONG_LONG);

  switch (length) {
    case PRINTF_LENGTH_DEFAULT:
      break;
    case PRINTF_LENGTH_LONG:
      printf_iterator_error(iterator, "CQL0414: 'l' length specifier has no effect; consider 'll' instead", NULL);
      return;
    case PRINTF_LENGTH_LONG_LONG:
      if ((iterator->flags & PRINTF_FLAGS_BANG)) {
        printf_iterator_error(iterator, "CQL0415: length specifier cannot be combined with '!' flag", NULL);
        return;
      }
      break;
  }

  iterator->length = length;
}

// Sets the type specifier associated with the character provided for the
// current substitution. Sets `SEM_TYPE_ERROR` if the type specifier is not
// compatible with the previously recorded flags or length specifier, or if the
// type specifier is not allowed in CQL, or if the character provided does not
// correspond to any type specifier.
static void printf_iterator_set_type_char(printf_iterator *iterator, char c) {
  Contract(iterator);
  Contract(iterator->sem_type == SEM_TYPE_PENDING);
  Contract(iterator->state == PRINTF_STATE_TYPE);

  CSTR_OF_CHAR(type_string, c);

  // '-' works with all possible type specifications.
  printf_flags valid_flags = PRINTF_FLAGS_MINUS;
  bool_t allows_length_specifier;

  switch (c) {
    case 'd': case 'i': {
      allows_length_specifier = true;
      valid_flags |= PRINTF_FLAGS_PLUS;
      valid_flags |= PRINTF_FLAGS_SPACE;
      valid_flags |= PRINTF_FLAGS_ZERO;
      valid_flags |= PRINTF_FLAGS_COMMA;
      if (iterator->length == PRINTF_LENGTH_LONG_LONG) {
        iterator->sem_type = SEM_TYPE_LONG_INTEGER;
      } else {
        iterator->sem_type = SEM_TYPE_INTEGER;
      }
      break;
    }
    case 'u':
      allows_length_specifier = true;
      valid_flags |= PRINTF_FLAGS_ZERO;
      if (iterator->length == PRINTF_LENGTH_LONG_LONG) {
        iterator->sem_type = SEM_TYPE_LONG_INTEGER;
      } else {
        iterator->sem_type = SEM_TYPE_INTEGER;
      }
      break;
    case 'f': case 'e': case 'E': case 'g': case 'G':
      allows_length_specifier = false;
      valid_flags |= PRINTF_FLAGS_ZERO;
      valid_flags |= PRINTF_FLAGS_BANG;
      valid_flags |= PRINTF_FLAGS_HASH;
      iterator->sem_type = SEM_TYPE_REAL;
      break;
    case 'x': case 'X': case 'o':
      allows_length_specifier = true;
      valid_flags |= PRINTF_FLAGS_ZERO;
      valid_flags |= PRINTF_FLAGS_HASH;
      if (iterator->length == PRINTF_LENGTH_LONG_LONG) {
        iterator->sem_type = SEM_TYPE_LONG_INTEGER;
      } else {
        iterator->sem_type = SEM_TYPE_INTEGER;
      }
      break;
    case 's':
      allows_length_specifier = false;
      valid_flags |= PRINTF_FLAGS_BANG;
      iterator->sem_type = SEM_TYPE_TEXT;
      break;
    case 'c': case 'z': case 'p': case 'n': case 'q': case 'Q': case 'w': {
      // NOTE: 'c' could be supported with codegen changes. It is presently
      // disallowed because it requires a TEXT argument when used in an SQL
      // context, yet it requires an integer argument when used via
      // `sqlite3_mprintf`. The code generator currently cannot handle the
      // latter case correctly.
      printf_iterator_error(iterator, "CQL0416: type specifier not allowed in CQL", type_string);
      return;
    }
    default:
      printf_iterator_error(iterator, "CQL0417: unrecognized type specifier", type_string);
      return;
  }

  if ((iterator->flags | valid_flags) != valid_flags) {
    printf_iterator_error(iterator, "CQL0418: type specifier combined with inappropriate flags", type_string);
    return;
  }

  if (iterator->length != PRINTF_LENGTH_DEFAULT && !allows_length_specifier) {
    printf_iterator_error(iterator, "CQL0419: type specifier cannot be combined with length specifier", type_string);
    return;
  }
}

// Resets the iterator after successfully parsing one substitution to prepare
// for the next call to `printf_iterator_next`. This must not be called if in
// the middle of a substitution with a '*' width specifier;
// `printf_iterator_suspend_for_star` should be used instead.
static void printf_iterator_reset(printf_iterator *iterator) {
  Contract(iterator);
  Contract(iterator->state == PRINTF_STATE_TYPE);
  Contract(iterator->sem_type != SEM_TYPE_ERROR);
  Contract(iterator->sem_type != SEM_TYPE_OK);
  Contract(iterator->sem_type != SEM_TYPE_PENDING);

  iterator->flags = PRINTF_FLAGS_NONE;
  iterator->format_string++;
  iterator->length = PRINTF_LENGTH_DEFAULT;
  iterator->sem_type = SEM_TYPE_PENDING;
  iterator->state = PRINTF_STATE_START;
  iterator->width = PRINTF_WIDTH_NONE;
}

// Prepares the iterator for the next call to `printf_iterator_next` after
// encountering a '*' width specifier.
static void printf_iterator_suspend_for_star(printf_iterator *iterator) {
  Contract(iterator);
  Contract(iterator->state == PRINTF_STATE_WIDTH);
  Contract(iterator->width == PRINTF_WIDTH_NONE);
  Contract(*iterator->format_string == '*');

  // The '*' width requires two arguments for the substitution instead of one,
  // the first of which will be the width. For example, the following two uses
  // of printf are equivalent:
  //
  //   printf("%10d\n", 42);
  //   printf("%*d\n, 10, 42);
  //
  // It therefore follows that we need to return the fact that we need an
  // integer, and then be ready to resume parsing the rest of the current
  // substitution later.
  printf_set_width(iterator, PRINTF_WIDTH_STAR);
  // Setting the width to `PRINTF_WIDTH_STAR` cannot fail.
  Invariant(iterator->sem_type != SEM_TYPE_ERROR);
  // We'll resume looking for a dot after the star.
  iterator->state = PRINTF_STATE_DOT;
  // Consume the '*' character.
  iterator->format_string++;
}

// Returns the type of the next substitution, else `SEM_TYPE_OK` if no
// substitutions remain or `SEM_TYPE_ERROR` in the case of an error.
cql_noexport sem_t printf_iterator_next(printf_iterator *iterator) {
  Contract(iterator);
  Contract(iterator->sem_type == SEM_TYPE_PENDING);
  // We should either be at the start of a substituion or resuming a
  // substitution with a '*' width specifier.
  Contract(iterator->state == PRINTF_STATE_START || iterator->width == PRINTF_WIDTH_STAR);

  for (;;) {
    // If we encountered an error or hit the end of the string, stop.
    if (iterator->sem_type != SEM_TYPE_PENDING) {
      return iterator->sem_type;
    }

    // Read the current character in the format string.
    char c = *iterator->format_string;

    // Check if we're at the end of the string. If so, stop.
    if (c == '\0') {
      if (iterator->state == PRINTF_STATE_START) {
        // We hit the end while not within a substitution, so we're simply done.
        iterator->sem_type = SEM_TYPE_OK;
      } else {
        // We hit the end in the middle of a substitution, so the substitution
        // is incomplete and the format string is invalid.
        printf_iterator_error(iterator, "CQL0420: incomplete substitution in format string", NULL);
      }
      return iterator->sem_type;
    }

    // Here, we dispatch appropriately based on the current state. If the
    // current character should be consumed, we `break` to jump out of the
    // switch and advance the string to the next character at the end of the for
    // loop. If we want to go onto another step without consuming the current
    // character, we `continue` instead to jump back to the top of the for loop
    // without advancing the string.
    switch (iterator->state) {
      case PRINTF_STATE_START:
        if (c == '%') {
          iterator->state = PRINTF_STATE_PERCENT;
        }
        break;
      case PRINTF_STATE_PERCENT:
        if (c == '%') {
          iterator->state = PRINTF_STATE_START;
          break;
        }
        iterator->state = PRINTF_STATE_FLAG;
        continue;
      case PRINTF_STATE_FLAG:
        if (printf_is_flag_char(c)) {
          printf_iterator_add_flag_char(iterator, c);
          break;
        }
        iterator->state = PRINTF_STATE_WIDTH;
        continue;
      case PRINTF_STATE_WIDTH:
        if (c >= '0' && c <= '9') {
          printf_set_width(iterator, PRINTF_WIDTH_NUMERIC);
          iterator->state = PRINTF_STATE_WIDTH_NUMERIC;
          break;
        }
        if (c == '*') {
          // Return the fact that we need an integer and prepare to resume
          // parsing the rest of the substitution later.
          printf_iterator_suspend_for_star(iterator);
          return SEM_TYPE_INTEGER;
        }
        printf_set_width(iterator, PRINTF_WIDTH_NONE);
        iterator->state = PRINTF_STATE_DOT;
        continue;
      case PRINTF_STATE_WIDTH_NUMERIC:
        if (c >= '0' && c <= '9') {
          break;
        }
        iterator->state = PRINTF_STATE_DOT;
        continue;
      case PRINTF_STATE_DOT:
        if (c == '.') {
          iterator->state = PRINTF_STATE_PRECISION;
          break;
        }
        iterator->state = PRINTF_STATE_LENGTH_LONG;
        continue;
      case PRINTF_STATE_PRECISION:
        if (c >= '0' && c <= '9') {
          break;
        }
        iterator->state = PRINTF_STATE_LENGTH_LONG;
        continue;
      case PRINTF_STATE_LENGTH_LONG:
        if (c == 'l') {
          iterator->state = PRINTF_STATE_LENGTH_LONG_LONG;
          break;
        }
        printf_set_length(iterator, PRINTF_LENGTH_DEFAULT);
        iterator->state = PRINTF_STATE_TYPE;
        continue;
      case PRINTF_STATE_LENGTH_LONG_LONG:
        if (c == 'l') {
          printf_set_length(iterator, PRINTF_LENGTH_LONG_LONG);
          iterator->state = PRINTF_STATE_TYPE;
          break;
        }
        printf_set_length(iterator, PRINTF_LENGTH_LONG);
        iterator->state = PRINTF_STATE_TYPE;
        continue;
      case PRINTF_STATE_TYPE:
        printf_iterator_set_type_char(iterator, c);
        sem_t sem_type = iterator->sem_type;
        if (sem_type != SEM_TYPE_ERROR) {
          printf_iterator_reset(iterator);
        }
        return sem_type;
    }

    // Consume the current character and continue.
    iterator->format_string++;
  }
}

#endif
