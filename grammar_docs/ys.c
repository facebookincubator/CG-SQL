// (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

// Strips the C content of out a .y file for further post processing of the grammar

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

int32_t peeked = EOF;

// read one character
int32_t get() {
  // if we peeked use that instead of reading another character
  if (peeked != EOF) {
    int32_t ch = peeked;
    peeked = -1;
    return ch;
  }

  int32_t ch = getchar();
  if (ch == EOF) {
    exit(0);
  }

  return ch;
}

// peek and the next character and remember it
int32_t peek() {
  if (peeked == EOF)  {
    peeked = getchar();
  }
  return peeked;
}

int32_t in_code = 0;

// emit a character unless we are "in code"
void put(int32_t ch) {
  if (!in_code) {
    putchar(ch);
  }
}

// read up to a matching double quote, handles \" as well.
void process_double_quote() {
  // note, put might be a no-op if in_code
  for (;;) {
    int32_t ch = get();
    if (ch == '\\') {
      put(ch);
      ch = get();
      put(ch);
    }
    else if (ch == '"') {
      put(ch);
      return;
    }
    else {
      put(ch);
    }
  }
}

// read up to a matching single quote, handles \' as well.
void process_single_quote() {
  // note, put might be a no-op if in_code
  for (;;) {
    int32_t ch = get();
    if (ch == '\\') {
      put(ch);
      ch = get();
      put(ch);
    }
    else if (ch == '\'') {
      put(ch);
      return;
    }
    else {
      put(ch);
    }
  }
}

// skip code until we find the matching close brace
void process_code() {
  in_code = 1;
  while (in_code) {
    int32_t ch = get();
    if (ch == '"') {
      process_double_quote(); // output will be suppressed
    }
    else if (ch == '\'') {
      process_single_quote(); // output will be suppressed
    }
    else if (ch == '{') {
      in_code++;
    }
    else if (ch == '}') {
      in_code--;
    }
  }
}

// processes stdin:  skip up to %% then process grammar skipping blocks end at the next %%
int32_t main(int32_t argc, char **argv)
{
  // skip up to the first %%
  for (;;) {
    int32_t ch = get();
    if (ch == '%' && peek() == '%') {
      ch = get(); // read the second %
      break;
    }
  }

  for (;;) {
    int32_t ch = get();
    if (ch == '{') {
      process_code();
    }
    else if (ch == '"') {
      put(ch);
      process_double_quote();
    }
    else if (ch == '\'') {
      put(ch);
      process_single_quote();
    }
    else if (ch == '%' && peek() == '%') {
      // end of rules, ignore the code after
      break;
    }
    else {
      put(ch);
    }
  }
}
