/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cql.h"
#include "symtab.h"
#include "bytebuf.h"
#include "charbuf.h"

static void symtab_rehash(symtab *syms);

static void set_payload(symtab *syms) {
  syms->payload = (symtab_entry *)calloc(syms->capacity, sizeof(symtab_entry));
}

static uint32_t hash_case_insens(const char *sym) {
  const unsigned char *bytes = (const unsigned char *)sym;
  uint64_t hash = 0;
  while (*bytes) {
    unsigned char byte = *bytes | 0x20;
    hash = ((hash << 5) | (hash >> 27)) ^ byte;
    bytes++;
  }
  return (uint32_t)(hash ^ (hash >>32));
}

static int32_t cmp_case_insens(const char *s1, const char *s2) {
  return (int32_t)Strcasecmp(s1, s2);
}

static int32_t cmp_case_sens(const char *s1, const char *s2) {
  return (int32_t)strcmp(s1, s2);
}

static uint32_t hash_case_sens(const char *sym) {
  const unsigned char *bytes = (const unsigned char *)sym;
  uint64_t hash = 0;
  while (*bytes) {
    unsigned char byte = *bytes;
    hash = ((hash << 5) | (hash >> 27)) ^ byte;
    bytes++;
  }
  return (uint32_t)(hash ^ (hash >>32));
}

cql_noexport symtab *symtab_new() {
  symtab *syms = _new(symtab);
  syms->count = 0;
  syms->capacity = SYMTAB_INIT_SIZE;
  syms->hash = hash_case_insens;
  syms->cmp = cmp_case_insens;
  syms->teardown = NULL;
  set_payload(syms);
  return syms;
}

cql_noexport symtab *symtab_new_case_sens() {
  symtab *syms = symtab_new();
  syms->hash = hash_case_sens;
  syms->cmp = cmp_case_sens;
  return syms;
}

cql_noexport void symtab_delete(symtab *syms) {
  if (syms->teardown) {
    for (int32_t i = 0; i < syms->capacity; i++) {
      void *val = syms->payload[i].val;
      if (val) {
        syms->teardown(val);
      }
    }
  }
  free(syms->payload);
  free(syms);
}

cql_noexport bool_t symtab_add(symtab *syms, const char *sym_new, void *val_new) {
  uint32_t hash = syms->hash(sym_new);
  uint32_t offset = hash % syms->capacity;
  symtab_entry *payload = syms->payload;

  for (;;) {
    const char *sym = payload[offset].sym;
    if (!sym) {
      payload[offset].sym = sym_new;
      payload[offset].val = val_new;

      syms->count++;
      if (syms->count > syms->capacity * SYMTAB_LOAD_FACTOR) {
        symtab_rehash(syms);
      }

      return true;
    }

    if (!syms->cmp(sym, sym_new)) {
      return false;
    }

    offset++;
    if (offset >= syms->capacity) {
      offset = 0;
    }
  }
}

cql_noexport symtab_entry *symtab_find(symtab *syms, const char *sym_needed) {
  if (!syms) {
    return NULL;
  }

  uint32_t hash = syms->hash(sym_needed);
  uint32_t offset = hash % syms->capacity;
  symtab_entry *payload = syms->payload;

  for (;;) {
    const char *sym = syms->payload[offset].sym;
    if (!sym) {
      return NULL;
    }

    if (!syms->cmp(sym, sym_needed)) {
      return &payload[offset];
    }

    offset++;
    if (offset >= syms->capacity) {
      offset = 0;
    }
  }
}

static void symtab_rehash(symtab *syms) {
  uint32_t old_capacity = syms->capacity;
  symtab_entry *old_payload = syms->payload;

  syms->count = 0;
  syms->capacity *= 2;
  set_payload(syms);

  for (uint32_t i = 0; i < old_capacity; i++) {
    const char *sym = old_payload[i].sym;
    if (!sym) {
      continue;
    }

    symtab_add(syms, old_payload[i].sym, old_payload[i].val);
  }

  free(old_payload);
}

// patternlint-disable-next-line prefer-sized-ints-in-msys
cql_noexport int default_symtab_comparator(symtab_entry *entry1, symtab_entry *entry2) {
  return strcmp(entry1->sym, entry2->sym);
}

// patternlint-disable-next-line prefer-sized-ints-in-msys
cql_noexport symtab_entry *symtab_copy_sorted_payload(symtab *syms, int (*comparator)(symtab_entry *entry1, symtab_entry *entry2)) {
  uint32_t count = syms->count;
  size_t size = sizeof(symtab_entry);
  symtab_entry *sorted = calloc(count, size);
  int32_t found = 0;
  for (int32_t i = 0; i < syms->capacity; i++) {
    // skip the null syms in our copy
    if (syms->payload[i].sym) {
      sorted[found++] = syms->payload[i];
    }
  }
  // now sort the nonnull values
  // patternlint-disable-next-line prefer-sized-ints-in-msys
  qsort(sorted, count, size, (int (*)(const void *, const void *))comparator);
  return sorted;
}

// adding special case code for two common cases
//  * a symbol table with payload of symbol tables
//  * a symbol table with payload of bytebuffers

static void symtab_teardown(void *val) {
  symtab_delete(val);
}

cql_noexport symtab *_Nonnull symtab_ensure_symtab(symtab *syms, const char *name) {
  syms->teardown = symtab_teardown;
  symtab_entry *entry = symtab_find(syms, name);

  symtab *value = entry ? (symtab *)entry->val : NULL;
  if (entry == NULL) {
    value = symtab_new();
    symtab_add(syms, name, value);
  }
  return value;
}

cql_noexport bool_t symtab_add_symtab(symtab *syms, CSTR name, symtab *data) {
  syms->teardown = symtab_teardown;
  return symtab_add(syms, name, (void*)data);
}

static void bytebuf_teardown(void *val) {
  bytebuf_close((bytebuf*)val);
  free(val);
}

cql_noexport bytebuf *_Nonnull symtab_ensure_bytebuf(symtab *syms, const char *sym_new) {
  syms->teardown = bytebuf_teardown;
  symtab_entry *entry = symtab_find(syms, sym_new);

  bytebuf *buf = entry ? (bytebuf *)entry->val : NULL;
  if (entry == NULL) {
    buf = _new(bytebuf);
    bytebuf_open(buf);
    symtab_add(syms, sym_new, buf);
  }
  return buf;
}

cql_noexport void symtab_append_bytes(symtab *syms, const char *sym_new, const void *bytes, size_t count) {
  bytebuf *buf = symtab_ensure_bytebuf(syms, sym_new);
  bytebuf_append(buf, bytes, (uint32_t)count);
}

static void charbuf_teardown(void *val) {
  bclose((charbuf*)val);
  free(val);
}

cql_noexport charbuf *_Nonnull symtab_ensure_charbuf(symtab *syms, const char *sym_new) {
  syms->teardown = charbuf_teardown;
  symtab_entry *entry = symtab_find(syms, sym_new);
  charbuf *output = entry ? (charbuf *)entry->val : NULL;
  if (!output) {
    // None found, create one
    output = _new(charbuf);
    bopen(output);
    // This buffer doesn't participate in the normal stack of charbufs
    // it will be freed when the symbol table is torn down
    charbuf_open_count--;
    symtab_add(syms, sym_new, output);
  }
  return output;
}
