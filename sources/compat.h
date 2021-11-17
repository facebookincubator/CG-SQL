/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

cql_noexport char *_Nonnull Strdup(const char *_Nonnull s);
cql_noexport int32_t Strcasecmp(const char *_Nonnull s1, const char *_Nonnull s2);
cql_noexport int32_t Strncasecmp(const char *_Nonnull s1, const char *_Nonnull s2, size_t n);
cql_noexport int32_t Strendswith(const char *_Nonnull haystack, const char *_Nonnull needle);
