---
id: int06
title: "Part 6: Schema Management"
sidebar_label: "Part 6: Schema Management"
---
<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
### Preface

Part 6 continues with a discussion of the essentials schema management in the CQL compiler.
As in the previous sections, the goal here is not to go over every detail but rather to give
a sense of how schema management happens in general -- the core strategies and implementation choices --
so that when reading the management code you will have an idea how it all hangs together. To accomplish
this, various key data structures will be explained in detail as well as selected examples of their use.

## Schema Management

The primary goal of the schema management features of the CQL compiler is to provide the ability
to create a "schema upgrader" that can move a given users database from a previous version
of the schema to the current version.  Because of the limitations of SQL in general and
SQLite in particular not all transforms are possible; so additionally the system must correctly
detect and prevent upgrades that cannot be safely performed.

### Attributes

The full set of schema attributes and their meaning is described in [Chapter 10](https://cgsql.dev/cql-guide/ch10)
and the full set of validations is described in [Chapter 11](https://cgsql.dev/cql-guide/ch11).  Briefly the
directives are:

* `@create(n)`: indicates a table/column is to be created at version `n`.
* `@delete(n)`: indicates a table/column is to be deleted at version `n`.
* `@recreate`: indicates the table contents are not precious
   * it and can be dropped and created when its schema changes
   * this does not combine with `@create`
   * it applies only to tables
   * views, triggers, and indices are always on the `@recreate` plan and do not have to be marked so

Now the various annotations can occur substantially in any order as there are no rules that require that
tables that are created later appear later in the input.  This would be most inconvenient for any upgrading
logic.  However, the semantic validation pass gathers all such annotations into a large array.



