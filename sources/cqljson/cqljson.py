#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


# cqljson.py -> converts CQL JSON format into various useful outputs
#
# The CQL JSON format is documented here:
#   https://cgsql.dev/cql-guide/ch13
# and here:
#   https://cgsql.dev/json-diagram
#
# NB: This code should be considered SAMPLE code, not production code.
# Which is to say you can reasonably expect that the specifics of the diagrams
# and the database produced here are likely to change at whim.  If you need
# a particular output, you are enouraged to FORK this sample into something
# stable.  The JSON format itself is the contract and it evolves in a backwards
# compatible way.  This script is likely to change to make different pretty
# pictures at various times.

import json
import sys


def usage():
    print(
        (
            "Usage:\n"
            "\n"
            "--table_diagram input.json [universe]> tables.dot\n"
            "   creates a .dot file for a table diagram\n"
            "\n"
            "--region_diagram input.json > regions.dot\n"
            "   creates a .dot file for a region diagram\n"
            "\n"
            "--erd input.json [universe] > erd.dot\n"
            "   creates a .dot file for an ER diagram\n"
            "\n"
            "--sql input.json > inputdb.sql\n"
            "   creates a .sql file for a database with the schema info\n"
            "\n"
            "The [universe] arguments can be:\n"
            "  * nothing in which case all tables are the used\n"
            "  * a list of targets which are processed in order\n"
            "  * a target is:\n"
            "    * a table name\n"
            "    * a table name followed by +fks (e.g. foo+fks)\n"
            "    * a table name followed by +refs (e.g. foo+refs)\n"
            "    * a table name followed by +graph (e.g. foo+graph)\n"
            "  * +fks: means the table and all its FK tables, transitively\n"
            "  * +refs: means the table and all that refer to it via FK, transitively\n"
            "  * +graphs: means the table and any table linked to it either way, transitively\n"
            "  * a target may be prefixed with '-' indicating that pattern should be removed\n"
            "\n"
            "To create a CQL JSON file you can start with any CQL, but\n"
            "probably a file with most or all of your schema is the best\n"
            "choice."
            "  cql --in your_file.sql --rt json_schema --cg output.json\n"
            "\n"
            "To process a .dot file use a command like:\n"
            "  dot x.dot -Tpng -o x.png\n"
        )
    )


def add_colinfo(colinfo, col, data):
    if col not in colinfo:
        colinfo[col] = data
    else:
        colinfo[col] = colinfo[col] + "," + data


def compute_pk(t):
    pk = {}
    for pkcol in t["primaryKey"]:
        pk[pkcol] = 1
    return pk


def compute_colinfo(t):
    colinfo = {}

    pk = {}
    for pkcol in t["primaryKey"]:
        pk[pkcol] = 1
        add_colinfo(colinfo, pkcol, "PK")

    ifk = 0
    for fktup in enumerate(t["foreignKeys"]):
        ifk = ifk + 1
        fkname = f"FK{ifk}"
        fk = fktup[1]
        for fkcol in fk["columns"]:
            add_colinfo(colinfo, fkcol, fkname)

    iuk = 0
    for uktup in enumerate(t["uniqueKeys"]):
        iuk = iuk + 1
        ukname = f"UK{iuk}"
        uk = uktup[1]
        for ukcol in uk["columns"]:
            add_colinfo(colinfo, ukcol, ukname)

    return colinfo


# First we look up all the tables and make a dictionary so we can find them by name,
# then we walk the list of table arguments, and for each table:
#   * make a dictionary that contains the PK columns
#   * make a dictionary that contains the FK columns
#   * emit an HTML "label"
#     * the first row is the table name in bold
#     * we walk the columns twice
#       * the first time we emit just the pk columns
#       * then we emit a spacer row "---"
#       * the second time we emit all the non-PK columns
#     * for each emitted column we emit the name, type, and PK/FK status
#       * if the column is not null the type is bold
#       * the PK and FK dictionaries are used to emit the PK/FK info
#   * we walk the foreign keys and emit a link foo -> bar for each referenced table
def emit_erd(data, universe, tables):
    print("digraph parse {")
    print("rankdir=LR;")

    for t_name in universe:

        t = tables[t_name]
        pk = compute_pk(t)
        colinfo = compute_colinfo(t)

        fkports = {}

        for fktup in enumerate(t["foreignKeys"]):
            fk = fktup[1]
            reftable = fk["referenceTable"]
            portcol = fk["columns"][0]
            print(f"{t_name}:{portcol} -> {reftable}")
            fkports[portcol] = 1

        print(f"{t_name} [")
        print("  shape=plaintext")
        print("  label=<")
        print("    <table border='1' cellborder='0'>")
        print(f"     <tr><td><b>{t_name}</b></td></tr>")

        for not_pk_pass in range(0, 2):
            for ctup in enumerate(t["columns"]):
                c = ctup[1]
                c_name = c["name"]
                c_type = c["type"]
                c_kind = "&lt;" + c["kind"] + "&gt;" if "kind" in c else ""
                c_notnull = c["isNotNull"] == 1

                nntext1 = "<b>" if c_notnull else ""
                nntext2 = "</b>" if c_notnull else "?"

                fkport = f" port='{c_name}'" if c_name in fkports else ""

                if not_pk_pass != (c_name in pk):
                    print("<tr>")
                    print(f"<td align='left'>{c_name}</td>")
                    print(f"<td align='left'>{nntext1}{c_type}{c_kind}{nntext2}</td>")
                    if c_name in colinfo:
                        print(f"<td align='left'{fkport}>{colinfo[c_name]}</td>")
                    else:
                        print("<td></td>")
                    print("</tr>")

            if not_pk_pass == 0:
                print("<tr><td align='left'>---------</td></tr>")

        print("    </table>")
        print(">];")

    print("}")


# Here we emit a digraph that has all the tables
# any table that is connected to any other table is included in the main part
#  * this part is connected to "root"
# any tables that are connected to nothing are linked to "orphans"
#  * this keeps them from clogging the main diagram
# we compute the "connected" set by walking all foreign keys for all tables
#  * any table that is the source or target of an FK is marked "connected"
#
# The main diagram begins by walking all tables
#  * for each table we emit a text-only shape
#  * for each table we emit a link that follows its foreign keys
#  * if there are no foreign keys we emit a link to "root" or "orphans"
#    * any not connected table gets a link to "orphans"
#
def emit_table_diagram(data, universe, tables):
    connected = {}

    for t_name in universe:
        t = tables[t_name]
        for fktup in enumerate(t["foreignKeys"]):
            fk = fktup[1]
            reftable = fk["referenceTable"]
            connected[t_name] = 1
            connected[reftable] = 1

    print("digraph parse {")
    print("rankdir=LR;")

    need_orphans = False
    need_root = False

    for t_name in universe:
        t = tables[t_name]
        print(f'{t_name} [label = "{t_name}" shape=plaintext]')

        if len(t["foreignKeys"]) == 0:
            if t_name in connected:
                need_root = True
                print(f"{t_name} -> root")
            else:
                need_orphans = True
                print(f"{t_name} -> orphans")

        for fktup in enumerate(t["foreignKeys"]):
            fk = fktup[1]
            reftable = fk["referenceTable"]
            print(f"{t_name} -> {reftable}")

    if need_orphans and need_root:
        print("{rank=same orphans root}")

    print("}")


# Here we emit a digraph that has all the tables
# This works exactly the same as "tables" except we follow
# the dependent region link instead of the foreign key links
# everything else is the same even "orphans" etc.
def emit_region_diagram(data):
    connected = {}

    for tup in enumerate(data["regions"]):
        r = tup[1]
        r_name = r["name"]

        for rdep in enumerate(r["using"]):
            rparent = rdep[1]
            connected[r_name] = 1
            connected[rparent] = 1

    print("digraph parse {")
    print("rankdir=LR;")

    need_orphans = False
    need_root = False

    for tup in enumerate(data["regions"]):
        r = tup[1]
        r_name = r["name"]
        print(f'{r_name} [label = "{r_name}" shape=plaintext]')

        if len(r["using"]) == 0:
            if r_name in connected:
                need_root = True
                print(f"{r_name} -> root")
            else:
                need_orphans = True
                print(f"{r_name} -> orphans")

        for rdep in enumerate(r["using"]):
            rparent = rdep[1]
            print(f"{r_name} -> {rparent}")

    if need_orphans and need_root:
        print("{rank=same orphans root}")

    print("}")


# This generates the schema we need for our sql output
# Note that this is pretty normalized, so if you want counts
# and so forth you have to do them the usual way for
# normalized tables.  It's done this way for simplicity
# and because data volumes are expected to be low and
# also if you really needed any denorms, you can make them
# yourself very easily starting from this.  All of this
# is pretty much the relational version of what's in the JSON
# for tables, columns, PKs, FKs, and regions. But not all
# the fields are present (e.g. column sensitivity is absent).
# This could easily be made more complete but the essential
# metadata is here; at least enough to ask important questions
# about usage and to study the metadata to find possible
# consolidations and stuff like that.
def emit_schema():
    print(
        (
            "create table tables(\n"
            "  t_name text primary key,\n"
            "  region text not null,\n"
            "  deleted bool not null,\n"
            "  create_version int not null,\n"
            "  delete_version int not null,\n"
            "  recreate bool not null,\n"
            "  recreate_group text not null);\n"
            "\n"
            "create table pks(\n"
            "  t_name text not null,\n"
            "  c_name text not null);\n"
            "\n"
            "create table columns(\n"
            "  t_name text not null,\n"
            "  c_name text not null,\n"
            "  c_type text not null,\n"
            "  c_kind text not null,\n"
            "  c_notnull bool not null,\n"
            "  primary key (t_name, c_name));\n"
            "\n"
            "create table regions(\n"
            "  r_name text primary key);\n"
            "\n"
            "create table region_deps(\n"
            "  rchild text not null,\n"
            "  rparent text not null);\n"
            "\n"
            "create table fks(\n"
            "  fk_name text not null,\n"
            "  src_table text not null,\n"
            "  ref_table text not null,\n"
            "  src_col text not null,\n"
            "  ref_col text not null);\n"
            "\n"
            "create table proc_deps(\n"
            "  p_name text not null,\n"
            "  t_name text not null);\n"
            "\n"
            "create table views(\n"
            "  v_name text primary key,\n"
            "  region text not null,\n"
            "  deleted bool not null,\n"
            "  create_version int not null,\n"
            "  delete_version int not null);\n"
            "\n"
            "create table proc_view_deps(\n"
            "  p_name text not null,\n"
            "  v_name text not null);\n"
            "\n"
        )
    )


# For any chunk of JSON that has the "dependencies" sub-block
# (see CQL JSON docs) we emit the table dependency info
# by following the "usesTables" data.  Note that per docs
# this entry is not optional!
def emit_tabledep(section):
    for src in section:
        pname = src["name"]
        usesTables = src["usesTables"]
        for tdep in usesTables:
            print(f"insert into proc_deps values('{pname}', '{tdep}');")
        for vdep in src.get("usesViews", []):
            print(f"insert into proc_view_deps values('{pname}', '{vdep}');")


# This walks the various JSON chunks and emits them into the equivalent table:
# * first we walk the tables, this populates:
#   * one row in the tables table (t_name, region)
#   * one row per column in the columns table (t_name, c_name, c_type, c_notnull)
#   * one row per primary key column in the pks table (t_name, pkcol)
#   * we enumerate the FKs, giving each a name like fk1, fk2, etc. (fk{ifk})
#     * emit one row per FK per column (fk{ifk}, t_name, reftable, fkcol, fkref)
#  * next walk the regions
#     * emit one row per region in the region table
#     * emit one row per dependency to region_deps table (r_name, rparent)
#  * we use emit_tabledep for each chunk of procedures that has dependencies
#     * this is "queries", "inserts", "updates", "deletes", "general", and "generalInserts"
#     * see the CQL JSON docs for the meaning of each of these sections
#       * these all have the "dependencies" block in their JSON
def emit_sql(data):
    for tup in enumerate(data["tables"]):
        t = tup[1]
        t_name = t["name"]
        region = t.get("region", "None")
        deleted = 1 if t["isDeleted"] else 0
        recreated = 1 if t["isRecreated"] else 0
        createVersion = t.get("addedVersion", 0)
        deleteVersion = t.get("deletedVersion", -1)
        groupName = t.get("recreateGroupName", "")
        print(
            f"insert into tables values('{t_name}', '{region}', {deleted}, {createVersion}, {deleteVersion}, {recreated}, '{groupName}');"
        )
        for ctup in enumerate(t["columns"]):
            c = ctup[1]
            c_name = c["name"]
            c_type = c["type"]
            c_kind = c.get("kind", "")
            c_notnull = c["isNotNull"]
            print(
                f"insert into columns values ('{t_name}', '{c_name}', '{c_type}', '{c_kind}', {c_notnull});"
            )

        for pkcol in t["primaryKey"]:
            print(f"insert into pks values('{t_name}', '{pkcol}');")

        ifk = 0
        for fktup in enumerate(t["foreignKeys"]):
            ifk = ifk + 1
            fk = fktup[1]
            fkcols = fk["columns"]
            fkrefs = fk["referenceColumns"]
            reftable = fk["referenceTable"]
            ccols = len(fkcols)
            for icol in range(0, ccols):
                fkcol = fkcols[icol]
                fkref = fkrefs[icol]
                print(
                    f"insert into fks values('fk{ifk}', '{t_name}', '{reftable}', '{fkcol}', '{fkref}');"
                )

    for tup in enumerate(data["regions"]):
        r = tup[1]
        r_name = r["name"]
        print(f"insert into regions values('{r_name}');")
        for rdep in enumerate(r["using"]):
            rparent = rdep[1]
            print(f"insert into region_deps values('{r_name}', '{rparent}');")

    emit_tabledep(data["queries"])
    emit_tabledep(data["deletes"])
    emit_tabledep(data["inserts"])
    emit_tabledep(data["generalInserts"])
    emit_tabledep(data["updates"])
    emit_tabledep(data["general"])

    for tup in enumerate(data["views"]):
        v = tup[1]
        v_name = v["name"]
        region = v.get("region", "None")
        deleted = 1 if v["isDeleted"] else 0
        createVersion = v.get("addedVersion", 0)
        deleteVersion = v.get("deletedVersion", -1)
        print(
            f"insert into views values('{v_name}', '{region}', {deleted}, {createVersion}, {deleteVersion});"
        )


def get_fks(targets, tables, data, arg):
    # skips deleted tables
    if arg not in tables:
        return

    # skips tables already visited
    if arg in targets:
        return

    t = tables[arg]
    targets[arg] = 1

    for fktup in enumerate(t["foreignKeys"]):
        fk = fktup[1]
        reftable = fk["referenceTable"]
        get_fks(targets, tables, data, reftable)


def get_refs(targets, tables, data, refmap, arg):
    # skips deleted tables
    if arg not in tables:
        return

    # skips tables already visited
    if arg in targets:
        return

    targets[arg] = 1

    if arg in refmap:
        for srctable in refmap[arg]:
            get_refs(targets, tables, data, refmap, srctable)


def get_graph(targets, tables, data, refmap, arg):
    # skips deleted tables
    if arg not in tables:
        return

    # skips tables already visited
    if arg in targets:
        return

    t = tables[arg]
    targets[arg] = 1

    if arg in refmap:
        for srctable in refmap[arg]:
            get_graph(targets, tables, data, refmap, srctable)

    for fktup in enumerate(t["foreignKeys"]):
        fk = fktup[1]
        reftable = fk["referenceTable"]
        get_graph(targets, tables, data, refmap, reftable)


def compute_refmap(data):
    refmap = {}
    for tup in enumerate(data["tables"]):
        t = tup[1]
        t_name = t["name"]
        for fktup in enumerate(t["foreignKeys"]):
            fk = fktup[1]
            reftable = fk["referenceTable"]

            # now make the reverse index
            if reftable not in refmap:
                refmap[reftable] = {}
            refmap[reftable][t_name] = 1

    return refmap


def get_targets(tables, data, refmap, arg):
    targets = {}
    if arg.endswith("+fks"):
        arg = arg[0:-4]
        if arg not in tables:
            print(f"'{arg}' not a valid table")
            exit(1)
        get_fks(targets, tables, data, arg)
    elif arg.endswith("+refs"):
        arg = arg[0:-5]
        if arg not in tables:
            print(f"'{arg}' not a valid table")
            exit(1)
        get_refs(targets, tables, data, refmap, arg)
    elif arg.endswith("+graph"):
        arg = arg[0:-6]
        if arg not in tables:
            print(f"'{arg}' not a valid table")
            exit(1)
        get_graph(targets, tables, data, refmap, arg)
    elif arg in tables:
        targets[arg] = 1
    else:
        print(f"'{arg}' not a valid table")
        exit(1)
    return targets


def get_universe(tables, data):
    universe = {}
    # if no args, the default universe is "everything"
    if len(sys.argv) == 3:
        for t in tables:
            universe[t] = 1
    else:
        refmap = compute_refmap(data)

        for i in range(3, len(sys.argv)):
            arg = sys.argv[i]
            subtract = False
            if arg[0] == "-":
                subtract = True
                arg = arg[1:]

            targets = get_targets(tables, data, refmap, arg)

            if not subtract:
                for t_name in targets:
                    universe[t_name] = 1
            else:
                for t_name in targets:
                    if t_name in universe:
                        del universe[t_name]

    return universe


def dispatch_option():
    jfile = sys.argv[2]
    with open(jfile) as json_file:
        data = json.load(json_file)
        tables = {}
        for tup in enumerate(data["tables"]):
            t = tup[1]
            if not t["isDeleted"]:
                t_name = t["name"]
                tables[t_name] = t

        universe = get_universe(tables, data)
        if sys.argv[1] == "--table_diagram":
            emit_table_diagram(data, universe, tables)
        elif sys.argv[1] == "--region_diagram":
            emit_region_diagram(data)
        elif sys.argv[1] == "--erd":
            emit_erd(data, universe, tables)
        elif sys.argv[1] == "--sql":
            emit_schema()
            emit_sql(data)
        else:
            usage()


def main():
    # here we are just going to decode the arguments
    if len(sys.argv) < 3:
        usage()
    else:
        dispatch_option()


if __name__ == "__main__":
    main()
