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
            "--table_diagram input.json > tables.dot\n"
            "   creates a .dot file for a table diagram\n"
            "\n"
            "--region_diagram input.json > regions.dot\n"
            "   creates a .dot file for a region diagram\n"
            "\n"
            "--erd input.json tables... > erd.dot\n"
            "   creates a .dot file for an ER diagram\n"
            "\n"
            "--sql input.json > inputdb.sql\n"
            "   creates a .sql file for a database with the schema info\n"
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
def emit_erd():
    if len(sys.argv) < 4:
        return

    jfile = sys.argv[2]
    with open(jfile) as json_file:
        data = json.load(json_file)
        print("digraph parse {")
        print("rankdir=LR;")

        tables = {}
        for tup in enumerate(data["tables"]):
            t = tup[1]
            t_name = t["name"]
            tables[t_name] = t

        for i in range(3, len(sys.argv)):
            t_name = sys.argv[i]
            print(f"{t_name} [")
            print("  shape=plaintext")
            print("  label=<")
            print("    <table border='1' cellborder='0'>")
            print(f"     <tr><td><b>{t_name}</b></td></tr>")

            t = tables[t_name]
            pk = {}
            for pkcol in t["primaryKey"]:
                pk[pkcol] = 1

            fks = {}
            for fktup in enumerate(t["foreignKeys"]):
                fk = fktup[1]
                for fkcol in fk["columns"]:
                    fks[fkcol] = 1

            for dopk in range(0, 2):
                for ctup in enumerate(t["columns"]):
                    c = ctup[1]
                    c_name = c["name"]
                    c_type = c["type"]
                    c_notnull = c["isNotNull"] == 1

                    pktext = "PK" if dopk == 0 else ""
                    fktext = "FK" if c_name in fks else ""
                    nntext1 = "<b>" if c_notnull else ""
                    nntext2 = "</b>" if c_notnull else ""

                    if dopk != (c_name in pk):
                        print("<tr>")
                        print(f"<td align='left'>{c_name}</td>")
                        print(f"<td align='left'>{nntext1}{c_type}{nntext2}</td>")
                        print(f"<td>{pktext}</td>")
                        print(f"<td>{fktext}</td>")
                        print("</tr>")

                if dopk == 0:
                    print("<tr><td align='left'>---------</td></tr>")

            print("    </table>")
            print(">];")

            for fktup in enumerate(t["foreignKeys"]):
                fk = fktup[1]
                reftable = fk["referenceTable"]
                print(f"{t_name} -> {reftable}")

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
def emit_table_diagram(jfile):
    with open(jfile) as json_file:
        data = json.load(json_file)

        connected = {}

        for tup in enumerate(data["tables"]):
            t = tup[1]
            t_name = t["name"]

            for fktup in enumerate(t["foreignKeys"]):
                fk = fktup[1]
                reftable = fk["referenceTable"]
                connected[t_name] = 1
                connected[reftable] = 1

        print("digraph parse {")
        print("rankdir=LR;")
        print("{rank=same orphans root}")

        for tup in enumerate(data["tables"]):
            t = tup[1]
            t_name = t["name"]
            print(f'{t_name} [label = "{t_name}" shape=plaintext]')

            if len(t["foreignKeys"]) == 0:
                if t_name in connected:
                    print(f"{t_name} -> root")
                else:
                    print(f"{t_name} -> orphans")

            for fktup in enumerate(t["foreignKeys"]):
                fk = fktup[1]
                reftable = fk["referenceTable"]
                print(f"{t_name} -> {reftable}")

        print("}")


# Here we emit a digraph that has all the tables
# This works exactly the same as "tables" except we follow
# the dependent region link instead of the foreign key links
# everything else is the same even "orphans" etc.
def emit_region_diagram(jfile):
    with open(jfile) as json_file:
        data = json.load(json_file)

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
        print("{rank=same orphans root}")

        for tup in enumerate(data["regions"]):
            r = tup[1]
            r_name = r["name"]
            print(f'{r_name} [label = "{r_name}" shape=plaintext]')

            if len(r["using"]) == 0:
                if r_name in connected:
                    print(f"{r_name} -> root")
                else:
                    print(f"{r_name} -> orphans")

            for rdep in enumerate(r["using"]):
                rparent = rdep[1]
                print(f"{r_name} -> {rparent}")

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
            "  region text not null);\n"
            "\n"
            "create table pks(\n"
            "  t_name text not null,\n"
            "  c_name text not null);\n"
            "\n"
            "create table columns(\n"
            "  t_name text not null,\n"
            "  c_name text not null,\n"
            "  c_type text not null,\n"
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
def emit_sql(jfile):
    with open(jfile) as json_file:
        data = json.load(json_file)
        for tup in enumerate(data["tables"]):
            t = tup[1]
            t_name = t["name"]
            region = t["region"] if "region" in t else "None"
            print(f"insert into tables values('{t_name}', '{region}');")
            for ctup in enumerate(t["columns"]):
                c = ctup[1]
                c_name = c["name"]
                c_type = c["type"]
                c_notnull = c["isNotNull"]
                print(
                    f"insert into columns values ('{t_name}', '{c_name}', '{c_type}', {c_notnull});"
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


# here we are just going to decode the arguments

if len(sys.argv) < 3:
    usage()
    exit(0)

if sys.argv[1] == "--table_diagram":
    emit_table_diagram(sys.argv[2])
    exit(0)

if sys.argv[1] == "--region_diagram":
    emit_region_diagram(sys.argv[2])
    exit(0)

if sys.argv[1] == "--erd":
    emit_erd()
    exit(0)

if sys.argv[1] == "--sql":
    emit_schema()
    emit_sql(sys.argv[2])
    exit(0)

if __name__ == "__main__":
    usage()
