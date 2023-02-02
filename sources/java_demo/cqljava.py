#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# cqljava.py -> converts CQL JSON format into java classes for interop
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
#
# This approach is just one way to generate java, there are other ways you can
# create wrapper classes;  The nested class approach with one class per
# procedure works but it isn't the best for everyone.  The naming conventions
# used here are the simplest with the least transform from the original CQL
# but you could reasonably want to camelCase or PascalCase names as needed
# to create something cleaner looking.  All these things are possible with
# not much python at all.  It's also possible to create the JNI code to
# invoke the procedures but this sample has not done so at this time.

import json
import sys

def usage():
    print(
        (
            "Usage: input.json [options] >result.java\n"
            "\n"
            "--package package_name\n"
            "   specifies the output package name for the java\n"
            "--class outer_class_name\n"
            "   specifies the output class name for the wrapping java class\n"
        )
    )

notnull_types = {}
notnull_types["integer"] = "int"
notnull_types["long"] = "long"
notnull_types["real"] = "double"
notnull_types["object"] = "Object"
notnull_types["blob"] = "byte[]"
notnull_types["bool"] = "bool"
notnull_types["text"] = "String"

nullable_types = {}
nullable_types["integer"] = "Integer"
nullable_types["long"] = "Long"
nullable_types["real"] = "Double"
nullable_types["object"] = "Object"
nullable_types["blob"] = "byte[]"
nullable_types["bool"] = "Boolean"
nullable_types["text"] = "String"

getters = {}
getters["integer"] = "Integer"
getters["long"] = "Long"
getters["real"] = "Double"
getters["object"] = "ChildResultSet"
getters["blob"] = "Blob"
getters["bool"] = "Boolean"
getters["text"] = "String"

# The procedure might have any number of projected columns if it has a result
# We emit them all here
# projected_column
#  name : STRING
#  type : STRING
#  kind : STRING [optional]
#  isSensitive : BOOL [optional]
#  isNotNull" : BOOL
def emit_projection(p_name, projection, attributes):
    col = 0
    for p in projection:
        c_name = p["name"]
        type = p["type"]
        kind = p.get("kind", "")
        isSensitive = p.get("isSensitive", 0)
        isNotNull = p["isNotNull"]

        vaulted_columns = attributes.get("cql:vault_sensitive", None)
        vault_all = vaulted_columns == 1       

        getter = getters[type];

        if isNotNull:
            type = nullable_types[type]
            nullable = ""
        else:
            type = notnull_types[type]
            if getter == "String" or getter == "Blob" or getter == "ChildResultSet":
                nullable = ""
            else:
                nullable = "Nullable"

        if getter == "ChildResultSet":
            type = "CQLResultSet"

        isEncoded = False

        if isSensitive and vaulted_columns is not None and (vault_all or c_name in vaulted_columns):
            isEncoded = True
            # use custom encoded string type for encoded strings
            if type == "String" :
                type = "Encoded" + type
                getter = "Encoded" + getter

        print(f"    public {type} get_{c_name}(int row)", end = "")
        print(" {")
        print(f"      return mResultSet.get{nullable}{getter}(row, {col});")
        print("    }\n")

        if isEncoded:
            print(f"    public boolean get_{c_name}_IsEncoded()", end = "")
            print(" {")
            print(f"      return mResultSet.getIsEncoded({col});")
            print("    }\n")

        col = col + 1


# Here we emit all the information for the procedures that are known
# this is basic info about the name and arguments as well as dependencies.
# For any chunk of JSON that has the "dependencies" sub-block
# (see CQL JSON docs) we emit the table dependency info
# by following the "usesTables" data.  Note that per docs
# this entry is not optional!
def emit_procinfo(section, s_name):
    for src in section:
        p_name = src["name"]

        # for now only procs with a result type, like before
        # we'd like to emit JNI helpers for other procs too, but not now

        if "projection" in src:
            print(f"  static public final class {p_name}ViewModel extends CQLViewModel", end = "")
            print("   {\n")
            print(f"    public {p_name}ViewModel(CQLResultSet resultSet)", end = "")
            print("    {")
            print(f"       super(resultSet);")
            print("    }\n")

            alist = src.get("attributes", [])
            attributes = {}
            for attr in alist:
                k = attr["name"]
                v = attr["value"]
                attributes[k] = v

            emit_projection(p_name, src["projection"], attributes)

            identityResult = "true" if "cql:identity" in attributes else "false"

            print("    @Override")
            print("    protected boolean hasIdentityColumns() {");
            print(f"      return {identityResult};");
            print("    }\n");

            print("    public int getCount() {")
            print(f"      return mResultSet.getCount();")
            print("    }\n")
     

            print("  }")

# This walks the various JSON chunks and emits them into the equivalent table:
# * first we walk the tables, this populates:
#  * we use emit_procinfo for each chunk of procedures that has dependencies
#     * this is "queries", "inserts", "updates", "deletes", "general", and "generalInserts"
#     * see the CQL JSON docs for the meaning of each of these sections
#       * these all have the "dependencies" block in their JSON
def emit_java(data):
    emit_procinfo(data["queries"], "queries")
    emit_procinfo(data["deletes"], "deletes")
    emit_procinfo(data["inserts"], "inserts")
    emit_procinfo(data["generalInserts"], "generalInserts")
    emit_procinfo(data["updates"], "updates")
    emit_procinfo(data["general"], "general")

def main():
    jfile = sys.argv[1]
    with open(jfile) as json_file:
        data = json.load(json_file)

        package_name = "default_package"
        class_name  = "default_class"
        
        i = 2
        while i + 2 <= len(sys.argv):
            if sys.argv[i] == "--class":
                class_name = sys.argv[i+1]
            elif sys.argv[i] == "--package":
                package_name = sys.argv[i+1]
            else:
                usage()
            i = i + 2
 
        print("/*")
        print("* Copyright (c) Meta Platforms, Inc. and affiliates.")
        print("*")
        print("* This source code is licensed under the MIT license found in the");
        print("* LICENSE file in the root directory of this source tree.")
        print("*/\n")

        print(f"package {package_name};\n\n")

        print("import com.facebook.cgsql.CQLResultSet;\n")
        print("import com.facebook.cgsql.CQLViewModel;\n")
        print("import com.facebook.cgsql.EncodedString;\n")

        print(f"public class {class_name}")
        print("{")

        emit_java(data)

        print("}")

if __name__ == "__main__":
    if len(sys.argv) == 1:
      usage()
    else:
      main()
