#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

OUT_DIR=out
CQL_FILE="${OUT_DIR}/generated_upgrade_test.cql"
GENERATED_DOWNGRADE_SOURCE="${OUT_DIR}/generated_downgrade_test"
GENERATED_UPGRADE_SOURCE="${OUT_DIR}/generated_upgrade_test"
SCHEMA_FILE="${OUT_DIR}/generated_upgrade_test_schema.sql"
TEMP_FILE="${OUT_DIR}/upgrade_tmp_file"
TEST_PREFIX="test"
CQL="./${OUT_DIR}/cql"

# Delete databases (if they exist).
rm $TEST_PREFIX*.db &> /dev/null

while [ "$1" != "" ]
do
  if [ "$1" == "--coverage" ]
  then
     MAKE_COVERAGE_ARGS="COVERAGE=1"
     shift 1
  else
     echo "Usage: upgrade_test.sh  [--coverage]"
     exit 1
  fi
done

# ----- BEGIN UPGRADE TESTING -----

for i in {0..3}
do
  for j in {0..3}
  do
    if [ $j -le $i ]; then

      # Generate schema file.
      echo "#include \"upgrade/SchemaPersistentV$i.sql\"" > "$SCHEMA_FILE"

      # Perform previous schema validation, if applicable.
      (( k=i-1 ))
      (( b=i*3+j ))
      if [ $k -ge 0 ]; then
        echo "@previous_schema;" >> "$SCHEMA_FILE"
        echo "#include \"upgrade/SchemaPersistentV$k.sql\"" >> "$SCHEMA_FILE"
        echo "Testing upgrade of DB $j from V$k to V$i"
      else
        echo "Testing baseline installation of V0 on DB $j"
      fi

      # Generate upgrade CQL.
      if ! (cc -I./ -Iupgrade -w -E -x c "$SCHEMA_FILE" > "$TEMP_FILE" && ${CQL} \
          --in "$TEMP_FILE" --cg "$CQL_FILE" --rt schema_upgrade \
          --global_proc "$TEST_PREFIX"); then
        echo "Failed to generate upgrade CQL."
        echo "cc -I./ -Iupgrade -w -E -x c $SCHEMA_FILE > $TEMP_FILE && ${CQL} -- \
            --in $TEMP_FILE --cg $CQL_FILE --rt schema_upgrade \
            --global_proc $TEST_PREFIX"
        exit 1
      fi

      # Compile upgrade CQL to C.
      if ! (${CQL} --compress --rt c --in "$CQL_FILE" \
          --cg "${GENERATED_UPGRADE_SOURCE}.h" "${GENERATED_UPGRADE_SOURCE}$b.c"); then
        echo "Failed to compile upgrade CQL to C."
        echo "${CQL} -- --compress --rt c --in $CQL_FILE \
          --cg ${GENERATED_UPGRADE_SOURCE}.h ${GENERATED_UPGRADE_SOURCE}$b.c"
        exit 1
      fi

      if ! (make --always-make ${MAKE_COVERAGE_ARGS} RUN=$b upgrade_test); then
        echo "Upgrade test build failed."
        echo "make RUN=$b --always-make ${MAKE_COVERAGE_ARGS} upgrade_test"
        exit 1
      fi

      # Run the upgrade test binary.
      if ! (./${OUT_DIR}/upgrade_test "${OUT_DIR}/$TEST_PREFIX$j.db"); then
        echo "Upgrade test failed."
        echo "./${OUT_DIR}/upgrade_test ${OUT_DIR}/$TEST_PREFIX$j.db"
        exit 1
      fi

      # Delete the temporary file.
      if ! rm "$TEMP_FILE"; then
        echo "Unable to delete the temporary file."
        echo "rm $TEMP_FILE"
        exit 1
      fi
    fi
  done
done

# ----- END UPGRADE TESTING -----

# ----- BEGIN DOWNGRADE TESTING -----

(( i=1 ))
echo "Testing downgrade of DB $i"

# Generate the V1 schema file.
echo "#include \"upgrade/SchemaPersistentV$i.sql\"" > "$SCHEMA_FILE"

# Generate upgrade CQL.
if ! (cc -I./ -Iupgrade -w -E -x c "$SCHEMA_FILE" > "$TEMP_FILE" && ${CQL} \
    --in "$TEMP_FILE" --cg "$CQL_FILE" --rt schema_upgrade \
    --global_proc "$TEST_PREFIX"); then
  echo "Failed to generate upgrade CQL for downgrade test."
  echo "cc -I./ -Iupgrade -w -E -x c $SCHEMA_FILE > $TEMP_FILE && ${CQL} -- \
      --in $TEMP_FILE --cg $CQL_FILE --rt schema_upgrade \
      --global_proc $TEST_PREFIX"
  exit 1
fi

# Compile upgrade CQL to C.
if ! (${CQL} --compress --rt c --in "$CQL_FILE" \
    --cg "${GENERATED_DOWNGRADE_SOURCE}.h" "${GENERATED_DOWNGRADE_SOURCE}.c"); then
  echo "Failed to compile upgrade CQL to C for downgrade test."
  echo "${CQL} -- --compress --rt c --in $CQL_FILE \
    --cg ${GENERATED_DOWNGRADE_SOURCE}.h ${GENERATED_DOWNGRADE_SOURCE}.c"
  exit 1
fi

if ! (make --always-make ${MAKE_COVERAGE_ARGS} downgrade_test); then
  echo "Downgrade test build failed."
  echo "make --always-make ${MAKE_COVERAGE_ARGS} downgrade_test"
  exit 1
fi

# Run the downgrade test binary on DB 1, which now has the V3 schema.
if ! (./${OUT_DIR}/downgrade_test "${OUT_DIR}/$TEST_PREFIX$i.db"); then
  echo "Downgrade test failed."
  echo "./${OUT_DIR}/downgrade_test ${OUT_DIR}/$TEST_PREFIX$i.db"
  exit 1
fi

# Delete the temporary file.
if ! rm "$TEMP_FILE"; then
  echo "Unable to delete the temporary file."
  echo "rm $TEMP_FILE"
  exit 1
fi

# ----- END DOWNGRADE TESTING -----
