#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

OUT_DIR="out"
TEST_DIR="test"
CQL_FILE="${OUT_DIR}/generated_upgrade_test.cql"
SCHEMA_FILE="${OUT_DIR}/generated_upgrade_test_schema.sql"
TEST_PREFIX="test"
CQL="./${OUT_DIR}/cql"
ERROR_TRACE=0

DIR="$( dirname -- "$0"; )"

cd "${DIR}/.." || exit

# shellcheck disable=SC1091
source common/test_helpers.sh || exit 1

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

set_exclusive() {
  if [ $1 -eq 4 ]; then
    exclusive="--schema_exclusive"
  else
    exclusive=""
  fi
}

# Delete databases (if they exist).
rm -f ${OUT_DIR}/*.db

# Delete upgraders if they exist
rm -f ${OUT_DIR}/generated_upgrader*
rm -f ${OUT_DIR}/upgrade?

echo "compiling the shared schema validator to C"

cp upgrade/upgrade_validate.sql "${OUT_DIR}/upgrade_validate.sql"

 # set ERROR_TRACE to get verbose tracing in the upgrader
if [ "${ERROR_TRACE}" != "0" ]
then
   cat upgrade/errortrace.inc "${OUT_DIR}/upgrade_validate.sql" >"${OUT_DIR}/x"
   mv "${OUT_DIR}/x" "${OUT_DIR}/upgrade_validate.sql"
fi

if ! ${CQL} --in ${OUT_DIR}/upgrade_validate.sql --cg ${OUT_DIR}/upgrade_validate.h ${OUT_DIR}/upgrade_validate.c; then
  echo failed compiling upgrade validator
  echo ${CQL} --in upgrade/upgrade_validate.sql --cg ${OUT_DIR}/upgrade_validate.h ${OUT_DIR}/upgrade_validate.c
  exit 1;
fi

echo "creating the upgrade to v[n] schema upgraders"

for i in {0..4}
do
  set_exclusive $i

  if ! ${CQL} ${exclusive} --in "upgrade/SchemaPersistentV$i.sql" --rt schema_upgrade --cg "${OUT_DIR}/generated_upgrader$i.sql" --global_proc "$TEST_PREFIX"; then
    echo ${CQL} --in "upgrade/SchemaPersistentV$i.sql" --rt schema_upgrade --cg "${OUT_DIR}/generated_upgrader$i.sql" --global_proc "$TEST_PREFIX"
    echo "failed generating upgrade to version $i CQL"
    exit 1
  fi

  # set ERROR_TRACE to get verbose tracing in the upgrader
  if [ "${ERROR_TRACE}" != "0" ]
  then
    cat upgrade/errortrace.inc "${OUT_DIR}/generated_upgrader$i.sql" >"${OUT_DIR}/x"
    mv "${OUT_DIR}/x" "${OUT_DIR}/generated_upgrader$i.sql"
  fi

  if ! ${CQL} --in "${OUT_DIR}/generated_upgrader$i.sql" --compress --cg "${OUT_DIR}/generated_upgrade$i.h" "${OUT_DIR}/generated_upgrade$i.c"; then
    echo ${CQL} --in "${OUT_DIR}/generated_upgrader$i.sql" --compress --cg "${OUT_DIR}/generated_upgrade$i.h" "${OUT_DIR}/generated_upgrade$i.c"
    echo "failed C from the upgrader $i"
    exit 1
  fi
done

# compile the upgraders above to executables upgrade0-4

if ! make ${MAKE_COVERAGE_ARGS} upgrade_test; then
  echo make ${MAKE_COVERAGE_ARGS} upgrade_test
  echo failed compiling upgraders
fi

# now do the basic validation, can we create a schema of version n?
for i in {0..4}
do
  echo "testing upgrade to v$i from scratch"
  if ! "${OUT_DIR}/upgrade$i" "${OUT_DIR}/test_$i.db" > "${OUT_DIR}/upgrade_schema_v$i.out"; then
    echo "${OUT_DIR}/upgrade$i" "${OUT_DIR}/test_$i.db" ">" "${OUT_DIR}/upgrade_schema_v$i.out"
    echo "failed generating schema from scratch"
    echo "see log file above for details"
    exit 1
  fi

  on_diff_exit "upgrade_schema_v$i.out"
done

# now we'll try various previous schema combos with the current upgrader to make sure the work

for i in {1..4}
do
  (( j=i-1 ))
  echo "Verifying previous schema $j vs. final schema $i is ok"

  # Generate schema file with previous schema
  cat "upgrade/SchemaPersistentV$i.sql" > "$SCHEMA_FILE"
  echo "@previous_schema;" >> "$SCHEMA_FILE"
  cat "upgrade/SchemaPersistentV$j.sql" >> "$SCHEMA_FILE"

  set_exclusive $i

  # Generate upgrade CQL.
  # shellcheck disable=SC2086
  if ! ${CQL} ${exclusive} --in "${SCHEMA_FILE}" --cg "${CQL_FILE}" --rt schema_upgrade --global_proc "${TEST_PREFIX}"; then
    echo "Failed to generate upgrade CQL."
    echo "cc -I./ -Iupgrade -w -E -x c ${SCHEMA_FILE} > ${TEMP_FILE} && ${CQL} -- \
        --in ${TEMP_FILE} --cg ${CQL_FILE} --rt schema_upgrade \
        --global_proc ${TEST_PREFIX}"
    exit 1
  fi

  # set ERROR_TRACE to get verbose tracing in the upgrader
  if [ "${ERROR_TRACE}" != "0" ]
  then
    cat upgrade/errortrace.inc "${CQL_FILE}" >"${OUT_DIR}/x"
    mv "${OUT_DIR}/x" "${CQL_FILE}"
  fi

  if ! diff "${OUT_DIR}/generated_upgrader$i.sql" "${CQL_FILE}" ; then
    echo diff "${OUT_DIR}/generated_upgrader$i.sql" "${CQL_FILE}"
    echo "The upgrader from $j to $i was different than with no previous schema specified $i!"
    exit 1
  fi
done

for i in {0..4}
do
  for j in {0..4}
  do
    if [ $j -le $i ]; then

      echo "Upgrade from nothing to v$j, then to v$i -- must match direct update to v$i"

      rm -f "${OUT_DIR}/test.db"
      if ! ${OUT_DIR}/upgrade$j "${OUT_DIR}/test.db" > ${OUT_DIR}/partial.out; then
        echo ${OUT_DIR}/upgrade$j "${OUT_DIR}/test.db > ${OUT_DIR}/partial.out"
        echo "initial step to version $j" failed
      fi

      if ! diff "${OUT_DIR}/upgrade_schema_v$j.out" "${OUT_DIR}/partial.out";  then
        echo diff "${OUT_DIR}/upgrade_schema_v$j.out" "${OUT_DIR}/partial.out"
        echo going from nothing to $j was different when the upgrader was run again!
        exit 1
      fi

      if ! ${OUT_DIR}/upgrade$i "${OUT_DIR}/test.db" > ${OUT_DIR}/final.out; then
        echo ${OUT_DIR}/upgrade$i "${OUT_DIR}/test.db > ${OUT_DIR}/final.out"
        echo "initial step to version $i" failed
      fi

      if ! diff "${OUT_DIR}/upgrade_schema_v$i.out" "${OUT_DIR}/final.out";  then
        echo diff "${OUT_DIR}/upgrade_schema_v$i.out" "${OUT_DIR}/final.out"
        echo going from $j to $i was different than going directly to $i
        exit 1
      fi
    fi
  done
done

# ----- END UPGRADE TESTING -----

# ----- BEGIN DOWNGRADE TESTING -----

echo "Testing downgrade"

# Run the downgrade test binary on the test db which now has the v3 format
# the upgrader needed was already built and the downgrade test hardness was already linked
if ! (./${OUT_DIR}/downgrade_test "${OUT_DIR}/test.db"); then
  echo "Downgrade test failed."
  echo "./${OUT_DIR}/downgrade_test ${OUT_DIR}/test.db"
  exit 1
fi

# ----- END DOWNGRADE TESTING -----
