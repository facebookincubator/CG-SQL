#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# exit when any command fails
set -e

if [ "${JAVA_HOME}" == "" ] ;
then
  echo "JAVA_HOME must be set to your JDK dir"
  echo  "e.g. JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk-10.0.1.jdk/Contents/Home"
  echo  "e.g. JAVA_HOME=/lib/jvm/java-16-openjdk-amd64/"
  exit 1
fi

echo "java located at:" ${JAVA_HOME}

CC="cc -g"

if [ "${CGSQL_GCC}" != "" ] ;
then
  # gcc flags removing clang extensions
  CC="${CC} -std=c99 -D_Nullable= -D_Nonnull="
fi

if [ "$(uname)" == "Linux" ];
then
  # linux path variation and -fPIC for .so output
  CC="${CC} -I.. -I${JAVA_HOME}/include -I${JAVA_HOME}/include/linux -fPIC"
  SUFFIX=so
else
  # assuming clang elsewhere (e.g. Mac)
  CC="${CC} -I.. -I${JAVA_HOME}/include -I${JAVA_HOME}/include/darwin"
  SUFFIX=jnilib
fi

if [ "${SQLITE_PATH}" != "" ] ;
then
  echo building sqlite amalgam
  CC="${CC} -I${SQLITE_PATH}"
  SQLITE_LINK=sqlite3-all.o
  ${CC} -c -o sqlite3-all.o ${SQLITE_PATH}/sqlite3-all.c
else
  SQLITE_LINK=-lsqlite3
fi

echo "building cql"
(cd .. ; make)

echo "making directories"

mkdir -p com/facebook/cgsql
mkdir -p sample

echo generating stored procs
../out/cql --in Sample.sql --cg Sample.h Sample.c
../out/cql --in Sample.sql --rt java --cg sample/Sample.java --java_package_name sample

# normalize the output
# 1. the internal version has different java path names, normalize on the OSS path names
# 2. Nullable annotations appear in the output, to keep dependencies simple
#    they are stripped here.  In your environments it's totally reasonable to keep these
#    as the dependencies are not likely to be onerous.  But for here we just strip them.
#
sed -e "s/msys.mci/cgsql/" -e "s/@Nullable//" -e "/import javax.annotation.Nullable/d" <sample/Sample.java >__tmp1
mv __tmp1 sample/Sample.java

echo "regenerating JNI .h file"
javac -h . com/facebook/cgsql/CQLResultSet.java
javac -h . TestResult.java

echo "adding license headers to generated files"

cat <<EOF >__tmp1
/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

EOF

cat __tmp1 com_facebook_cgsql_CQLResultSet.h >__tmp2
mv __tmp2 com_facebook_cgsql_CQLResultSet.h

cat __tmp1 TestResult.h >__tmp2
mv __tmp2 TestResult.h

rm __tmp1

echo "compiling native code"
${CC} -c com_facebook_cgsql_CQLResultSet.c
${CC} -c TestResult.c
${CC} -c Sample.c

${CC} -o libTestResult.${SUFFIX} -shared TestResult.o Sample.o ../cqlrt.c ${SQLITE_LINK}
${CC} -o libCQLResultSet.${SUFFIX} -shared com_facebook_cgsql_CQLResultSet.o ../cqlrt.c ${SQLITE_LINK}

echo making .class files

javac CGSQLMain.java TestResult.java com/facebook/cgsql/CQLResultSet.java com/facebook/cgsql/CQLViewModel.java com/facebook/cgsql/EncodedString.java sample/Sample.java

echo "executing"
LIBPATH=.
java -Djava.library.path=${LIBPATH} CGSQLMain TestResult com/facebook/cgsql/CQLResultSet CQLViewModel sample/Sample

echo "run clean.sh to remove build artifacts"
echo "done"
