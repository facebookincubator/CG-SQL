# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

LEX = flex
YACC = bison

CQL_DIR=./
O=out
TEST_DIR=test

TARGET = $(shell uname)
ifeq ($(TARGET), Linux)
LINUX = 1
endif

ifdef LINUX
SQLITE_PATH=$(shell hg root)/xplat/third-party/sqlite/sqlite-3280000/upstream_full/
CC = cc -g -std=c99 -D_Nullable= -D_Nonnull= -I$(SQLITE_PATH)
SQLITE_OBJ=$O/sqlite3-all.o
SQLITE_LINK=$(SQLITE_OBJ) -pthread -ldl
else
CC = cc -g
SQLITE_LINK=-lsqlite3
endif

CFLAGS+=-I$(CQL_DIR)
CFLAGS+=-I$O

ifdef COVERAGE
CFLAGS+=-O0 --coverage
endif

#note cql itself doesn't use cqlrt.o but we insist that it can be compiled
PROGRAMS = $O/cql $O/cqlrt.o $O/cqltest.o

all: ${PROGRAMS}

OBJECTS = $O/cql.y.o $O/cql.o $O/ast.o $O/gen_sql.o $O/sem.o $O/list.o $O/bytebuf.o $O/charbuf.o $O/cg_common.o $O/cg_c.o \
          $O/cg_java.o $O/cg_objc.o $O/symtab.o $O/compat.o \
          $O/cg_schema.o $O/crc64xz.o $O/cg_json_schema.o $O/cg_test_helpers.o $O/encoders.o \
          $O/unit_tests.o $O/cg_query_plan.o ${O}/minipool.o $O/cg_udf.o $O/rt.o

$O/%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

$O/cql: $(OBJECTS) $(SQLITE_OBJ)
	${CC} -DSQLITE_DEBUG ${CFLAGS} -o $O/cql $(OBJECTS)

$O/cql.y.c $O/cql.y.h: cql.y encoders.h charbuf.h
	${YACC} -y -vd cql.y -o $O/cql.y.c

$O/cql.c: cql.l
	${LEX} -o $*.c $<

$O/cql.y.o: $O/cql.y.c $O/cql.y.h

$O/cql.o: $O/cql.c $O/cql.y.h cql.h ast.h

$O/unit_tests.o: unit_tests.c unit_tests.h

$O/encoders.o: encoders.c encoders.h

$O/crc64xz.o: crc64xz.c crc64xz.h

$O/ast.o: ast.c ast.h cql.h encoders.h charbuf.h

$O/cqlrt.o: cqlrt.c cqlrt.h

$O/rt.o: rt.c rt.h

$O/gen_sql.o: gen_sql.c ast.h gen_sql.h charbuf.h cql.h encoders.h

$O/cg_schema.o: cg_schema.h cql.h sem.h ast.h list.h symtab.h cg_common.h crc64xz.h

$O/cg_json_schema.o: cg_json_schema.h cql.h sem.h ast.h list.h symtab.h cg_common.h crc64xz.h

$O/sem.o: sem.c sem.h ast.h list.h charbuf.h cql.h bytebuf.h

$O/list.o: list.c list.h ast.h cql.h

$O/compat.o: compat.c compat.h

$O/charbuf.o: charbuf.c charbuf.h cql.h

$O/bytebuf.o: bytebuf.c bytebuf.h cql.h

$O/cg_common.o: cg_common.c cg_common.h ast.h charbuf.h cql.h symtab.h sem.h

$O/cg_c.o: cg_c.c cg_c.h cg_common.h ast.h sem.h charbuf.h list.h cql.h ast.h

$O/cg_java.o: cg_java.c cg_java.h cg_common.h ast.h sem.h charbuf.h list.h cql.h ast.h

$O/cg_objc.o: cg_objc.c cg_objc.h cg_common.h ast.h sem.h charbuf.h list.h cql.h ast.h

$O/cg_test_helpers.o: cg_test_helpers.c cg_test_helpers.h cg_common.h sem.h charbuf.h list.h cql.h ast.h

$O/cg_query_plan.o: cg_query_plan.c cg_query_plan.h cg_common.h ast.h sem.h charbuf.h list.h cql.h ast.h

$O/cg_udf.o: cg_udf.c cg_udf.h cg_common.h ast.h sem.h charbuf.h list.h cql.h ast.h

$O/symtab.o: symtab.c symtab.h bytebuf.h

$O/minipool.o: minipool.c minipool.h

$O/sqlite3-all.o: $(SQLITE_PATH)/sqlite3-all.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -rf cql_amalgam.gcno
	rm -rf $O
	mkdir $O

amalgam: $O/cql.c $O/cql.y.c $O/cql.y.h
	bash make_amalgam.sh
	$(CC) $(CFLAGS) -o $O/cql_amalgam.o -c $O/cql_amalgam.c
	$(CC) $(CFLAGS) -o $O/cql_amalgam $O/cql_amalgam.o

$O/cql.y.o: $O/cql.y.c $O/cql.y.h

cg_test:
	$(CC) $(CFLAGS) -c -o $O/cg_test.o $O/cg_test_c.c

cg_test_extension_fragment:
	$(CC) $(CFLAGS) -c -o $O/cg_test_extension_fragment.o $O/cg_test_extension_fragment_c.c

cg_test_assembly_query:
	$(CC) $(CFLAGS) -c -o $O/cg_test_assembly_query.o $O/cg_test_assembly_query_c.c

cg_test_schema_upgrade:
	$(CC) $(CFLAGS) -c -o ${O}/cg_test_schema_upgrade.o ${O}/cg_test_schema_upgrade.c

$O/cqltest.o: $(TEST_DIR)/cqltest.c
	$(CC) $(CFLAGS) -c -o $O/cqltest.o $(TEST_DIR)/cqltest.c

$O/run_test.o: $O/run_test.c

$O/run_test_client.o: run_test_client.c

$O/result_set_extension.o: result_set_extension.c

$O/query_plan_test.o: query_plan_test.c

$O/query_plan.o: $O/query_plan.c

$O/udf.o: $O/udf.c

$O/cqlrt_mocked.o: cqlrt.c cqlrt.h
	$(CC) $(CFLAGS) -DCQL_RUN_TEST -c -o $O/cqlrt_mocked.o cqlrt.c

$O/cqlrt_compat.o: cqlrt.c cqlrt.h
	$(CC) $(CFLAGS) -DCQL_RUN_TEST -DCQL_COMPAT_VERSION_NUMBER=1 -c -o $O/cqlrt_compat.o cqlrt.c

RUN_TEST_ARGS=-o $O/a.out $O/run_test.o $O/cqltest.o $O/run_test_client.o $O/cqlrt_mocked.o $O/result_set_extension.o

COMPAT_TEST_ARGS=-o $O/a.out $O/run_test.o $O/cqltest.o $O/run_test_client.o $O/cqlrt_compat.o $O/result_set_extension.o

run_test: $O/cqltest.o $O/run_test.o $O/run_test_client.o $O/result_set_extension.o $O/cqlrt_mocked.o
	$(CC) $(CFLAGS) $(RUN_TEST_ARGS) $(SQLITE_LINK)

run_test_compat: $O/cqltest.o $O/run_test.o $O/run_test_client.o $O/result_set_extension.o $O/cqlrt_compat.o
	$(CC) $(CFLAGS) $(COMPAT_TEST_ARGS) $(SQLITE_LINK)

$O/generated_downgrade_test.o: $O/generated_downgrade_test.c $O/generated_downgrade_test.h

$O/downgrade_test_obj.o: upgrade/downgrade_test.c $O/cqlrt.o
	$(CC) $(CFLAGS) -c -o $O/downgrade_test_obj.o upgrade/downgrade_test.c

downgrade_test: $O/generated_downgrade_test.o $O/downgrade_test_obj.o $O/cqlrt.o
	$(CC) $(CFLAGS) -o $O/downgrade_test $O/cqlrt.o $O/generated_downgrade_test.o $O/downgrade_test_obj.o $(SQLITE_LINK)

$O/generated_upgrade_test$(RUN).o: $O/generated_upgrade_test$(RUN).c $O/generated_upgrade_test.h

$O/upgrade_test_obj.o: upgrade/upgrade_test.c $O/cqlrt.o
	$(CC) $(CFLAGS) -c -o $O/upgrade_test_obj.o upgrade/upgrade_test.c

upgrade_test: $O/generated_upgrade_test$(RUN).o $O/upgrade_test_obj.o $O/cqlrt.o
	$(CC) $(CFLAGS) -o $O/upgrade_test $O/cqlrt.o $O/generated_upgrade_test$(RUN).o $O/upgrade_test_obj.o $(SQLITE_LINK)

query_plan_test: $O/query_plan_test.o $O/cqlrt.o $O/query_plan.o $O/udf.o
	$(CC) $(CFLAGS) -o $O/query_plan_test $O/query_plan_test.o $O/cqlrt.o $O/query_plan.o $O/udf.o $(SQLITE_LINK)
