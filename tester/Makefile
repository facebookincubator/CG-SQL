# (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TARGET = $(shell uname)
ifeq ($(TARGET), Linux)
LINUX = 1
endif

ifdef LINUX
SQLITE_PATH=$(shell hg root)/xplat/third-party/sqlite/sqlite-3280000/upstream_full/
CC = cc -g -std=c99 -D_Nullable= -D_Nonnull= -I$(SQLITE_PATH)
SQLITE=../out/sqlite3-all.o -pthread -ldl
else
CC = cc -g
SQLITE=-lsqlite3
endif

OUT_DIR=../out

# this is the master build
all : cql-verify

cql-verify : main.c ../cqlrt.c ../cqlrt.h dbhelp.c
	$(CC) -I.. -o ${OUT_DIR}/cql-verify main.c ../cqlrt.c dbhelp.c $(SQLITE)

regen:
	../out/cql --in dbhelp.sql --cg dbhelp.h dbhelp.c

clean:
	rm -f *.o ${OUT_DIR}/cql-verify

