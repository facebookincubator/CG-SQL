@echo c, "#undef cql_error_trace\n";
@echo c, "#define cql_error_trace() fprintf(stderr, \"SQL Failure %d %s: %s %d\\n\", _rc_, sqlite3_errmsg(_db_), __FILE__, __LINE__)\n";
