declare proc printf no check;

-- Note that the same validator runs against every version, the validator knows what
-- to expect at each version and does different things.  This makes it a lot easier
-- to build each upgrader with built-in validation.  There's one central place (here)
-- where all validation happens.
create proc print_schema()
begin
  let version := cast(lua_upgrade_cql_get_facet_version("cql_schema_version") as integer);

  call printf("reference results for version %d\n\n", version);

  declare C cursor for select * from sqlite_master order by name;
  loop fetch C
  begin
    call printf("----- %s -----\n\n", C.name);
    call printf("type: %s\n", C.type);
    call printf("tbl_name: %s\n", C.tbl_name);

    -- Canonicalize and put in the split markers so we get some useful line breaks consistently
    -- Different SQLite versions will either preserve whitespace or not so this is an effort to
    -- normalize.  It's not perfect but it doesn't need to be, it only needs to work for
    -- schema the test will ever see.

    let s := (select replace(C.sql, "\n", " "));
    set s := (select replace(s, " ,", ","));
    set s := (select replace(s, " )", ")"));
    set s := (select replace(s, "( ", "("));
    set s := (select replace(s, "  ", " "));
    set s := (select replace(s, ",", ",$"));
    set s := (select replace(s, "(", "($"));

    -- split the string at the $ marks
    declare lines cursor for
      with split(line, str) as (
          select '', s || '$'
        union all
          select substr(str, 1, instr(str, '$') - 1), substr(str, instr(str, '$') + 1)
        from split
        where str != '')
      select line from (select trim(line) line from split) where line != '';

    -- some standard indenting, very simple
    let indent := 0;
    loop fetch lines
    begin
      let i := 0;
      let indent_str := "";
      while i < indent
      begin
        set indent_str := printf("%s%s", indent_str, "  "); -- ugh
        set i := i + 1;
      end;
      call printf("%s%s\n", indent_str, lines.line);

      -- trailing ( starts indent
      -- trailing ) ends indent
      let tail := (select substr(lines.line, length(lines.line)));
      if tail == '(' then
        set indent := indent + 1;
      else if tail == ')' then
        set indent := indent - 1;
      end if;

      -- trailing ), ends indent
      set tail := (select substr(lines.line, length(lines.line)-1));
      if tail == '),' then
        set indent := indent - 1;
      end if;
    end;
    call printf("\n");
  end;
end;

create procedure go()
begin
  begin transaction;
  call lua_upgrade_no_virtual_tables();
  call print_schema();
  commit transaction;
end;

@echo lua, "go(sqlite3.open(arg[1]))\n";
