---
id: introduction
title: Introduction
sidebar_label: Introduction
---
CG/SQL is a code generation system for the popular SQLite library that allows developers to write stored procedures in a variant of Transact-SQL (T-SQL) and compile them into C code that uses SQLite’s C API to do the coded operations. CG/SQL enables engineers to create highly complex stored procedures with very large queries, without the manual code checking that existing methods require.

The full system also includes features for managing and upgrading schema, creating test code for stored procedures, getting query plans for procedures, as well as interfacing with stored procedures from other languages, such as Java and Objective-C. The JSON output allows for the creation of even more analysis or interfacing code. The package includes extensive documentation on the language and system.

:::tip
When we write "CQL" in these documents we're referring to the compiler proper, its flags, outputs and so forth.  In order to avoid confusion with the many similarly named systems (there are many!) we refer to the overall project as CG/SQL.  This comprises the docs, runtime, samples, everything.
:::

# What it does:

The CQL compiler does most of the heavy lifting. This compiler reads the schema and procedures, providing a strongly typed language with hundreds of compile-time errors designed to prevent runtime SQL issues. The compiler carefully tracks the data types of variables as well as schema types, and reports inconsistencies (such as trying to assign nullable columns to non-nullable output variables), and otherwise ensures that the SQLite APIs are used consistently and properly.

The generated code always checks the various return codes and always uses the correct column ordinals and column types when binding or reading data to or from the SQLite system — areas that are notoriously difficult to get right and keep right. Additionally, annotations on schema allow the system to automatically create stored procedures that will upgrade a database from any previous schema version to the current one, with dozens of checks in place to make this possible.

Annotations on procedures can also be used to indicate that you would like supporting test code to create schema fragments and insert data into that schema. This allows procedures to be unit tested with minimal fuss and no dependency on an as-deployed system. Likewise, these facilities can create schema to allow query plans to be checked at compile time.

# Why it matters:

SQLite is widely used, but creating well tested and maintainable data access layers can be challenging at best. Many teams use some kind of code generation to avoid having to change dozens of ordinals every time a column is added, but these can be error prone. The CQL compiler in CG/SQL allows you to create highly complex stored procedures with very large queries and with a combination of syntax helpers and strong typing these procedures are much easier to get right and keep right. The combination of strong typing in the language plus facilities for good unit testing can provide confidence that even very complex logic is correct. Syntax helpers convert safer code into the canonical SQL, allowing engineers to write less code that is more correct — and still runs everywhere. For example:

```
create procedure insert_a_row(like your_table)
begin
  insert into your_table from arguments;
end;
```

Creates a procedure that inserts into any table (e.g., your_table) whose arguments are exactly the columns of the table. You cannot possibly forget any columns, nor can you put the potentially dozens of arguments in the wrong order. These forms are not only brief but also highly error-resistant, making it easier for engineers to generate code without having to check every bit manually.

Original Source: [FB Engineering blog](https://engineering.fb.com/open-source/cg-sql/)
