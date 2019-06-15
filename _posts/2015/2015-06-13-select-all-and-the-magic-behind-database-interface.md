---
layout: post
title: SELECT * and the magic behind database interface
lang: en
tags: [sap, abap]
ref: select-all-and-the-magic-behind-database-interface
---

The `Database Interface` is the middleware between the application and the database. It's his responsibility of translating OpenSQL to NativeSQL, keeping the data in buffer up-to-date, among other database related things.

Recently I found out something additional that it does and it's healthy to the database and the system overall.

It's very common to see arguments against the use of `SELECT *` in any language because it may degrade the performance by querying and transporting unnecessary data between various layers.

Fortunately SAP system have this magical database interface component that does some really cool optimizations. One of them is the selection of only the fields you really need.

Here are three examples. The first one using `SELECT fieldlist`. The other two are using `SELECT *`, but one of them targets a table with less fields.

~~~
TYPES: BEGIN OF ty_field,
        tabname TYPE dd03l-tabname,
        fieldname TYPE dd03l-fieldname,
       END OF ty_field.

DATA: gt_fields1 TYPE TABLE OF ty_field.
DATA: gt_fields2 TYPE TABLE OF ty_field.
DATA: gt_fields3 TYPE TABLE OF dd03l.

SELECT tabname fieldname
  FROM dd03l
  INTO TABLE gt_fields1
  WHERE tabname = 'PARTNER_RECORD'.

SELECT *
  FROM dd03l
  INTO CORRESPONDING FIELDS OF TABLE gt_fields2
  WHERE tabname = 'PARTNER_RECORD'.

SELECT *
  FROM dd03l
  INTO TABLE gt_fields3
  WHERE tabname = 'PARTNER_RECORD'.
~~~

The first example is probably the most accepted method, where each field is set both in the query and in the target table variable. I tend to believe it makes the code easier to read and much more optimized for database reading. When executing `SQL Trace (ST05)` on this program it it possible to note that the execution time of the first two queries are exactly the same, except for the third query which is a bit more expensive.

![](/public/images/st05-select-all.png)

That, my friends, it's `Database Interface` in action.

The real query that was sent to the database for the first two queries are exactly the same. This means that the system identified the `INTO CORRESPONDING FIELDS` instruction and that the target table contains only a few fields, so it replaced the all query for a specific query containing only the fields that are really used.

Happy `SELECT *` coding. But remember kids, with great powers comes great responsibility. Use it wisely.

Cheers!