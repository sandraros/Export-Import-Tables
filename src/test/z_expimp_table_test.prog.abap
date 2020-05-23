*&---------------------------------------------------------------------*
*& Report z_expimp_table_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_expimp_table_test.

SELECT tabname, as4local, tabclass
    FROM dd02l
    WHERE tabname LIKE 'ZEXPIMP_TEST%'
    INTO TABLE @DATA(dd02l_s).

SELECT tabname, fieldname, as4local, keyflag, position, rollname, datatype, leng, inttype, intlen
    FROM dd03l
    WHERE tabname LIKE 'ZEXPIMP_TEST%'
    INTO TABLE @DATA(dd03l_s).

DATA(test1) = 'LH 040120200423214800'.
DELETE FROM zexpimp_test1.
EXPORT test = 1 TO DATABASE zexpimp_test1(zz) ID test1.

DATA(test2) = 'LH 040120200423214800'.
DELETE FROM zexpimp_test2.
EXPORT test = 1 TO DATABASE zexpimp_test2(zz) ID test2.

DATA(test4) = 'LH'.
DELETE FROM zexpimp_test4.
EXPORT test = 1 TO DATABASE zexpimp_test4(zz) ID test4.

DATA(test5) = 'LH'.
DELETE FROM zexpimp_test5.
EXPORT test = 1 TO DATABASE zexpimp_test5(zz) ID test5.

ASSERT 1 = 1.
