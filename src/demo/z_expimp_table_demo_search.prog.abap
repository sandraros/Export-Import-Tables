*&---------------------------------------------------------------------*
*& Report z_expimp_table_view
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_expimp_table_demo_search LINE-SIZE 1023.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    TYPES: ty_range_tabnames TYPE RANGE OF tabname,
           ty_id             TYPE x LENGTH 1,
           ty_version        TYPE x LENGTH 1,
           ty_int_fmt        TYPE x LENGTH 1,
           ty_flt_fmt        TYPE x LENGTH 1,
           ty_compress       TYPE x LENGTH 1,
           ty_dd             TYPE x LENGTH 1,
           ty_range_ids      TYPE RANGE OF ty_id,
           ty_range_versions TYPE RANGE OF ty_version,
           ty_range_int_fmts TYPE RANGE OF ty_int_fmt,
           ty_range_flt_fmts TYPE RANGE OF ty_flt_fmt,
           ty_range_compress TYPE RANGE OF ty_compress,
           ty_range_dds      TYPE RANGE OF ty_dd.

    METHODS main
      IMPORTING
        client     TYPE mandt DEFAULT sy-mandt
        tabnames   TYPE ty_range_tabnames
        area       TYPE relid
        id         TYPE clike OPTIONAL
        invalid    TYPE abap_bool
        ids        TYPE ty_range_ids
        versions   TYPE ty_range_versions
        int_fmts   TYPE ty_range_int_fmts
        flt_fmts   TYPE ty_range_flt_fmts
        compress   TYPE ty_range_compress
        dds        TYPE ty_range_dds
        max_rows   TYPE i
      RETURNING
        VALUE(xml) TYPE string
      RAISING
        zcx_expimp_table.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD main.

    DATA:
      ref_to_table TYPE REF TO data,
      count_rows   TYPE i.
    FIELD-SYMBOLS:
      <table>  TYPE STANDARD TABLE,
      <line>   TYPE any,
      <clustd> TYPE xstring,
      <id>     TYPE c.

    SELECT DISTINCT tabname
        FROM dd03l
        WHERE tabname  IN @tabnames
          AND as4local = 'A'
          AND fieldname = 'CLUSTD'
        INTO TABLE @DATA(dd03l_s).

    LOOP AT dd03l_s INTO DATA(dd03l).

      TRY.
          DATA(info) = zcl_expimp_table=>get_info( dd03l-tabname ).
        CATCH zcx_expimp_table ##NO_HANDLER.
          CONTINUE.
      ENDTRY.

      CREATE DATA ref_to_table TYPE TABLE OF (dd03l-tabname).
      ASSIGN ref_to_table->* TO <table>.

      DATA(where_tab) = VALUE string_table(
          ( COND #( WHEN info-client_fieldname <> '' THEN |{ info-client_fieldname } LIKE '{ CONV symandt( client && '%' ) }'| ) )
          ( COND #( WHEN info-is_structure_one_row = abap_false THEN |SRTF2 = 0| ) ) ).
      DELETE where_tab WHERE table_line IS INITIAL.
      DATA(where) = concat_lines_of( table = where_tab sep = | AND | ).

      SELECT *
          FROM (dd03l-tabname) CLIENT SPECIFIED
          INTO TABLE @<table>
          WHERE (where).

* ID
** FF
* Version
** 01
** 02
** 03
** 04
** 05
** 06 (UNICODE 6.10+ ?)
* IntFormat
** 01 : big ENDIAN
** 02 : little ENDIAN
* FloatFormat
** 01 : big ENDIAN
** 02 : little ENDIAN
* Compress
* unused1 (2 bytes) : ??? (for example 8000)
* CODEPAGE (4 bytes, US-ASCII 128) : (:include8 sap_code_page:) (for example 34313033 = 4103)
* unused2 (4 bytes) : ??? (for example 00000000)
      IF info-is_structure_one_row = abap_false.
        IF invalid = abap_true.
          where = |CLUSTR >= 16|.
        ELSE.
          " CLUSTD is of type X
          where = 'NOT ( clustd+0(1) IN ids'
                & ' AND clustd+1(1) IN versions'
                & ' AND clustd+2(1) IN int_fmts'
                & ' AND clustd+3(1) IN flt_fmts'
                & ' AND clustd+4(1) IN compress'
                & ' AND clustd+5(1) IN dds )'.
        ENDIF.
        DELETE <table> WHERE (where).
      ELSE.
        " CLUSTD is of type XSTRING
        IF invalid = abap_true.
          LOOP AT <table> ASSIGNING <line>.
            ASSIGN COMPONENT 'CLUSTD' OF STRUCTURE <line> TO <clustd>.
            IF xstrlen( <clustd> ) >= 16.
              DELETE <table> USING KEY loop_key.
            ENDIF.
          ENDLOOP.
        ELSE.
          LOOP AT <table> ASSIGNING <line>.
            ASSIGN COMPONENT 'CLUSTD' OF STRUCTURE <line> TO <clustd>.
            IF NOT ( <clustd>+0(1) IN ids
                   AND <clustd>+1(1) IN versions
                   AND <clustd>+2(1) IN int_fmts
                   AND <clustd>+3(1) IN flt_fmts
                   AND <clustd>+4(1) IN compress
                   AND <clustd>+5(1) IN dds ).
              DELETE <table> USING KEY loop_key.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

      IF <table> IS NOT INITIAL.

        IF info-is_structure_one_row = abap_true.
          DATA(ref_to_keytab) = zcl_expimp_table=>create_keytab_for_get_keys( tabname = dd03l-tabname ).
          FIELD-SYMBOLS: <keytab> TYPE STANDARD TABLE.
          ASSIGN ref_to_keytab->* TO <keytab>.
          DATA ref_to_key TYPE REF TO data.
          CREATE DATA ref_to_key LIKE LINE OF <keytab>.
          ASSIGN ref_to_key->* TO FIELD-SYMBOL(<key>).
        ENDIF.

        LOOP AT <table> ASSIGNING <line>.

          IF info-client_fieldname <> ''.
            ASSIGN COMPONENT info-client_fieldname OF STRUCTURE <line> TO FIELD-SYMBOL(<client>).
            DATA(l_client) = CONV mandt( <client> ).
          ENDIF.

          ASSIGN COMPONENT 'RELID' OF STRUCTURE <line> TO FIELD-SYMBOL(<relid>).
          DATA(l_relid) = CONV indx_relid( <relid> ).

          zcl_expimp_table=>import_as_xstring(
            EXPORTING
              client   = l_client
              tabname  = dd03l-tabname
              area     = l_relid
              id_new   = <line>
            IMPORTING
              xstring  = DATA(xstring) ).

          IF info-is_structure_one_row = abap_false.
            ASSIGN <line>+info-id_offset(info-id_length) TO <id> CASTING.
          ELSE.
            " that's a export/Import table with one-row format, which means
            " that CLUSTD is a RAWSTRING/XSTRING column, subfield offset cannot
            " be used on such structure, so transfer fields to a structure
            " which doesn't have the XSTRING field, and then subfield offset
            " can be used.
            <key> = CORRESPONDING #( <line> ).
            ASSIGN <key>+info-id_offset(info-id_length) TO <id> CASTING.
          ENDIF.

          WRITE : / dd03l-tabname, l_client, l_relid, <id>, xstring.

          ADD 1 TO count_rows.
          IF count_rows = max_rows.
            RETURN.
          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

TABLES sscrfields.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE titl_b01.
DATA gv_tabname TYPE tabname.
SELECT-OPTIONS tabnames FOR gv_tabname.
PARAMETERS client TYPE symandt DEFAULT sy-mandt MODIF ID cli.
PARAMETERS area TYPE relid.
PARAMETERS id TYPE string LOWER CASE.
PARAMETERS max_rows TYPE i DEFAULT 100.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE titl_b02.
PARAMETERS invalid AS CHECKBOX.
DATA g_id TYPE x LENGTH 1.
SELECT-OPTIONS ids FOR g_id.
DATA g_version TYPE x LENGTH 1.
SELECT-OPTIONS versions FOR g_version. " between 01 and 06
DATA g_int_fmt TYPE x LENGTH 1.
SELECT-OPTIONS int_fmts FOR g_int_fmt.
DATA g_flt_fmt TYPE x LENGTH 1.
SELECT-OPTIONS flt_fmts FOR g_flt_fmt.
DATA g_compress TYPE x LENGTH 1.
SELECT-OPTIONS compress FOR g_compress.
DATA g_dd TYPE x LENGTH 1.
SELECT-OPTIONS dds FOR g_dd.
SELECTION-SCREEN END OF BLOCK b02.

INITIALIZATION.
  titl_b01 = ''(b01).
  titl_b02 = ''(b02).

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.

START-OF-SELECTION.

  DATA(xml) = NEW lcl_app( )->main(
      client   = client
      tabnames = tabnames[]
      area     = area
      id       = id
      invalid  = invalid
      ids      = ids[]
      versions = versions[]
      int_fmts = int_fmts[]
      flt_fmts = flt_fmts[]
      compress = compress[]
      dds      = dds[]
      max_rows = max_rows  ).

  cl_demo_output=>display_xml( xml ).
