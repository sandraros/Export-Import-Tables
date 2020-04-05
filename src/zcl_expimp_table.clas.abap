CLASS zcl_expimp_table DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS import_all
      IMPORTING
        mandant                  TYPE mandt DEFAULT sy-mandt
        export_import_table_name TYPE tabname
        area                     TYPE relid
      EXPORTING
        tab_cpar                 TYPE tab_cpar
      CHANGING
        id_wa                    TYPE any.

    CLASS-METHODS export_all
      IMPORTING
        mandant                  TYPE mandt DEFAULT sy-mandt
        tabname TYPE tabname
        area                     TYPE relid
        id_wa                    TYPE any
        tab_cpar                 TYPE tab_cpar.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_expimp_table IMPLEMENTATION.

  METHOD import_all.

    DATA:
      ls_dd02l        TYPE dd02l,
      lt_dd03l        TYPE TABLE OF dd03l,
      fieldname_mandt TYPE fieldname,
      area_index      TYPE i,
      ref_table       TYPE REF TO data,
      offset_clustr   TYPE i,
      offset_clustd   TYPE i,
      xstring         TYPE xstring,
      where           TYPE string,
      skip            TYPE i,
      string          TYPE string.
    FIELD-SYMBOLS:
      <ls_dd02l>       TYPE dd02l,
      <ls_dd03l_mandt> TYPE dd03l,
      <ls_dd03l_area>  TYPE dd03l,
      <table>          TYPE STANDARD TABLE,
      <first_field>    TYPE any,
      <line_fs>        TYPE x,
      <clustr>         TYPE any,
      <clustd>         TYPE any,
      <ls_dd03l>       TYPE dd03l,
      <first_line>     TYPE any,
      <length>         TYPE int2.


    SELECT SINGLE * FROM dd02l
      WHERE tabname  = @export_import_table_name
        AND as4local = 'A'
        AND as4vers  = 0
      INTO @ls_dd02l.
    CHECK sy-subrc = 0.
    ASSIGN ls_dd02l TO <ls_dd02l>.

    " Key columns except Client, RELID and SRTF2.
    SELECT * FROM dd03l
      INTO TABLE lt_dd03l
      WHERE tabname = export_import_table_name
        AND keyflag = 'X'
        AND as4local = 'A'
        AND as4vers = 0
        AND fieldname NOT LIKE '.%'
        AND fieldname NE 'SRTF2'.

    SORT lt_dd03l BY position.


    " Build WHERE
    IF <ls_dd02l>-clidep = abap_true.
      READ TABLE lt_dd03l WITH KEY datatype = 'CLNT' ASSIGNING <ls_dd03l_mandt>.
      fieldname_mandt = <ls_dd03l_mandt>-fieldname.
    ELSE.
      UNASSIGN <ls_dd03l_mandt>.
      CLEAR fieldname_mandt.
    ENDIF.

    IF <ls_dd02l>-clidep = abap_true.
      area_index = 2.
    ELSE.
      area_index = 1.
    ENDIF.
    READ TABLE lt_dd03l INDEX area_index ASSIGNING <ls_dd03l_area>.
    ASSERT sy-subrc = 0.

    where = ''.
    IF <ls_dd02l>-clidep = abap_true.
      where = |{ <ls_dd03l_mandt>-fieldname } = '{ mandant }' AND |.
    ENDIF.
    where = |{ where }{ <ls_dd03l_area>-fieldname } = { cl_abap_dyn_prg=>quote( area ) }|.

    IF <ls_dd02l>-clidep = abap_false.
      skip = 1.
    ELSE.
      skip = 2.
    ENDIF.
    LOOP AT lt_dd03l ASSIGNING <ls_dd03l> WHERE tabname = <ls_dd02l>-tabname.
      IF skip <> 0.
        SUBTRACT 1 FROM skip.
      ELSE.
        ASSIGN COMPONENT <ls_dd03l>-fieldname OF STRUCTURE id_wa TO FIELD-SYMBOL(<field>).
        IF sy-subrc <> 0.
          " raise error
        ENDIF.
        where = |{ where } AND { <ls_dd03l>-fieldname } = { cl_abap_dyn_prg=>quote( <field> ) }|.
      ENDIF.
    ENDLOOP.

    " Equivalent to IMPORT (all-fields) FROM DATABASE <table>(<area>) TO <wa> CLIENT <client> ID <id>
    CREATE DATA ref_table TYPE TABLE OF (<ls_dd02l>-tabname).
    ASSIGN ref_table->* TO <table>.
    SELECT * FROM (<ls_dd02l>-tabname) CLIENT SPECIFIED
          INTO TABLE <table>
          WHERE (where).
    IF sy-subrc <> 0.
      " NOT FOUND
      RETURN.
    ENDIF.

    SORT <table> BY table_line.

    " TAB_CPAR
    READ TABLE <table> INDEX 1 ASSIGNING <first_line>.
    ASSIGN COMPONENT 1 OF STRUCTURE <first_line> TO <first_field>.
    ASSIGN COMPONENT 'CLUSTR' OF STRUCTURE <first_line> TO <clustr>.
    DESCRIBE DISTANCE BETWEEN <first_field> AND <clustr> INTO offset_clustr IN BYTE MODE.
    ASSIGN COMPONENT 'CLUSTD' OF STRUCTURE <first_line> TO <clustd>.
    DESCRIBE DISTANCE BETWEEN <first_field> AND <clustd> INTO offset_clustd IN BYTE MODE.

    CLEAR xstring.
    LOOP AT <table> ASSIGNING <line_fs> CASTING.
      ASSIGN <line_fs>+offset_clustr(2) TO <length> CASTING.
      CONCATENATE xstring <line_fs>+offset_clustd(<length>) INTO xstring IN BYTE MODE.
    ENDLOOP.

    tab_cpar = cl_abap_expimp_utilities=>dbuf_import_create_data( dbuf = xstring ).

    " WA
    CLEAR <clustr>.
    CLEAR <clustd>.
    id_wa = <first_line>.

  ENDMETHOD.

  METHOD export_all.

    TYPES: BEGIN OF ty_pdat_line,
             paramname TYPE string,
             varname   TYPE c LENGTH 255,
           END OF ty_pdat_line,
           ty_pdat_lines TYPE STANDARD TABLE OF ty_pdat_line WITH EMPTY KEY.
    FIELD-SYMBOLS:
    <table> TYPE STANDARD TABLE.
    TYPES: BEGIN OF ty_pdat_level3,
             p0 TYPE REF TO data,
             p1 TYPE REF TO data,
             p2 TYPE REF TO data,
             p3 TYPE REF TO data,
             p4 TYPE REF TO data,
             p5 TYPE REF TO data,
             p6 TYPE REF TO data,
             p7 TYPE REF TO data,
             p8 TYPE REF TO data,
             p9 TYPE REF TO data,
           END OF ty_pdat_level3,
           BEGIN OF ty_pdat_level2,
             p0 TYPE ty_pdat_level3,
             p1 TYPE ty_pdat_level3,
             p2 TYPE ty_pdat_level3,
             p3 TYPE ty_pdat_level3,
             p4 TYPE ty_pdat_level3,
             p5 TYPE ty_pdat_level3,
             p6 TYPE ty_pdat_level3,
             p7 TYPE ty_pdat_level3,
             p8 TYPE ty_pdat_level3,
             p9 TYPE ty_pdat_level3,
           END OF ty_pdat_level2,
           BEGIN OF ty_pdat_level1,
             p0 TYPE ty_pdat_level2,
             p1 TYPE ty_pdat_level2,
             p2 TYPE ty_pdat_level2,
             p3 TYPE ty_pdat_level2,
             p4 TYPE ty_pdat_level2,
             p5 TYPE ty_pdat_level2,
             p6 TYPE ty_pdat_level2,
             p7 TYPE ty_pdat_level2,
             p8 TYPE ty_pdat_level2,
             p9 TYPE ty_pdat_level2,
           END OF ty_pdat_level1.
    DATA:
      ref_table TYPE REF TO data,
      ref_line  TYPE REF TO data,
      p         TYPE ty_pdat_level1.

    DATA(xstring) = VALUE xstring( ).

    DATA(level1) = 0.
    DATA(level2) = 0.
    DATA(level3) = 0.

    LOOP AT tab_cpar REFERENCE INTO DATA(cpar).

      DATA(varname) = |P-P{ level1 }-P{ level2 }-P{ level3 }|.
      ASSIGN (varname) TO FIELD-SYMBOL(<var>).
      <var> = cpar->dref.

      DATA pdat_lines TYPE ty_pdat_lines.
      pdat_lines = VALUE #( BASE pdat_lines
          ( paramname = cpar->name varname = |{ varname }->*| ) ).

      level3 = level3 + 1.
      IF level3 = 10.
        level3 = 0.
        level2 = level2 + 1.
        IF level2 = 10.
          level2 = 0.
          level1 = level1 + 1.
          IF level1 = 10.
            " TODO MORE THAN 1000 DATA OBJECTS !
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.

    EXPORT (pdat_lines) TO DATA BUFFER xstring COMPRESSION ON.

    CREATE DATA ref_line TYPE (tabname).
    ASSIGN ref_line->* TO FIELD-SYMBOL(<line>).
    CREATE DATA ref_table TYPE TABLE OF (tabname).
    ASSIGN ref_table->* TO <table>.

    ASSIGN COMPONENT 1 OF STRUCTURE <line> TO FIELD-SYMBOL(<first_field>).
    ASSIGN COMPONENT 'RELID' OF STRUCTURE <line> TO FIELD-SYMBOL(<relid>).
    DESCRIBE DISTANCE BETWEEN <first_field> AND <relid> INTO DATA(offset_relid) IN BYTE MODE.
    ASSIGN COMPONENT 'SRTF2' OF STRUCTURE <line> TO FIELD-SYMBOL(<srtf2>).
    ASSIGN COMPONENT 'CLUSTR' OF STRUCTURE <line> TO FIELD-SYMBOL(<clustr>).
    DESCRIBE DISTANCE BETWEEN <first_field> AND <clustr> INTO DATA(offset_clustr) IN BYTE MODE.
    ASSIGN COMPONENT 'CLUSTD' OF STRUCTURE <line> TO FIELD-SYMBOL(<clustd>).
    DESCRIBE DISTANCE BETWEEN <first_field> AND <clustd> INTO DATA(offset_clustd) IN BYTE MODE.
    DESCRIBE FIELD <clustd> LENGTH DATA(length_clustd) IN BYTE MODE.

    <line> = id_wa.
    " Inject MANDANT + AREA + ID into <line>
    IF offset_relid > 0.
      <first_field> = mandant.
    ENDIF.
    <relid> = area.

    DATA(offset) = 0.
    <srtf2> = 0.
    WHILE offset < xstrlen( xstring ).
      <clustd> = xstring+offset.
      offset = offset + length_clustd.
      IF offset < xstrlen( xstring ).
        <clustr> = length_clustd.
      ELSE.
        <clustr> = xstrlen( xstring ) - offset + length_clustd.
      ENDIF.
      APPEND <line> TO <table>.
      ADD 1 TO <srtf2>.
    ENDWHILE.

    MODIFY (tabname) FROM TABLE <table>.

  ENDMETHOD.

ENDCLASS.
