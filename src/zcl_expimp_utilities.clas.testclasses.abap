*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS nested_structure FOR TESTING RAISING cx_static_check.
    METHODS filler FOR TESTING RAISING cx_static_check.
    METHODS flat_structure FOR TESTING RAISING cx_static_check.
    METHODS primitive FOR TESTING RAISING cx_static_check.
    METHODS import_from_database.
    METHODS compare IMPORTING blob TYPE xstring.
    METHODS itab.
    DATA blob TYPE xstring.
ENDCLASS.

CLASS ltc_main IMPLEMENTATION.

  METHOD nested_structure.
    DATA: BEGIN OF struct,
            a TYPE i,
            BEGIN OF b,
              b TYPE decfloat16,
              c TYPE decfloat34,
              d TYPE int8,
            END OF b,
          END OF struct.
  ENDMETHOD.

  METHOD filler.
    DATA: BEGIN OF struct,
            a TYPE i,
            b TYPE decfloat16,
          END OF struct.
  ENDMETHOD.

  METHOD flat_structure.
  ENDMETHOD.

  METHOD itab.
  DATA: BEGIN OF line,
          aaa TYPE string,
          bbb TYPE i,
        END OF line,
        itab LIKE TABLE OF line.
  ITAB = VALUE #( ( aaa = |hello| ) ( aaa = |world| ) ).
*  ITAB = VALUE string_table( ( |hello| ) ( |world| ) ).
  ENDMETHOD.

  METHOD import_from_database.
    DATA(eudb_key) = VALUE rseu1_key(
        name  = '/ASU/ASUSTART'
        sprsl = 'D' ).
    DATA(string) = repeat( val = ` ` occ = 10 ).
    EXPORT string = string TO DATABASE eudb(zz) ID eudb_key.

    DATA dref TYPE REF TO data.
    FIELD-SYMBOLS <fs> TYPE any.

    SELECT * FROM eudb
        WHERE relid = 'ZZ'
          AND name  = '/ASU/ASUSTART'
          AND sprsl = 'D'
        INTO TABLE @DATA(table_lines).

    ROLLBACK WORK.

*  SELECT * FROM vari
*      CLIENT SPECIFIED
*      WHERE mandt   = '000'
*        AND relid   = 'VA'
*        AND report  = 'ABAP_INTROSPECTOR_01'
*        AND variant = 'SAP&BOPF'
*      INTO TABLE @DATA(table_lines).

    DATA(blob) = VALUE xstring( ).
    LOOP AT table_lines REFERENCE INTO DATA(table_line).
      blob = blob && table_line->clustd(table_line->clustr).
    ENDLOOP.
  ENDMETHOD.

  METHOD primitive.
    DATA aa TYPE c LENGTH 2 VALUE 'AB'.
    EXPORT name = aa TO DATA BUFFER blob.
    compare( blob ).
  ENDMETHOD.

  METHOD compare.
    TRY.
        DATA(partab) = NEW zcl_expimp_utilities( )->dbuf_import_create_data( blob ).
      CATCH cx_root INTO DATA(lx).
        cl_abap_unit_assert=>fail( msg = 'error during custom IMPORT' ).
    ENDTRY.
    DATA(partab2) = cl_abap_expimp_utilities=>dbuf_import_create_data( blob ).
    IF lines( partab2 ) <> lines( partab ).
      cl_abap_unit_assert=>fail( msg = 'are different' ).
    ENDIF.
    " COMPARISON
    LOOP AT partab INTO DATA(cpar).
      DATA(cpar2) = VALUE #( partab2[ sy-tabix ] OPTIONAL ).
      ASSIGN cpar-dref->* TO FIELD-SYMBOL(<fs>).
      ASSIGN cpar2-dref->* TO FIELD-SYMBOL(<fs2>).
      IF cpar-name <> cpar2-name OR <fs> <> <fs2>.
        cl_abap_unit_assert=>fail( msg = 'are different' ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
