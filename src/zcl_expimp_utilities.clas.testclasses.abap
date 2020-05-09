*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS nested_structure FOR TESTING RAISING cx_static_check.
    METHODS filler FOR TESTING RAISING cx_static_check.
    METHODS deep_structure FOR TESTING RAISING cx_static_check.
    METHODS filler_flat_structure FOR TESTING RAISING cx_static_check.
    METHODS clike_flat_structure FOR TESTING RAISING cx_static_check.
    METHODS primitive_c FOR TESTING RAISING cx_static_check.
    METHODS primitive_i FOR TESTING RAISING cx_static_check.
    METHODS primitive_string FOR TESTING RAISING cx_static_check.
    METHODS import_from_database.
    METHODS compare.
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

  METHOD clike_flat_structure.
    DATA: BEGIN OF struct,
            aaa TYPE c LENGTH 5,
            bbb TYPE c LENGTH 5,
          END OF struct.
    struct = VALUE #( aaa = |hello| bbb = |world| ).
    EXPORT structure-variable = struct TO DATA BUFFER blob.
    compare( ).
  ENDMETHOD.

  METHOD filler_flat_structure.
    DATA: BEGIN OF struct,
            aaa TYPE c LENGTH 5,
            bbb TYPE decfloat16,
          END OF struct.
    struct = VALUE #( aaa = |hello| bbb = 25 ).
    EXPORT structure-variable = struct TO DATA BUFFER blob.
*    compare( ).
  ENDMETHOD.

  METHOD deep_structure.
    DATA: BEGIN OF struct,
            aaa TYPE string,
            bbb TYPE string,
          END OF struct.
    struct = VALUE #( aaa = |hello| bbb = |world| ).
    EXPORT structure-variable = struct TO DATA BUFFER blob.
    compare( ).
  ENDMETHOD.

  METHOD itab.
*    DATA: BEGIN OF line,
*            aaa TYPE string,
*            bbb TYPE i,
*          END OF line,
*          itab LIKE TABLE OF line.
*    itab = VALUE #( ( aaa = |hello| ) ( aaa = |world| ) ).
*    EXPORT the-itab = itab TO DATA BUFFER blob.
*    compare( blob ).
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

  METHOD compare.
    DATA(partab2) = cl_abap_expimp_utilities=>dbuf_import_create_data( blob ).
    TRY.
        DATA(partab) = NEW zcl_expimp_utilities( )->dbuf_import_create_data( CHANGING dbuf = blob ).
      CATCH cx_root INTO DATA(lx).
        cl_abap_unit_assert=>fail( msg = 'error during custom IMPORT' ).
    ENDTRY.
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

  METHOD primitive_c.
    DATA aa TYPE c LENGTH 2 VALUE 'AB'.
    EXPORT character-variable = aa TO DATA BUFFER blob.
    compare( ).
  ENDMETHOD.

  METHOD primitive_i.
    EXPORT integer = 25 TO DATA BUFFER blob.
    compare( ).
  ENDMETHOD.

  METHOD primitive_string.
    EXPORT string = `Hello world` TO DATA BUFFER blob.
    compare( ).
  ENDMETHOD.

ENDCLASS.
