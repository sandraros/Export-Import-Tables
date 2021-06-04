*"* use this source file for your ABAP unit test classes

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
    METHODS primitive_f FOR TESTING RAISING cx_static_check.
    METHODS primitive_string FOR TESTING RAISING cx_static_check.
    METHODS itab_empty FOR TESTING RAISING cx_static_check.
    METHODS itab_one_line FOR TESTING RAISING cx_static_check.
    METHODS itab_two_lines FOR TESTING RAISING cx_static_check.
    METHODS iso_8859_1 FOR TESTING RAISING cx_static_check.

    METHODS compare.
    METHODS compare2 IMPORTING partab2 TYPE tab_cpar.

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
    struct = VALUE #( a = 25 b-b = 10 b-d = 30 ).
    EXPORT structure-variable = struct TO DATA BUFFER blob.

    compare2( VALUE #(
        ( name = 'STRUCTURE-VARIABLE' dref = REF #( struct ) ) ) ).

  ENDMETHOD.

  METHOD filler.

    DATA: BEGIN OF struct,
            a TYPE i,
            b TYPE decfloat16,
          END OF struct.
    struct = VALUE #( a = 25 b = 10 ).
    EXPORT structure-variable = struct TO DATA BUFFER blob.

    compare( ).

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

    compare( ).

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

  METHOD itab_one_line.

    DATA: BEGIN OF line,
            aaa TYPE c LENGTH 5,
            bbb TYPE i,
          END OF line,
          itab LIKE TABLE OF line.

    itab = VALUE #( ( aaa = |hello| bbb = 8 ) ).
    EXPORT the-itab = itab TO DATA BUFFER blob.

    compare( ).

  ENDMETHOD.

  METHOD itab_empty.

    DATA: BEGIN OF line,
            aaa TYPE c LENGTH 5,
            bbb TYPE i,
          END OF line,
          itab LIKE TABLE OF line.

    itab = VALUE #( ).
    EXPORT the-itab = itab TO DATA BUFFER blob.

    compare( ).

  ENDMETHOD.

  METHOD itab_two_lines.

    DATA: BEGIN OF line,
            aaa TYPE c LENGTH 5,
            bbb TYPE i,
          END OF line,
          itab LIKE TABLE OF line.

    itab = VALUE #( ( aaa = |hello| bbb = 8 ) ( aaa = |world| bbb = 20 ) ).
    EXPORT the-itab = itab TO DATA BUFFER blob.

    compare( ).

  ENDMETHOD.

  METHOD compare.

    TRY.
        DATA(partab2) = cl_abap_expimp_utilities=>dbuf_import_create_data( blob ).
      CATCH cx_sy_import_format_error INTO DATA(lx1).
        ASSERT 1 = 1. "handle exception
    ENDTRY.

    TRY.
*        DATA(partab) = NEW zcl_expimp_v6_importer( )->import( blob ).
      CATCH cx_root INTO DATA(lx2).
        DATA(b) = 0.
        b = b + 1.
        cl_abap_unit_assert=>fail( msg = 'error during custom IMPORT' ).
    ENDTRY.

*    " COMPARISON
*    IF lines( partab2 ) <> lines( partab ).
*      cl_abap_unit_assert=>fail( msg = 'are different' ).
*    ENDIF.
*    LOOP AT partab INTO DATA(cpar).
*      DATA(cpar2) = VALUE #( partab2[ sy-tabix ] OPTIONAL ).
*      ASSIGN cpar-dref->* TO FIELD-SYMBOL(<fs>).
*      ASSIGN cpar2-dref->* TO FIELD-SYMBOL(<fs2>).
*      IF cpar-name <> cpar2-name OR <fs> <> <fs2>.
*        cl_abap_unit_assert=>fail( msg = 'are different' ).
*      ENDIF.
*    ENDLOOP.

  ENDMETHOD.

  METHOD compare2.

    TRY.
        DATA(partab) = NEW zcl_expimp_utilities( )->dbuf_import_create_data( CHANGING dbuf = blob ).
      CATCH cx_root INTO DATA(lx2).
        cl_abap_unit_assert=>fail( msg = 'error during custom IMPORT' ).
    ENDTRY.

    " COMPARISON
    IF lines( partab2 ) <> lines( partab ).
      cl_abap_unit_assert=>fail( msg = 'are different' ).
    ENDIF.
    LOOP AT partab INTO DATA(cpar).
      DATA(cpar2) = VALUE #( partab2[ sy-tabix ] OPTIONAL ).
      ASSIGN cpar-dref->* TO FIELD-SYMBOL(<fs>).
      ASSIGN cpar2-dref->* TO FIELD-SYMBOL(<fs2>).
      IF cpar-name <> cpar2-name OR <fs> <> <fs2>.
        cl_abap_unit_assert=>fail( msg = 'are different' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD iso_8859_1.

    " EXPORT P_ACT_ID = 'SLSCN_COLLECT_DATA' (40C)
    blob = 'FF060201010280003131303000000000' " Transport header version 06 (16 bytes)
        && '0100000000002800000000080000000000000000000000000000000000000000' " Object header v06 (32 bytes)
        && '505F4143545F4944' " Object name : P_ACT_ID
        && 'BC00000028'
        && '534C53434E5F434F4C4C4543545F444154412020' " SLSCN_COLLECT_DATA (20 characters)
        && '2020202020202020202020202020202020202020' " spaces (20 characters)
        && 'BD04'.

    compare( ).

  ENDMETHOD.

  METHOD primitive_c.

    DATA aa TYPE c LENGTH 2 VALUE 'AB'.
    EXPORT character-variable = aa TO DATA BUFFER blob.
    DATA(b) = 0.
    compare( ).
    b = b + 1.
  ENDMETHOD.

  METHOD primitive_i.

    EXPORT integer = 25 TO DATA BUFFER blob.

    compare( ).

  ENDMETHOD.

  METHOD primitive_f.

    DATA f TYPE f.
    f = 25.
    EXPORT f = f TO DATA BUFFER blob.

    compare( ).

  ENDMETHOD.

  METHOD primitive_string.

    EXPORT string = `Hello world` TO DATA BUFFER blob.

    compare( ).

  ENDMETHOD.

ENDCLASS.
