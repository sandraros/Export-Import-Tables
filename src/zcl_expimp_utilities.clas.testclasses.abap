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
    METHODS version_03_filler_actual FOR TESTING RAISING cx_static_check.
    METHODS version_03_table FOR TESTING RAISING cx_static_check.
    METHODS version_03_structure FOR TESTING RAISING cx_static_check.
    METHODS version_03_filler FOR TESTING RAISING cx_static_check.

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
assert 1 = 1.
    DATA: BEGIN OF line,
            aaa TYPE c LENGTH 5,
            bbb TYPE i,
          END OF line,
          itab LIKE TABLE OF line.

    itab = VALUE #( ).
*    DATA(itab) = VALUE string_table( ).
assert 1 = 1.

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
        DATA(partab) = NEW zcl_expimp_utilities( )->dbuf_import_create_data( CHANGING dbuf = blob ).
      CATCH cx_root INTO DATA(lx2).
        DATA(b) = 0.
        b = b + 1.
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

  METHOD version_03_table.

    " In version 03, table IDs 'BE' and 'BF' don't exist, 'BB' is used to start each row

    TYPES: BEGIN OF ty_vari,
             cmp0001 TYPE c LENGTH 8,
           END OF ty_vari,
           tt_vari TYPE STANDARD TABLE OF ty_vari WITH EMPTY KEY.

    DATA(vari) = VALUE tt_vari( ( cmp0001 = 'FUNC' ) ( cmp0001 = 'FUNC' ) ).
    blob = 'FF030101010200003131303000000000' " Transport header version 03 (16 bytes)
        && '03000008002C06' " Object header v03 (7 bytes)
        && '255F56415249' " Object name : %_VARI
        && 'AA000008' " AA=simple data description C 8
        && 'BB46554E4320202020' " Line 1 - BB=single value - 'FUNC....' (. = space)
        && 'BB46554E4320202020' " Line 2 - BB=single value - 'FUNC....' (. = space)
        && '04'. " end

    compare2( VALUE #(
        ( name = '%_VARI' dref = REF #( vari ) ) ) ).

  ENDMETHOD.

  METHOD version_03_structure.

    " In version 03, table IDs 'BC' and 'BD' don't exist

    TYPES: BEGIN OF ty_vari,
             cmp0001 TYPE c LENGTH 8,
           END OF ty_vari.

    DATA(vari) = VALUE ty_vari( cmp0001 = 'FUNC' ).
    blob = 'FF030101010200003131303000000000' " Transport header version 03 (16 bytes)
        && '0200005C002C06' " Object header v03 (7 bytes) : 02=structure, 00=type C, 005C=length 92, 002C=offset 44 to next OH, 06=name length (%_VARI)
        && '255F56415249' " Object name : %_VARI
        && 'AA000008' " C 8
        && 'BB46554E4320202020' " FUNC
        && '04'.

    compare2( VALUE #(
        ( name = '%_VARI' dref = REF #( vari ) ) ) ).

  ENDMETHOD.

  METHOD version_03_filler.

    " In version 03, fillers are not described ('AF' in version 06)

    TYPES: BEGIN OF ty_vari,
             cmp0001 TYPE c LENGTH 8,
             cmp0002 TYPE i,
             cmp0003 TYPE c LENGTH 1,
             cmp0004 TYPE i,
           END OF ty_vari.

    " The filler occupies 3 bytes between CMP0003 and CMP0004
    DATA(vari) = VALUE ty_vari( cmp0001 = 'FUNC' cmp0003 = 'P' cmp0004 = 1 ).
    blob = 'FF030101010200003131303000000000' " Transport header version 03 (16 bytes)
        && '0200005C002C06' " Object header v03 (7 bytes)
        && '255F56415249' " Object name : %_VARI
        && 'AA000008' " C 8
        && 'AA080004' " I
        && 'AA000001' " C 1
        && 'AA080004' " I
        && 'BB46554E4320202020000000005000000000000001'
        && '04'.

    compare2( VALUE #(
        ( name = '%_VARI' dref = REF #( vari ) ) ) ).

  ENDMETHOD.

  METHOD version_03_filler_actual.

    TYPES: BEGIN OF ty_vari,
             cmp0001 TYPE c LENGTH 8,
             cmp0002 TYPE i,
             cmp0003 TYPE c LENGTH 1,
             cmp0004 TYPE i,
             cmp0005 TYPE c LENGTH 1,
             cmp0006 TYPE c LENGTH 30,
             cmp0007 TYPE c LENGTH 1,
             cmp0008 TYPE c LENGTH 1,
             cmp0009 TYPE i,
             cmp0010 TYPE c LENGTH 1,
             cmp0011 TYPE c LENGTH 30,
             cmp0012 TYPE c LENGTH 1,
             cmp0013 TYPE c LENGTH 1,
           END OF ty_vari,
           tt_vari TYPE STANDARD TABLE OF ty_vari WITH EMPTY KEY,
           BEGIN OF ty_varivdat,
             cmp0001 TYPE c LENGTH 8,
             cmp0002 TYPE c LENGTH 1,
             cmp0003 TYPE c LENGTH 2,
             cmp0004 TYPE i,
             cmp0005 TYPE i,
           END OF ty_varivdat,
           tt_varivdat TYPE STANDARD TABLE OF ty_varivdat WITH EMPTY KEY.

    DATA(vari) = VALUE tt_vari( ). " internal table
    DATA(varivdat) = VALUE tt_varivdat( ). " internal table
    DATA(prot) = 'F'.
    DATA(func) = 'S'.
    blob = 'FF030101010200003131303000000000' " Transport header version 03 (16 bytes)
        && '03000060004106' " Object header v03 (7 bytes) - flat table with lines of 96 bytes
        && '255F56415249' " Object name : %_VARI
        && 'AA000008' " C 8
        && 'AA080004' " I
        && 'AA000001' " C 1
        && 'AA080004' " I     preceded with 3 alignment bytes
        && 'AA000001' " C 1
        && 'AA00001E' " C 30
        && 'AA000001' " C 1
        && 'AA000001' " C 1
        && 'AA080004' " I     preceded with 3 alignment bytes
        && 'AA000001' " C 1
        && 'AA00001E' " C 30
        && 'AA000001' " C 1
        && 'AA000001' " C 1   followed by 3 alignment bytes
        && '0300001400250A' " Object header v03 (7 bytes) - flat table with lines of 20 bytes
        && '255F5641524956444154' " Object name : %_VARIVDAT
        && 'AA000008' " C 8
        && 'AA000001' " C 1
        && 'AA000002' " C 2
        && 'AA080004' " I     preceded with 1 alignment byte
        && 'AA080004' " I
        && '01000001000D04' " Object header v03 (7 bytes) - 1 character
        && '50524F54BB46' " Object name : PROT
        && '01000001000D04' " Object header v03 (7 bytes) - 1 character
        && '46554E43BB53' " Object name : FUNC
        && '04'.

    compare2( VALUE #(
        ( name = '%_VARI'     dref = REF #( vari ) )
        ( name = '%_VARIVDAT' dref = REF #( varivdat ) )
        ( name = 'PROT'       dref = REF #( prot ) )
        ( name = 'FUNC'       dref = REF #( func ) ) ) ).

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
