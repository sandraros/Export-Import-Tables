FUNCTION Z_EXPIMP_TABLE_TEST.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TABNAME) TYPE  TABNAME
*"     VALUE(KEY_XML) TYPE  XSTRING
*"  EXCEPTIONS
*"      IMPORT_ERROR
*"      STD_IMPORT_ERROR
*"      DBUF_IMPORT_CREATE_DATA
*"      Z_DBUF_IMPORT_CREATE_DATA
*"      DIFF_COUNT_LINES
*"      DIFF_OBJECT
*"----------------------------------------------------------------------
  DATA:
    dref_table_line TYPE REF TO data.
  FIELD-SYMBOLS:
    <keyline> TYPE any.


  CREATE DATA dref_table_line TYPE (tabname).
  ASSIGN dref_table_line->* TO <keyline>.
  CALL TRANSFORMATION id SOURCE XML key_xml RESULT data = <keyline>.

  TRY.
      zcl_expimp_table=>import_as_xstring(
        EXPORTING
          tabname = tabname
          id_new  = <keyline>
        IMPORTING
          xstring = DATA(xstring) ).
    CATCH zcx_expimp_table INTO DATA(lx3).
      RAISE import_error.
  ENDTRY.


  TRY.
      DATA(xstring2) = xstring.
      DATA(partab) = cl_abap_expimp_utilities=>dbuf_import_create_data( EXPORTING dbuf = xstring2 ).
      DATA(xstring3) = xstring.
      DATA(partab2) = cl_abap_expimp_utilities=>dbuf_import_create_data( EXPORTING dbuf = xstring3 ).
    CATCH zcx_expimp_table INTO DATA(lx2).
      RAISE z_dbuf_import_create_data.
  ENDTRY.

  " COMPARISON
  IF lines( partab2 ) <> lines( partab ).
    RAISE diff_count_lines.
  ENDIF.
  LOOP AT partab INTO DATA(cpar).
    DATA(cpar2) = VALUE #( partab2[ sy-tabix ] OPTIONAL ).
    ASSIGN cpar-dref->* TO FIELD-SYMBOL(<fs>).
    ASSIGN cpar2-dref->* TO FIELD-SYMBOL(<fs2>).
    IF cpar-name <> cpar2-name OR <fs> <> <fs2>.
      RAISE diff_object.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
