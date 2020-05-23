*&---------------------------------------------------------------------*
*& Report z_expimp_table_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_expimp_table_test2.

DATA gv_tabname TYPE tabname.
SELECT-OPTIONS tabnames FOR gv_tabname DEFAULT 'VARI*' SIGN I OPTION CP.

START-OF-SELECTION.
  PERFORM main.
  ASSERT 1 = 1.

FORM main.

  DATA: dd03l_tabnames TYPE TABLE OF dd03l-tabname,
        tabname        TYPE dd03l-tabname,
        exception_text TYPE c LENGTH 80.

  SELECT DISTINCT tabname
      FROM dd03l
      WHERE tabname  IN @tabnames
        AND as4local = 'A'
        AND fieldname = 'CLUSTD'
      INTO TABLE @dd03l_tabnames.

  LOOP AT dd03l_tabnames INTO tabname.
    TRY.
        DATA(info) = zcl_expimp_table=>get_info( tabname ).
      CATCH zcx_expimp_table ##NO_HANDLER.
        DELETE dd03l_tabnames USING KEY loop_key.
    ENDTRY.
  ENDLOOP.

  LOOP AT dd03l_tabnames INTO tabname.

    DATA(ref_to_keytab) = zcl_expimp_table=>create_keytab_for_get_keys( tabname ).

    FIELD-SYMBOLS: <keytab> TYPE STANDARD TABLE.
    ASSIGN ref_to_keytab->* TO <keytab>.

    zcl_expimp_table=>get_keys(
      EXPORTING
        tabname                 = tabname
*    client                  =
*    area                    =
*    id                      =
*    generic_key             = ABAP_FALSE
*    with_user_header        = ABAP_FALSE
*    client_specified        = ABAP_FALSE
      IMPORTING
        keytab                  = <keytab>
    ).
*  CATCH zcx_expimp_table.    "
*  CATCH cx_sy_client.    "
*  CATCH cx_sy_generic_key.    "
*  CATCH cx_sy_tabline_too_short.    "
*  CATCH cx_sy_incorrect_key.    "

    LOOP AT <keytab> ASSIGNING FIELD-SYMBOL(<keyline>).

      CALL TRANSFORMATION id SOURCE data = <keyline> RESULT XML DATA(key_xml).

      WRITE / 'ok'.

      CALL FUNCTION 'Z_EXPIMP_TABLE_TEST'
        DESTINATION 'NONE'
        EXPORTING
          tabname                   = tabname
          key_xml                   = key_xml
        EXCEPTIONS
          import_error              = 1
          dbuf_import_create_data   = 2
          z_dbuf_import_create_data = 3
          system_failure            = 4 MESSAGE exception_text
          OTHERS                    = 5.

      IF sy-subrc = 0.
        WRITE / 'ok'.
      ELSE.
        CASE sy-subrc.
          WHEN 1.
            WRITE / 'IMPORT_ERROR'.
          WHEN 2.
            WRITE / 'DBUF_IMPORT_CREATE_DATA'.
          WHEN 3.
            WRITE / 'Z_DBUF_IMPORT_CREATE_DATA'.
          WHEN 4.
            WRITE : / 'SYSTEM_FAILURE', exception_text.
          WHEN 5.
            WRITE / 'OTHERS'.
        ENDCASE.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

ENDFORM.
