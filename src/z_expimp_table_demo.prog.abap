*&---------------------------------------------------------------------*
*& Report z_expimp_table_demo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_expimp_table_demo.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS main
      RETURNING
        VALUE(messages) TYPE string_table.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD main.

    SELECT *
        FROM eufunc
        WHERE gruppe = 'SCAL'
          AND name = 'DATE_GET_WEEK'
          AND nummer <> '999'
        INTO TABLE @DATA(id_wa_lines).

    IF sy-subrc <> 0.
      messages = VALUE #( ( |Missing test data in SE37 for DATE_GET_WEEK. Please create a dummy one.| ) ).
      RETURN.
    ENDIF.

    LOOP AT id_wa_lines INTO DATA(id_wa).

      messages = VALUE #(
          BASE messages
          ( |Test data { CONV i( id_wa-nummer ) }:| ) ).

      " Method IMPORT_ALL
      zcl_expimp_table=>import_all(
         EXPORTING client    = sy-mandt
                   table_name = 'EUFUNC'
                   area       = 'FL'
         IMPORTING tab_cpar   = DATA(tab_cpar)
         CHANGING  id_wa      = id_wa ).

      DATA(dref) = tab_cpar[ name = '%_IDATE' ]-dref.
      DATA(date1) = CAST d( dref )->*.

      " ABAP statement IMPORT
      DATA(id) = VALUE functdir(
          area   = 'SCAL'
          progid = 'DATE_GET_WEEK'
          dataid = id_wa-nummer ).
      DATA: date2 TYPE scal-date,
            wa    TYPE eufunc.

      IMPORT %_idate = date2 FROM DATABASE eufunc(fl) CLIENT sy-mandt ID id TO wa.

      " For some reasons, the first byte of WA-CLUSTD is set to 00 by SAP (7.52), don't know why.
      " So, for the equality assertion, copy the first byte.
      wa-clustd(1) = id_wa-clustd(1).

      IF date2 <> date1 OR wa <> id_wa.
        messages = VALUE #(
            BASE messages
            ( |ALERT: method IMPORT_ALL does not behave consistently with ABAP statement IMPORT| ) ).
      ELSE.
        messages = VALUE #(
            BASE messages
            ( |Method IMPORT_ALL behaves consistently with ABAP statement IMPORT| ) ).
      ENDIF.

      messages = VALUE #(
          BASE messages
          ( LINES OF VALUE #(
              FOR <cpar> IN tab_cpar
              WHERE ( name CP '%_*' )
              ( |{ substring( val = <cpar>-name off = 3 )
                  }: { SWITCH string( <cpar>-name
                  WHEN '%_IDATE' THEN CAST scal-date( <cpar>-dref )->*
                  WHEN '%_VWEEK' THEN CAST scal-week( <cpar>-dref )->* ) }| ) ) )
          ( |---------------------------------------------| ) ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(messages) = NEW lcl_app( )->main( ).
  LOOP AT messages INTO DATA(message).
    WRITE / message.
  ENDLOOP.
