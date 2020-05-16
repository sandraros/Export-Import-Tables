*&---------------------------------------------------------------------*
*& Report z_expimp_table_view
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_expimp_table_search LINE-SIZE 1023.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    TYPES ty_range_tabnames TYPE RANGE OF tabname.
    TYPES ty_version TYPE x LENGTH 1.
    TYPES ty_range_versions TYPE RANGE OF ty_version.
    METHODS main
      IMPORTING
        client     TYPE mandt DEFAULT sy-mandt
        tabnames   TYPE ty_range_tabnames
        area       TYPE relid
        id         TYPE clike OPTIONAL
        versions   TYPE ty_range_versions
      RETURNING
        VALUE(xml) TYPE string
      RAISING
        zcx_expimp_table.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD main.

    DATA: ref_wa           TYPE REF TO data,
          xml_key_fields   TYPE string,
          xml_attr_fields  TYPE string,
          xml_data_objects TYPE string,
          generic          TYPE abap_trans_srcbind_tab.

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

      FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
      DATA ref_to_table TYPE REF TO DATA.
      CREATE DATA ref_to_table type table of (dd03l-tabname).
      ASSIGN ref_to_table->* TO <table>.

      data(where) = value string_table(
          ( COND #( WHEN info-client_fieldname <> '' THEN |{ info-client_fieldname } LIKE '{ CONV symandt( client && '%' ) }'| ) )
          ( COND #( WHEN info-is_structure_one_row = abap_false THEN |SRTF2 = 0| ) ) ).
      DELETE where WHERE table_line IS INITIAL.
      DATA(where2) = concat_lines_of( table = where sep = | AND | ).

      SELECT *
          FROM (dd03l-tabname) CLIENT SPECIFIED
          INTO TABLE @<table>
          WHERE (where2).

      LOOP AT <table> ASSIGNING FIELD-SYMBOL(<line>)
          WHERE ('clustd+1(1) IN versions').
*            AND clustd+2(1) IN versions.
*        DELETE <table> USING KEY loop_key.

        if info-client_fieldname <> ''.
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

        FIELD-SYMBOLS <chars> TYPE c.
        ASSIGN <line>+info-id_offset(info-id_length) TO <chars> CASTING.

        WRITE : / dd03l-tabname, l_client, l_relid, <chars>, xstring.

      ENDLOOP.

*      DATA(client_specified) = COND #( WHEN client IS NOT INITIAL THEN abap_true ELSE abap_false ).
*      TRY.
*          DATA(ref_to_keytab) = zcl_expimp_table=>create_keytab_for_get_keys(
*              tabname          = dd03l-tabname
*              with_user_header = abap_false ).
*        CATCH zcx_expimp_table.
*          ASSERT 0 = 1. " not expected because previously checked
*      ENDTRY.
*
*      FIELD-SYMBOLS: <keytab> TYPE STANDARD TABLE.
*      ASSIGN ref_to_keytab->* TO <keytab>.
*      TRY.
*          zcl_expimp_table=>get_keys(
*            EXPORTING
*              tabname          = dd03l-tabname
*              client           = client
*              area             = area
*              id               = id
*              generic_key      = abap_true
*              with_user_header = abap_false
*              client_specified = abap_true "client_specified
*            IMPORTING
*              keytab           = <keytab> ).
*        CATCH zcx_expimp_table
*              cx_sy_client
*              cx_sy_generic_key
*              cx_sy_tabline_too_short
*              cx_sy_incorrect_key
*              cx_sy_no_handler
*              INTO DATA(lx).
*          CONTINUE.
*      ENDTRY.
*
*      DATA ref_to_tabname TYPE REF TO data.
*      CREATE DATA ref_to_tabname TYPE (dd03l-tabname).
*      ASSIGN ref_to_tabname->* TO FIELD-SYMBOL(<line>).
*
*      LOOP AT <keytab> ASSIGNING FIELD-SYMBOL(<keyline>).
*
*        IF info-client_fieldname IS NOT INITIAL.
*          ASSIGN COMPONENT info-client_fieldname OF STRUCTURE <keyline> TO FIELD-SYMBOL(<client>).
*          DATA(l_client) = CONV mandt( <client> ).
*        ENDIF.
*
*        ASSIGN COMPONENT 'RELID' OF STRUCTURE <keyline> TO FIELD-SYMBOL(<relid>).
*        DATA(l_relid) = CONV indx_relid( <relid> ).
*
*        " ID fields
*        <line> = CORRESPONDING #( <keyline> ).
*
*        zcl_expimp_table=>import_as_xstring(
*          EXPORTING
*            client   = l_client
*            tabname  = dd03l-tabname
*            area     = l_relid
*            id_new   = <line>
*          IMPORTING
*            xstring  = DATA(xstring) ).
*
*        IF xstring+1(1) IN versions.
*          FIELD-SYMBOLS <chars> TYPE c.
*          ASSIGN <line>+info-id_offset(info-id_length) TO <chars> CASTING.
*          WRITE : / dd03l-tabname, l_client, l_relid, <chars>, xstring.
*        ENDIF.
*
*      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

TABLES sscrfields.

DATA gv_tabname TYPE tabname.
SELECT-OPTIONS tabnames FOR gv_tabname.
DATA g_version TYPE x LENGTH 1.
SELECT-OPTIONS versions FOR g_version. " between 01 and 06
PARAMETERS client TYPE symandt DEFAULT sy-mandt MODIF ID cli.
PARAMETERS area TYPE relid.
PARAMETERS id TYPE string LOWER CASE.

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.

START-OF-SELECTION.

  DATA(xml) = NEW lcl_app( )->main(
      client   = client
      tabnames = tabnames[]
      area     = area
      id       = id
      versions = versions[] ).

  cl_demo_output=>display_xml( xml ).
