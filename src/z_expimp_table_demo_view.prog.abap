*&---------------------------------------------------------------------*
*& Report z_expimp_table_view
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_expimp_table_demo_view.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.

    METHODS f4_key
      CHANGING
        alv_table     TYPE STANDARD TABLE
      RETURNING
        VALUE(result) TYPE REF TO data.

    METHODS on_double_click FOR EVENT double_click OF if_salv_events_actions_table IMPORTING row.

    METHODS read_fields_from_screen.

    METHODS write_fields_to_screen.

    METHODS start_of_selection.

    METHODS at_selection_screen_output.

    METHODS at_selection_screen
      IMPORTING
        ucomm TYPE sscrfields-ucomm.

    METHODS constructor.

    DATA: lt_sel TYPE TABLE OF rsparamsl_255,
          BEGIN OF program_parameters,
            client   TYPE mandt,
            tabname  TYPE tabname,
            area     TYPE relid,
            id       TYPE string,
            show_hex TYPE abap_bool,
            upper    TYPE abap_bool,
            id1_val  TYPE c LENGTH 255,
            id1_len  TYPE i,
            id2_val  TYPE c LENGTH 255,
            id2_len  TYPE i,
            id3_val  TYPE c LENGTH 255,
            id3_len  TYPE i,
            id4_val  TYPE c LENGTH 255,
            id4_len  TYPE i,
            id5_val  TYPE c LENGTH 255,
            id5_len  TYPE i,
            id6_val  TYPE c LENGTH 255,
            id6_len  TYPE i,
            id7_val  TYPE c LENGTH 255,
            id7_len  TYPE i,
            id8_val  TYPE c LENGTH 255,
            id8_len  TYPE i,
            id9_val  TYPE c LENGTH 255,
            id9_len  TYPE i,
            id10_val TYPE c LENGTH 255,
            id10_len TYPE i,
            id11_val TYPE c LENGTH 255,
            id11_len TYPE i,
            id12_val TYPE c LENGTH 255,
            id12_len TYPE i,
            id13_val TYPE c LENGTH 255,
            id13_len TYPE i,
            id14_val TYPE c LENGTH 255,
            id14_len TYPE i,
          END OF program_parameters.

    DATA salv TYPE REF TO cl_salv_table.
    DATA ref_alv_table TYPE REF TO data.
    DATA ref_alv_line TYPE REF TO data.

  PRIVATE SECTION.

    METHODS get_xml
      RETURNING
        VALUE(xml) TYPE string.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD constructor.
    DATA lt_dummy   TYPE TABLE OF rsparams.
    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report         = sy-repid
      TABLES
        selection_table     = lt_dummy
        selection_table_255 = lt_sel
      EXCEPTIONS
        not_found           = 1
        no_report           = 2
        OTHERS              = 3.
  ENDMETHOD.


  METHOD read_fields_from_screen.

    FIELD-SYMBOLS: <select_options> TYPE STANDARD TABLE,
                   <range_table>    TYPE STANDARD TABLE.

    DATA(screen_field_name) = ``.
    LOOP AT lt_sel ASSIGNING FIELD-SYMBOL(<parameter_values>) GROUP BY ( selname = <parameter_values>-selname kind = <parameter_values>-kind ) ASSIGNING FIELD-SYMBOL(<parameter>).
      IF <parameter>-kind = 'P'.
        screen_field_name = |({ sy-repid }){ <parameter>-selname }|." CONV char255( )
        ASSIGN (screen_field_name) TO FIELD-SYMBOL(<screen_field>).
        CHECK sy-subrc = 0.
        ASSIGN COMPONENT <parameter>-selname OF STRUCTURE program_parameters TO FIELD-SYMBOL(<attribute_field>).
        CHECK sy-subrc = 0.
        <attribute_field> = <screen_field>.
      ELSE.
        screen_field_name = |({ sy-repid }){ <parameter>-selname }[]|.
        ASSIGN (screen_field_name) TO <select_options>.
        CHECK sy-subrc = 0.
        ASSIGN COMPONENT <parameter>-selname OF STRUCTURE program_parameters TO <range_table>.
        CHECK sy-subrc = 0.
        LOOP AT <select_options> ASSIGNING FIELD-SYMBOL(<select_options_line>).
          APPEND INITIAL LINE TO <range_table> ASSIGNING FIELD-SYMBOL(<range_line>).
          LOOP AT VALUE string_table( ( |SIGN| ) ( |OPTION| ) ( |LOW| ) ( |HIGH| ) ) ASSIGNING FIELD-SYMBOL(<component_name>).
            ASSIGN COMPONENT <component_name> OF STRUCTURE <select_options_line> TO FIELD-SYMBOL(<select_options_line_comp>).
            CHECK sy-subrc = 0.
            ASSIGN COMPONENT <component_name> OF STRUCTURE <range_line> TO FIELD-SYMBOL(<range_line_comp>).
            CHECK sy-subrc = 0.
            <range_line_comp> = <select_options_line_comp>.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD at_selection_screen.

    FIELD-SYMBOLS: <keytab> TYPE STANDARD TABLE.

    CASE sy-dynnr.
      WHEN 1000.
        CASE ucomm.
          WHEN 'GET_KEYS'.
            TRY.
                DATA(info) = zcl_expimp_table=>get_info( program_parameters-tabname ).
                DATA(client_specified) = xsdbool( info-client_fieldname IS NOT INITIAL ).
                DATA(ref_to_keytab) = zcl_expimp_table=>create_keytab_for_get_keys( tabname = program_parameters-tabname ).
              CATCH zcx_expimp_table.
                MESSAGE 'not a valid Export/Import Table'(001) TYPE 'E'.
            ENDTRY.
            ASSIGN ref_to_keytab->* TO <keytab>.
            TRY.
                zcl_expimp_table=>get_keys(
                  EXPORTING
                    tabname          = program_parameters-tabname
                    client           = COND #( WHEN abap_true = client_specified THEN program_parameters-client )
                    area             = program_parameters-area
                    id               = program_parameters-id
                    generic_key      = abap_true
                    client_specified = client_specified
                  IMPORTING
                    keytab           = <keytab> ).
              CATCH zcx_expimp_table
                    cx_sy_client
                    cx_sy_generic_key
                    cx_sy_tabline_too_short
                    cx_sy_incorrect_key
                    cx_sy_no_handler
                    INTO DATA(lx).
                IF lx->previous IS BOUND."INSTANCE OF cx_sy_no_handler.
                  MESSAGE lx->previous TYPE 'E'.
                ELSE.
                  MESSAGE lx TYPE 'E'.
                ENDIF.
            ENDTRY.
            DATA(ref_alv_line) = f4_key( CHANGING alv_table = <keytab> ).
            IF ref_alv_line IS BOUND.
              ASSIGN ref_alv_line->* TO FIELD-SYMBOL(<alv_line>).
              DATA(offset) = 0.
              IF info-client_fieldname IS NOT INITIAL.
                ASSIGN COMPONENT 1 OF STRUCTURE <alv_line> TO FIELD-SYMBOL(<alv_field>).
                program_parameters-client = <alv_field>.
                ASSIGN COMPONENT 2 OF STRUCTURE <alv_line> TO <alv_field>.
                program_parameters-area = <alv_field>.
                offset = 2.
              ELSE.
                ASSIGN COMPONENT 1 OF STRUCTURE <alv_line> TO <alv_field>.
                program_parameters-area = <alv_field>.
                offset = 1.
              ENDIF.
              DATA(id_string) = ||.
              LOOP AT info-id_fields REFERENCE INTO DATA(id_field).
                ASSIGN COMPONENT sy-tabix + offset OF STRUCTURE <alv_line> TO <alv_field>.
                id_string = id_string && |{ <alv_field> WIDTH = id_field->length }|.
                DATA(fieldname) = |ID{ sy-tabix }_VAL|.
                ASSIGN (fieldname) TO FIELD-SYMBOL(<field>).
                <field> = <alv_field>.
              ENDLOOP.
              program_parameters-id = id_string.
            ENDIF.
          WHEN 'ENTER_ID'.
            IF NOT zcl_expimp_table=>is_valid_expimp_table( program_parameters-tabname ).
              MESSAGE 'not a valid Export/Import Table'(001) TYPE 'E'.
            ELSE.
              CALL SELECTION-SCREEN 1001 STARTING AT 40 10 ENDING AT 130 30.
              program_parameters-id = |{ program_parameters-id1_val  WIDTH = program_parameters-id1_len
                                      }{ program_parameters-id2_val  WIDTH = program_parameters-id2_len
                                      }{ program_parameters-id3_val  WIDTH = program_parameters-id3_len
                                      }{ program_parameters-id4_val  WIDTH = program_parameters-id4_len
                                      }{ program_parameters-id5_val  WIDTH = program_parameters-id5_len
                                      }{ program_parameters-id6_val  WIDTH = program_parameters-id6_len
                                      }{ program_parameters-id7_val  WIDTH = program_parameters-id7_len
                                      }{ program_parameters-id8_val  WIDTH = program_parameters-id8_len
                                      }{ program_parameters-id9_val  WIDTH = program_parameters-id9_len
                                      }{ program_parameters-id10_val WIDTH = program_parameters-id10_len
                                      }{ program_parameters-id11_val WIDTH = program_parameters-id11_len
                                      }{ program_parameters-id12_val WIDTH = program_parameters-id12_len
                                      }{ program_parameters-id13_val WIDTH = program_parameters-id13_len
                                      }{ program_parameters-id14_val WIDTH = program_parameters-id14_len }|.
            ENDIF.
        ENDCASE.
      WHEN 1001.
        IF program_parameters-upper = 'X'.
          TRANSLATE program_parameters-id1_val TO UPPER CASE.
          TRANSLATE program_parameters-id2_val TO UPPER CASE.
          TRANSLATE program_parameters-id3_val TO UPPER CASE.
          TRANSLATE program_parameters-id4_val TO UPPER CASE.
          TRANSLATE program_parameters-id5_val TO UPPER CASE.
          TRANSLATE program_parameters-id6_val TO UPPER CASE.
          TRANSLATE program_parameters-id7_val TO UPPER CASE.
          TRANSLATE program_parameters-id8_val TO UPPER CASE.
          TRANSLATE program_parameters-id9_val TO UPPER CASE.
          TRANSLATE program_parameters-id10_val TO UPPER CASE.
          TRANSLATE program_parameters-id11_val TO UPPER CASE.
          TRANSLATE program_parameters-id12_val TO UPPER CASE.
          TRANSLATE program_parameters-id13_val TO UPPER CASE.
          TRANSLATE program_parameters-id14_val TO UPPER CASE.
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD start_of_selection.

    DATA(xml) = get_xml( ).
    cl_demo_output=>display_xml( xml ).

  ENDMETHOD.

  METHOD get_xml.

    DATA: ref_wa           TYPE REF TO data,
          xml_key_fields   TYPE string,
          xml_attr_fields  TYPE string,
          xml_data_objects TYPE string,
          generic          TYPE abap_trans_srcbind_tab.

    DATA(info) = zcl_expimp_table=>get_info( program_parameters-tabname ).

    xml = |<?xml version="1.0"?><exportImportData>|.


    generic = VALUE #( ).
    IF info-client_fieldname IS NOT INITIAL.
      generic = VALUE #(
          BASE generic
          ( name  = info-client_fieldname
            value = REF #( program_parameters-client ) ) ).
    ENDIF.
    generic = VALUE #(
        BASE generic
        FOR <id_field> IN info-id_fields
        LET beyond         = boolc( strlen( program_parameters-id ) <= <id_field>-offset )
            offset         = COND #( WHEN beyond = abap_true THEN 0 ELSE <id_field>-offset )
            length         = COND #( WHEN beyond = abap_true THEN 0
                                ELSE nmin( val1 = strlen( program_parameters-id ) - offset val2 = <id_field>-length ) )
            "id_field_value = NEW string( id+offset(length) )
            IN
        ( name  = <id_field>-fieldname
          value = COND #( WHEN length <> 0 THEN NEW string( program_parameters-id+offset(length) ) ELSE NEW string( ) ) ) ).
    CALL TRANSFORMATION id
      SOURCE
        (generic)
      RESULT
        XML xml_key_fields
      OPTIONS
        xml_header = 'no'.
    xml = xml && |<keyFields>{ xml_key_fields }</keyFields>|.

    CREATE DATA ref_wa TYPE (program_parameters-tabname).
    ASSIGN ref_wa->* TO FIELD-SYMBOL(<wa>).

    IF program_parameters-show_hex = abap_true.

      zcl_expimp_table=>import_as_xstring(
        EXPORTING
          client   = program_parameters-client
          tabname  = program_parameters-tabname
          area     = program_parameters-area
          id       = program_parameters-id
        IMPORTING
          xstring  = DATA(xstring)
          wa       = <wa> ).

      xml = xml && |<dataObjects>{ xml_data_objects }</dataObjects>|.

    ELSE.

      zcl_expimp_table=>import_all(
        EXPORTING
          client   = program_parameters-client
          tabname  = program_parameters-tabname
          area     = program_parameters-area
          id       = program_parameters-id
        IMPORTING
          wa       = <wa>
          tab_cpar = DATA(tab_cpar) ).

    ENDIF.

    IF info-attr_fieldnames IS NOT INITIAL.
      generic = VALUE #( ).
      LOOP AT info-attr_fieldnames INTO DATA(fieldname).
        ASSIGN COMPONENT fieldname OF STRUCTURE <wa> TO FIELD-SYMBOL(<attr_field>).
        generic = VALUE #(
            BASE generic
            ( name  = fieldname
              value = REF #( <attr_field> ) ) ).
      ENDLOOP.
      CALL TRANSFORMATION id
        SOURCE
          (generic)
        RESULT
          XML xml_attr_fields
        OPTIONS
          xml_header = 'no'.
      xml = xml && |<attributes>{ xml_attr_fields }</attributes>|.

    ENDIF.

    IF program_parameters-show_hex = abap_true.

      xml = xml && |<dataCluster>{ xstring }</dataCluster>|.

    ELSE.

      generic = VALUE abap_trans_srcbind_tab(
          FOR <cpar> IN tab_cpar
          ( name  = <cpar>-name
            value = <cpar>-dref ) ).
      CALL TRANSFORMATION id
        SOURCE
          (generic)
        RESULT
          XML xml_data_objects
        OPTIONS
          xml_header = 'no'.

      xml = xml && |<dataObjects>{ xml_data_objects }</dataObjects>|.

    ENDIF.

    xml = xml && |</exportImportData>|.

  ENDMETHOD.

  METHOD f4_key.

    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = salv
      CHANGING
        t_table      = alv_table ).

    SET HANDLER on_double_click FOR salv->get_event( ).

    ref_alv_table = REF #( alv_table ).

    ref_alv_line = VALUE #( ).

    salv->get_functions( )->set_all( ).
    salv->display( ).

    SET HANDLER on_double_click FOR salv->get_event( ) ACTIVATION ''.

    result = ref_alv_line.

  ENDMETHOD.

  METHOD on_double_click.

    FIELD-SYMBOLS <alv_table> TYPE STANDARD TABLE.

    ASSIGN ref_alv_table->* TO <alv_table>.

    ref_alv_line = REF #( <alv_table>[ row ] ).

    salv->close_screen( ).

  ENDMETHOD.

  METHOD at_selection_screen_output.

    DATA: fieldname_range TYPE RANGE OF fieldname.

    TRY.
        DATA(info) = zcl_expimp_table=>get_info( program_parameters-tabname ).
      CATCH zcx_expimp_table.
    ENDTRY.

    CASE sy-dynnr.

      WHEN 1000.
        LOOP AT SCREEN.
          IF info-client_fieldname IS INITIAL AND screen-group1 = 'CLI'.
            screen-active = '0'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.

      WHEN 1001.
        fieldname_range = VALUE #(
            FOR <id_field> IN info-id_fields
            ( sign = 'I' option = 'EQ' low = <id_field>-fieldname ) ).
        DATA(rtti) = cl_abap_typedescr=>describe_by_name( program_parameters-tabname ).
        DATA(field_list) = CAST cl_abap_structdescr( rtti )->get_ddic_field_list( ).
        DELETE field_list WHERE fieldname NOT IN fieldname_range.
        LOOP AT field_list REFERENCE INTO DATA(field).
          DATA(fieldname) = |ID{ sy-tabix }_NAM|.
          ASSIGN COMPONENT fieldname OF STRUCTURE program_parameters TO FIELD-SYMBOL(<field>).
          IF sy-subrc = 0.
            <field> = field->scrtext_m.
          ENDIF.
          fieldname = |ID{ sy-tabix }_LEN|.
          ASSIGN COMPONENT fieldname OF STRUCTURE program_parameters TO <field>.
          IF sy-subrc = 0.
            <field> = field->leng.
          ENDIF.
        ENDLOOP.
        LOOP AT SCREEN.
          FIND FIRST OCCURRENCE OF REGEX 'ID(\d+)_VAL' IN screen-name SUBMATCHES DATA(number).
          IF sy-subrc = 0.
            READ TABLE field_list INDEX number REFERENCE INTO field.
            IF sy-subrc <> 0.
              screen-active = '0'.
            ELSE.
              screen-length = field->leng.
            ENDIF.
            MODIFY SCREEN.
          ELSE.
            FIND FIRST OCCURRENCE OF REGEX 'ID(\d+)_NAM' IN screen-name SUBMATCHES number.
            IF sy-subrc = 0.
              READ TABLE field_list INDEX number REFERENCE INTO field.
              IF sy-subrc <> 0.
                screen-active = '0'.
                MODIFY SCREEN.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
    ENDCASE.

  ENDMETHOD.

  METHOD write_fields_to_screen.

    FIELD-SYMBOLS: <select_options> TYPE STANDARD TABLE,
                   <range_table>    TYPE STANDARD TABLE.

    DATA(screen_field_name) = ``.
    LOOP AT lt_sel ASSIGNING FIELD-SYMBOL(<parameter_values>) GROUP BY ( selname = <parameter_values>-selname kind = <parameter_values>-kind ) ASSIGNING FIELD-SYMBOL(<parameter>).
      IF <parameter>-kind = 'P'.
        "=============
        " PARAMETERS
        "=============
        screen_field_name = |({ sy-repid }){ <parameter>-selname }|.
        ASSIGN (screen_field_name) TO FIELD-SYMBOL(<screen_field>).
        CHECK sy-subrc = 0.
        ASSIGN COMPONENT <parameter>-selname OF STRUCTURE program_parameters TO FIELD-SYMBOL(<attribute_field>).
        CHECK sy-subrc = 0.
        <screen_field> = <attribute_field>.
      ELSE.
        "=============
        " SELECT-OPTIONS
        "=============
        screen_field_name = |({ sy-repid }){ <parameter>-selname }[]|.
        ASSIGN (screen_field_name) TO <select_options>.
        CHECK sy-subrc = 0.
        ASSIGN COMPONENT <parameter>-selname OF STRUCTURE program_parameters TO <range_table>.
        CHECK sy-subrc = 0.
        LOOP AT <select_options> ASSIGNING FIELD-SYMBOL(<select_options_line>).
          APPEND INITIAL LINE TO <range_table> ASSIGNING FIELD-SYMBOL(<range_line>).
          LOOP AT VALUE string_table( ( |SIGN| ) ( |OPTION| ) ( |LOW| ) ( |HIGH| ) ) ASSIGNING FIELD-SYMBOL(<component_name>).
            ASSIGN COMPONENT <component_name> OF STRUCTURE <select_options_line> TO FIELD-SYMBOL(<select_options_line_comp>).
            CHECK sy-subrc = 0.
            ASSIGN COMPONENT <component_name> OF STRUCTURE <range_line> TO FIELD-SYMBOL(<range_line_comp>).
            CHECK sy-subrc = 0.
            <select_options_line_comp> = <range_line_comp>.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

TABLES sscrfields.

PARAMETERS show_hex AS CHECKBOX DEFAULT abap_false.
PARAMETERS tabname TYPE tabname.
SELECTION-SCREEN PUSHBUTTON /1(30) get_keys USER-COMMAND get_keys.
PARAMETERS client TYPE symandt DEFAULT sy-mandt MODIF ID cli.
PARAMETERS area TYPE relid.
SELECTION-SCREEN PUSHBUTTON /1(30) enter_id USER-COMMAND enter_id.
" Choosing type of ID field, either C255 or STRING, has an important impact
" on DB_GET_KEYS which interprets ID as a generic key only if there are
" no trailing spaces. I prefer to use type C so that generic key is
" preferred (that automatically works as if there were never trailing spaces)
PARAMETERS id TYPE c LENGTH 255.

SELECTION-SCREEN BEGIN OF SCREEN 1001 AS WINDOW.
PARAMETERS upper AS CHECKBOX DEFAULT abap_true.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (40) id1_nam.
PARAMETERS id1_val TYPE char255 LOWER CASE.
SELECTION-SCREEN END OF LINE.
PARAMETERS id1_len TYPE i NO-DISPLAY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (40) id2_nam.
PARAMETERS id2_val TYPE char255 LOWER CASE.
SELECTION-SCREEN END OF LINE.
PARAMETERS id2_len TYPE i NO-DISPLAY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (40) id3_nam.
PARAMETERS id3_val TYPE char255 LOWER CASE.
SELECTION-SCREEN END OF LINE.
PARAMETERS id3_len TYPE i NO-DISPLAY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (40) id4_nam.
PARAMETERS id4_val TYPE char255 LOWER CASE.
SELECTION-SCREEN END OF LINE.
PARAMETERS id4_len TYPE i NO-DISPLAY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (40) id5_nam.
PARAMETERS id5_val TYPE char255 LOWER CASE.
SELECTION-SCREEN END OF LINE.
PARAMETERS id5_len TYPE i NO-DISPLAY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (40) id6_nam.
PARAMETERS id6_val TYPE char255 LOWER CASE.
SELECTION-SCREEN END OF LINE.
PARAMETERS id6_len TYPE i NO-DISPLAY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (40) id7_nam.
PARAMETERS id7_val TYPE char255 LOWER CASE.
SELECTION-SCREEN END OF LINE.
PARAMETERS id7_len TYPE i NO-DISPLAY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (40) id8_nam.
PARAMETERS id8_val TYPE char255 LOWER CASE.
SELECTION-SCREEN END OF LINE.
PARAMETERS id8_len TYPE i NO-DISPLAY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (40) id9_nam.
PARAMETERS id9_val TYPE char255 LOWER CASE.
SELECTION-SCREEN END OF LINE.
PARAMETERS id9_len TYPE i NO-DISPLAY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (40) id10_nam.
PARAMETERS id10_val TYPE char255 LOWER CASE.
SELECTION-SCREEN END OF LINE.
PARAMETERS id10_len TYPE i NO-DISPLAY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (40) id11_nam.
PARAMETERS id11_val TYPE char255 LOWER CASE.
SELECTION-SCREEN END OF LINE.
PARAMETERS id11_len TYPE i NO-DISPLAY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (40) id12_nam.
PARAMETERS id12_val TYPE char255 LOWER CASE.
SELECTION-SCREEN END OF LINE.
PARAMETERS id12_len TYPE i NO-DISPLAY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (40) id13_nam.
PARAMETERS id13_val TYPE char255 LOWER CASE.
SELECTION-SCREEN END OF LINE.
PARAMETERS id13_len TYPE i NO-DISPLAY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (40) id14_nam.
PARAMETERS id14_val TYPE char255 LOWER CASE.
SELECTION-SCREEN END OF LINE.
PARAMETERS id14_len TYPE i NO-DISPLAY.

SELECTION-SCREEN END OF SCREEN 1001.

INITIALIZATION.
  DATA(app) = NEW lcl_app( ).
  enter_id = 'Enter ID fields'(001).
  get_keys = 'F4 existing keys'(002).

AT SELECTION-SCREEN OUTPUT.
  app->read_fields_from_screen( ).
  TRY.
      app->at_selection_screen_output( ).
    CATCH cx_root INTO DATA(lx_root).
      MESSAGE lx_root TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
  app->write_fields_to_screen( ).

AT SELECTION-SCREEN.
  app->read_fields_from_screen( ).
  TRY.
      app->at_selection_screen( sscrfields-ucomm ).
    CATCH cx_root INTO DATA(lx_root).
      app->write_fields_to_screen( ).
      MESSAGE lx_root TYPE 'E'.
  ENDTRY.
  app->write_fields_to_screen( ).

START-OF-SELECTION.
  TRY.
      app->start_of_selection( ).
    CATCH cx_root INTO DATA(lx_root).
      MESSAGE lx_root TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
