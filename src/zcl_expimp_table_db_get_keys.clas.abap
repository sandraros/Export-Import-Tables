"! <p>Method RUN same as GET_KEYS of CL_ABAP_EXPIMP_UTILITIES, with few corrections,
"! because there are these possible bugs, found in 7.52:</p>
"! <ul>
"! <li>Bug 1: standard code assumes client field is always named MANDT, but it may differ
"!        and in that case CX_SY_DYNAMIC_OSQL_SYNTAX occurs (but not handled -> CX_SY_NO_HANDLER)</li>
"! <li>Bug 2: TODO check again, I feel that maybe I tested incorrectly...
"!        if WITH_USER_HEADER = 'X', standard code almost always triggers CX_SY_TABLINE_TOO_SHORT</li>
"! <li>Bug 3: duplicate keys are returned, why not single ones? Solution -> use SELECT DISTINCT
"!        instead of SELECT.</li>
"! <li>Bug 4: WITH_USER_HEADER = 'X' and export/import table has structure "one row", CLUSTD is
"!        required in KEYTAB; it should not.</li>
"! <li>Bug 5: doesn't work at all for one row export/import tables because algorithm is based
"!        on presence of SRTF2 column, which is only for multiple rows.</li>
"! <li>Bug 6: a generic read of all entries for client-dependent tables is not permitted
"!        on purpose, by design, why? Considered a bug, it should be possible.</li>
"! </ul>
"! <p>The code of this method was put outside class ZCL_EXPIMP_TABLE so that to keep
"! formatting of original GET_KEYS of CL_ABAP_EXPIMP_UTILITIES to ease future comparisons.</p>
CLASS zcl_expimp_table_db_get_keys DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS run
      IMPORTING
        !tabname          TYPE csequence
        !client           TYPE mandt OPTIONAL
        !area             TYPE relid OPTIONAL
        !id               TYPE clike OPTIONAL
        !generic_key      TYPE abap_bool DEFAULT abap_false
        !with_user_header TYPE abap_bool DEFAULT abap_false
        !client_specified TYPE abap_bool DEFAULT abap_false
        !info             TYPE zcl_expimp_table=>ty_expimp_table_info
      EXPORTING
        !keytab           TYPE STANDARD TABLE
      RAISING
        zcx_expimp_table
        cx_sy_client
        cx_sy_generic_key
        cx_sy_tabline_too_short
        cx_sy_incorrect_key .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_expimp_table_db_get_keys IMPLEMENTATION.

  METHOD run.

    CLASS cl_abap_char_utilities DEFINITION LOAD.

    TYPES: t_src(108) TYPE c,
           BEGIN OF id_field,
             name     TYPE t_src,
             position TYPE i,
           END OF id_field.

    DATA: spc TYPE string.               "string containing a blank
    SHIFT spc RIGHT BY 1 PLACES.         "character

    DATA: client_clean TYPE string.   " these strings contain the IMPORTed
    DATA: area_clean   TYPE string.   " parameters, with SQL-sensitive
    " characters escaped by #

* value of 'generic' that is used for the actual query (see below)
    DATA: generic_clean TYPE abap_bool.

* TRUE iff id is not supplied or initial
    DATA empty_id       TYPE abap_bool.

* id (if supplied)
    DATA string_id      TYPE string.

* id length (if supplied)
    DATA input_id_length TYPE id.

* TRUE iff area is not supplied or initial
    DATA empty_area     TYPE abap_bool.

* TRUE iff client is not supplied or initial
    DATA empty_client   TYPE abap_bool.


    DATA: line       TYPE t_src.
    DATA: where_tab  TYPE TABLE OF t_src. "dynamic SQL options
    DATA: field_tab  TYPE TABLE OF t_src.

    DATA: id_fields  TYPE STANDARD TABLE OF id_field. "list of id fields
    DATA wa_id_field TYPE id_field.

    DATA number_of_id_fields TYPE i.
*    DATA total_id_length     TYPE i.

    DATA id_flag     TYPE abap_bool.

    DATA number_of_in_fields TYPE i.

    DATA: f    TYPE STANDARD TABLE OF x031l,
          f_wa TYPE x031l.
    DATA: tabname2 TYPE tabname.      " table name (internal copy)

    DATA current_position TYPE i.

    DATA: type_ref   TYPE REF TO cl_abap_typedescr,
          tab_ref    TYPE REF TO cl_abap_tabledescr,
          line_ref   TYPE REF TO cl_abap_structdescr,
          fields_tab TYPE abap_compdescr_tab,
          wa_fields  TYPE abap_compdescr.

    DATA id_pointer TYPE i.
    DATA last       TYPE abap_bool.
    DATA str_a      TYPE string.
    DATA: number_of_fields TYPE i.
    DATA itemp1     TYPE i.
    DATA itemp2     TYPE i.
*
* initialization
*
    REFRESH keytab.
    CLEAR field_tab.
    CLEAR where_tab.


*
* check if ID is absent or empty.
*

    IF id IS SUPPLIED  AND NOT ( id IS INITIAL ).
      empty_id = abap_false.
      string_id = id.
      input_id_length = strlen( string_id ).
    ELSE.
      empty_id = abap_true.
    ENDIF.

*
* check if area is absent or empty
*

    IF area IS SUPPLIED  AND NOT ( area IS INITIAL ).
      empty_area = abap_false.
    ELSE.
      empty_area = abap_true.
    ENDIF.

*
* check if client is absent or empty
*

    IF client IS SUPPLIED  AND NOT ( client IS INITIAL ).
      empty_client = abap_false.
    ELSE.
      empty_client = abap_true.
    ENDIF.

*
* check correct key
*
* exception if:
* 1) no table is supplied
* 2) no area is supplied but id is supplied
*

    IF tabname IS INITIAL
       OR ( empty_area = abap_true AND empty_id = abap_false ).
      RAISE EXCEPTION TYPE cx_sy_incorrect_key.
    ENDIF.

*
* 3) client_specified is false but a client is supplied
*
    IF client_specified = abap_false
       AND empty_client = abap_false.
      RAISE EXCEPTION TYPE cx_sy_client.
    ENDIF.

*
* 4) client_specified is true, generic_key is true,
* but client, area, key are all missing
*
    IF client_specified = abap_true AND
       0 = 1 AND                                               " <=== BUG 6
       empty_client = abap_true AND empty_area = abap_true
       AND empty_id = abap_true AND generic_key = abap_true.
      RAISE EXCEPTION TYPE cx_sy_generic_key.
    ENDIF.

*
* fill the table of fields to be retrieved from the DB
* and the table of id_fields.
*

*
* a)initialization
*
    id_flag = abap_false.
    current_position = 1.   " in CHAR
    id_flag = abap_false.
    number_of_in_fields = 0.


*
* b) get key-table description
*
    CALL METHOD cl_abap_tabledescr=>describe_by_data
      EXPORTING
        p_data      = keytab
      RECEIVING
        p_descr_ref = type_ref.
    tab_ref ?= type_ref.
    CALL METHOD tab_ref->get_table_line_type
      RECEIVING
        p_descr_ref = type_ref.
    line_ref ?= type_ref.
    fields_tab = line_ref->components.

    LOOP AT fields_tab INTO wa_fields.
      number_of_in_fields = number_of_in_fields + 1.
    ENDLOOP.


*
* c) get id fields in the table 'tabname'
*
    number_of_id_fields = 0.
*  IMPORT NAMETAB h f ID tabname. "obsolete
    tabname2 = tabname.
    CALL FUNCTION 'DD_GET_NAMETAB'
      EXPORTING
        status    = 'A'
        tabname   = tabname2
        get_all   = 'X'
      TABLES
        x031l_tab = f
      EXCEPTIONS
        not_found = 0
        no_fields = 0
        OTHERS    = 0.

    IF with_user_header = abap_false.
      number_of_fields = 0.
      LOOP AT f INTO f_wa.
        number_of_fields = number_of_fields + 1.

        IF f_wa-fieldname = 'SRTF2'.
* this is the last field (and does not belong to the key)
          EXIT.
        ENDIF.
        IF lines( id_fields ) = lines( info-id_fields ). " <=== BUG 5
          EXIT.                                          " <=== BUG 5
        ENDIF.                                           " <=== BUG 5

        IF number_of_fields > number_of_in_fields.
*
* exception: the supplied keytable does not have the right format
*
          RAISE EXCEPTION TYPE cx_sy_tabline_wrong_format.
        ELSE.
          READ TABLE fields_tab INDEX number_of_fields
             INTO wa_fields.
          IF wa_fields-length < f_wa-exlength.
*
* exception: keytable field(s) is too short
*
            RAISE EXCEPTION TYPE cx_sy_tabline_too_short.
          ENDIF.
        ENDIF.


        line = f_wa-fieldname.
        APPEND line TO field_tab. " add field name to list of fields

        IF id_flag = abap_true.  " this is an id field
          wa_id_field-position = current_position.
          wa_id_field-name = f_wa-fieldname.
          current_position = current_position +
            f_wa-dblength2 / cl_abap_char_utilities=>charsize.
          number_of_id_fields = number_of_id_fields + 1.

          " add to table of id fields
          INSERT wa_id_field INTO id_fields INDEX 1.
        ENDIF.
        IF  f_wa-fieldname = 'RELID'.
          id_flag = abap_true.  " the id fields start with the next field
        ENDIF.
      ENDLOOP.
    ELSE. " WITH_USER_HEADER = TRUE.
      number_of_fields = 0. " <=== BUG 2
      LOOP AT f INTO f_wa.
*        IF f_wa-fieldname = 'CLUSTR'.  " last field " <=== BUG 4
        IF f_wa-fieldname = 'CLUSTR'  " last field " <=== BUG 4
        OR f_wa-fieldname = 'CLUSTD'. " last field for one row export/import tables " <=== BUG 4
          EXIT.
        ENDIF.

        IF f_wa-fieldname <> 'SRTF2'. " skip 'SRTF2'
          line = f_wa-fieldname.
          APPEND line TO field_tab.
          number_of_fields = number_of_fields + 1. " <=== BUG 2
        ELSE.
          id_flag = abap_false.       " no id fields after 'SRTF2'
        ENDIF.
        IF lines( id_fields ) = lines( info-id_fields ). " <=== BUG 5
          id_flag = abap_false.                          " <=== BUG 5
        ENDIF.                                           " <=== BUG 5

        IF id_flag = abap_true.
          wa_id_field-position = current_position.
          wa_id_field-name = f_wa-fieldname.
          current_position = current_position +
          f_wa-dblength2 / cl_abap_char_utilities=>charsize.
          number_of_id_fields = number_of_id_fields + 1.
          IF number_of_id_fields > number_of_in_fields.
*
* exc'n: the supplied keytable does not have the right format
*
            RAISE EXCEPTION TYPE cx_sy_tabline_wrong_format.
          ELSE.
*            READ TABLE fields_tab INDEX number_of_id_fields " <=== BUG 2
            READ TABLE fields_tab INDEX number_of_fields " <=== BUG 2
               INTO wa_fields.
            IF wa_fields-length < f_wa-exlength.
*
* exception: keytable field(s) is too short
*
              RAISE EXCEPTION TYPE cx_sy_tabline_too_short.
            ENDIF.
          ENDIF.

          INSERT wa_id_field INTO id_fields INDEX 1.
        ENDIF.
        IF  f_wa-fieldname = 'RELID'.
          id_flag = abap_true.  " the id fields start with the next field
        ENDIF.
      ENDLOOP.
    ENDIF.

*    total_id_length = current_position - 1. "total length of the id fields

*
* check generic key
*

*
* generic is set to false (regardless of user input)  if either
* 1) no id is provided and the given area contains two characters,
* 2) neither id nor area are provided and the given client
*    contains 3 characters,
* 3) only table name is provided, or
* 4) the total length of the provided ids exactly matches
*    the length of the id fields in the DB table
* otherwise, the user input value of generic_key is used
*
    IF ( empty_id = abap_true AND area+1(1) <> '' )
          OR ( empty_id = abap_true
               AND empty_area = abap_true
               AND client+2(1) <> '' )
          OR ( empty_id = abap_true
               AND 0 = 1                            " <=== BUG 6
               AND empty_area = abap_true
               AND empty_client = abap_true )
          OR info-id_length = input_id_length.
      generic_clean = abap_false.
    ELSE.
      generic_clean = generic_key.
    ENDIF.


*
* fill the where condition
*

*
* id fields
*

    id_pointer = input_id_length.
    last = abap_true.

*
*  loop over the id fields of the DB table
*
    LOOP AT id_fields INTO wa_id_field.

      IF wa_id_field-position > input_id_length.
        CONTINUE. "no value for this field has been selected
      ENDIF.

      itemp1 = wa_id_field-position - 1.
      itemp2 = id_pointer - wa_id_field-position + 1.
      str_a = id+itemp1(itemp2).

      IF last = abap_true AND generic_clean <> abap_false.
*
* if this is the last field and generic is true,
* we must formulate the where condition as
* a 'LIKE'-relation. This in turn means that
* special characters must be escaped.
*
        REPLACE ALL OCCURRENCES OF '#' IN str_a WITH '##'.
        REPLACE ALL OCCURRENCES OF '%' IN str_a WITH '#%'.
        REPLACE ALL OCCURRENCES OF '_' IN str_a WITH '#_'.
        CONCATENATE wa_id_field-name spc 'LIKE' spc ''''
        str_a '%' '''' spc 'ESCAPE ''#''' spc 'AND' INTO str_a.
        last = abap_false.
      ELSE.
*
* in this case we can simply use a '=' condition.
* No escape is necessary
*
        CONCATENATE wa_id_field-name spc '=' spc '''' str_a ''''
        spc 'AND' INTO str_a.
      ENDIF.

      APPEND str_a TO where_tab.

      id_pointer = itemp1.
    ENDLOOP.

*
* area field
*

    IF empty_area = abap_false.
      area_clean = area.
      TRANSLATE area_clean TO UPPER CASE.

*
* (same logic as above)
*
      IF empty_id = abap_true
        AND generic_clean <> abap_false.
        REPLACE ALL OCCURRENCES OF '#' IN area_clean WITH '##'.
        REPLACE ALL OCCURRENCES OF '%' IN area_clean WITH '#%'.
        REPLACE ALL OCCURRENCES OF '_' IN area_clean WITH '#_'.
        CONCATENATE 'RELID LIKE' spc '''' area_clean '%' ''''
        spc 'ESCAPE ''#''' INTO line.
      ELSE.
        CONCATENATE 'RELID =' spc '''' area_clean '''' INTO line.
      ENDIF.
      APPEND line TO where_tab.
    ENDIF.

*
* client field
*

    IF client_specified <> abap_false
       AND ( empty_client = abap_false OR empty_area = abap_false ).
      IF empty_area = abap_false.
        line = 'AND'.
        APPEND line TO where_tab.
      ENDIF.
      DATA(client_fieldname) = f[ 1 ]-fieldname. " <=== BUG 1
      IF generic_clean <> abap_false.
        client_clean = client.
        REPLACE ALL OCCURRENCES OF '#' IN client_clean WITH '##'.
        REPLACE ALL OCCURRENCES OF '%' IN client_clean WITH '#%'.
        REPLACE ALL OCCURRENCES OF '_' IN client_clean WITH '#_'.
*        CONCATENATE 'MANDT LIKE' spc '''' client_clean '%' '''' " <=== BUG 1
        CONCATENATE client_fieldname ' LIKE' spc '''' client_clean '%' '''' " <=== BUG 1
        'ESCAPE ''#'''  INTO line.
      ELSE.
*        CONCATENATE 'MANDT =' spc '''' client '''' INTO line. " <=== BUG 1
        CONCATENATE client_fieldname ' =' spc '''' client '''' INTO line. " <=== BUG 1
      ENDIF.
      APPEND line TO where_tab.
    ENDIF.

*
* DB query
*
    IF client_specified <> abap_false.
*      SELECT (field_tab) FROM (tabname) CLIENT SPECIFIED " <=== BUG 3
      SELECT DISTINCT (field_tab) FROM (tabname) CLIENT SPECIFIED " <=== BUG 3
      INTO TABLE keytab
      WHERE (where_tab).
    ELSE.
*      SELECT (field_tab) FROM (tabname) " <=== BUG 3
      SELECT DISTINCT (field_tab) FROM (tabname) " <=== BUG 3
      INTO TABLE keytab
      WHERE (where_tab).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
