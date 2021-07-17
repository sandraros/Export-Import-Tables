"! <p class="shorttext synchronized" lang="en">Utility for export/import tables</p>
"! See demonstration in program Z_EXPIMP_TABLE_DEMO.
"! NB:<ul>
"! <li>Dynamic deletion may be achieved using CL_ABAP_EXPIMP_UTILITIES=>DB_DELETE</li>
"! </ul>
CLASS zcl_expimp_table DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_id_field,
             fieldname TYPE fieldname,
             offset    TYPE i,
             length    TYPE i,
           END OF ty_id_field,
           BEGIN OF ty_expimp_table_info,
             tabname              TYPE tabname,
             "! <ul>
             "! <li>false: table with multiple rows (has got columns SRTF2 and CLUSTR, CLUSTD is of type RAW/X)</li>
             "! <li>true: table with one row (hasN'T got columns SRTF2 and CLUSTR, CLUSTD is of type RAWSTRING/XSTRING)</li>
             "! </ul>
             is_structure_one_row TYPE abap_bool,
             "! Name of client column - Empty if no client column
             client_fieldname     TYPE fieldname,
             id_fields            TYPE STANDARD TABLE OF ty_id_field WITH EMPTY KEY,
             "! Offset of RELID column, in number of characters (0 or 3, after eventual client column)
             area_offset          TYPE i,
             "! Offset of first ID field in number of characters (2 or 5, after RELID column)
             id_offset            TYPE i,
             "! Total length of ID fields in number of characters
             id_length            TYPE i,
             total_key_length     TYPE i,
             attr_fieldnames      TYPE STANDARD TABLE OF fieldname WITH EMPTY KEY,
             "! Byte offset of CLUSTR column (only if structure is multiple rows, zero otherwise)
             offset_clustr        TYPE i,
             "! Number of bytes of SRTF2 column, 1, 2 or 4 (only if structure is multiple rows, zero otherwise)
             srtf2_length         TYPE i,
             offset_clustd        TYPE i,
             clustd_length        TYPE i,
           END OF ty_expimp_table_info.

    "! The advantage of the method IMPORT_ALL compared to the classic IMPORT ... FROM DATABASE ..., is that you may:<br/>
    "! <ul>
    "! <li>indicate the table name and the area dynamically,</li>
    "! <li>read all the data objects of the data cluster, no need of
    "! indicating the types and names of data objects to read,</li>
    "! <li>use the parameter ID_NEW (to be of "table_name" type) which is easier to use than ID (to comprise only the ID key fields).</li>
    "! </ul>
    "! Example:<br/>
    "! DATA(nummer) = CONV eufunc-nummer( 1 ).<br/>
    "! DATA(id_wa) = VALUE eufunc( gruppe = 'SCAL' name = 'DATE_GET_WEEK' nummer = nummer ).<br/>
    "! zcl_expimp_table=>import_all(<br/>
    "! &nbsp;&nbsp;&nbsp;EXPORTING table_name = 'EUFUNC' area = 'FL'<br/>
    "! &nbsp;&nbsp;&nbsp;IMPORTING tab_cpar = DATA(tab_cpar)<br/>
    "! &nbsp;&nbsp;&nbsp;CHANGING  id_wa = id_wa.<br/>
    "! DATA(dref) = tab_cpar[ name = '%_IDATE' ]-dref.<br/>
    "! DATA(date1) = CAST d( dref )->*. " Here the developer knows that %_IDATE is of type D.<br/>
    "! <br/>
    "! is equivalent to:<br/>
    "! <br/>
    "! DATA(nummer) = CONV eufunc-nummer( 1 ).<br/>
    "! DATA(id) = VALUE functdir( area = 'SCAL' progid = 'DATE_GET_WEEK' dataid = nummer ).<br/>
    "! <br/>
    "! IMPORT %_idate = date1 FROM DATABASE eufunc(fl) ID id TO wa.<br/>
    "! <br/>
    "! Conversion options (ACCEPTING, IGNORING, etc.) are currently not possible.
    "! <p class="shorttext synchronized" lang="en">Read all data objects of a data cluster</p>
    "!
    "! @parameter client | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter TABNAME | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter area | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter tab_cpar | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ID | <p class="shorttext synchronized" lang="en"></p>
    "!  | Key of the data cluster in the database table, which must be the concatenation of
    "!  | all key fields after the column RELID (except SRTF2).
    "! @parameter id_new | <p class="shorttext synchronized" lang="en"></p>
    "!  | Instead of using ID, you may find easier to use ID_NEW which may be a
    "!  | structure with columns of same name as key fields of the export/import table,
    "!  | for instance it may be a structure defined like the export/import table.
    "! @parameter key | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter WA | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_expimp_table | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS import_all
      IMPORTING
        client   TYPE mandt DEFAULT sy-mandt
        tabname  TYPE tabname
        area     TYPE relid OPTIONAL
        id       TYPE clike OPTIONAL
        id_new   TYPE any OPTIONAL
        key      TYPE any OPTIONAL
      EXPORTING
        wa       TYPE any
        tab_cpar TYPE tab_cpar
      RAISING
        zcx_expimp_table.

    "! <p class="shorttext synchronized" lang="en">Write all data objects (replace all)</p>
    "!
    "! @parameter client | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter TABNAME | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter area | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter id | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter id_new | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter key | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter WA | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter tab_cpar | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter compression | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_expimp_table | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS export_all
      IMPORTING
        client      TYPE mandt DEFAULT sy-mandt
        tabname     TYPE tabname
        area        TYPE relid OPTIONAL
        id          TYPE clike OPTIONAL
        id_new      TYPE any OPTIONAL
        key         TYPE any OPTIONAL
        wa          TYPE any
        tab_cpar    TYPE tab_cpar
        compression TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_expimp_table.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter TABNAME | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS is_valid_expimp_table
      IMPORTING
        tabname       TYPE tabname
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter TABNAME | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter info | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_expimp_table | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_info
      IMPORTING
        tabname     TYPE tabname
      RETURNING
        VALUE(info) TYPE ty_expimp_table_info
      RAISING
        zcx_expimp_table.

    "! This method creates a data object which is compatible with parameter KEYTAB of
    "! methods *_GET_KEYS of CL_ABAP_EXPIMP_UTILITIES
    "! <ul>
    "! <li>Optional client field</li>
    "! <li>Area field (RELID)</li>
    "! <li>ID field(s) (not SRTF2)</li>
    "! <li>Optional attribute fields</li>
    "! </ul>
    "! <p class="shorttext synchronized" lang="en">Utility for methods *_GET_KEYS of CL_ABAP_EXPIMP_UTILITIES</p>
    "!
    "! @parameter TABNAME | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter with_user_header | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ref_to_KEYtab | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS create_keytab_for_get_keys
      IMPORTING
        tabname              TYPE tabname
        with_user_header     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ref_to_keytab) TYPE REF TO data
      RAISING
        zcx_expimp_table.

    "! <p>Method same as GET_KEYS of CL_ABAP_EXPIMP_UTILITIES, with few corrections,
    "! because there are these bugs in versions up to 7.52 at least (I don't know after 7.52):</p>
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
    "!
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter TABNAME | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter CLIENT | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter AREA | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ID | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter GENERIC_KEY | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter WITH_USER_HEADER | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter CLIENT_SPECIFIED | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter KEYTAB | <p class="shorttext synchronized" lang="en"></p>
    "! @raising CX_SY_CLIENT | <p class="shorttext synchronized" lang="en"></p>
    "! @raising CX_SY_GENERIC_KEY | <p class="shorttext synchronized" lang="en"></p>
    "! @raising CX_SY_TABLINE_TOO_SHORT | <p class="shorttext synchronized" lang="en"></p>
    "! @raising CX_SY_INCORRECT_KEY | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_keys
      IMPORTING
        !tabname          TYPE csequence
        !client           TYPE mandt OPTIONAL
        !area             TYPE relid OPTIONAL
        !id               TYPE clike OPTIONAL
        !generic_key      TYPE abap_bool DEFAULT abap_false
        !with_user_header TYPE abap_bool DEFAULT abap_false
        !client_specified TYPE abap_bool DEFAULT abap_false
      EXPORTING
        !keytab           TYPE STANDARD TABLE
      RAISING
        zcx_expimp_table
        cx_sy_client
        cx_sy_generic_key
        cx_sy_tabline_too_short
        cx_sy_incorrect_key .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter client | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter TABNAME | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter area | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter id | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter id_new | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter key | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter XSTRING | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter WA | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_expimp_table | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS import_as_xstring
      IMPORTING
        client  TYPE mandt DEFAULT sy-mandt
        tabname TYPE tabname
        area    TYPE relid OPTIONAL
        id      TYPE clike OPTIONAL
        id_new  TYPE any OPTIONAL
        key     TYPE any OPTIONAL
      EXPORTING
        xstring TYPE xstring
        wa      TYPE any
      RAISING
        zcx_expimp_table.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS _build_where
      IMPORTING
        tabname        TYPE tabname
        client         TYPE mandt
        area           TYPE relid
        id             TYPE clike
        id_new         TYPE any
        key            TYPE any OPTIONAL
        info           TYPE zcl_expimp_table=>ty_expimp_table_info
      RETURNING
        VALUE(r_where) TYPE string.

ENDCLASS.



CLASS zcl_expimp_table IMPLEMENTATION.


  METHOD create_keytab_for_get_keys.

    DATA(info) = zcl_expimp_table=>get_info( tabname ).

    DATA(lo_struct) = cl_abap_structdescr=>create( VALUE #(
        ( LINES OF COND #( WHEN info-client_fieldname IS NOT INITIAL THEN VALUE #(
            ( name = info-client_fieldname
              type = CAST #( cl_abap_typedescr=>describe_by_data( sy-mandt ) ) ) ) ) )
        ( LINES OF VALUE #(
            ( name = 'RELID'
              type = CAST #( cl_abap_elemdescr=>get_c( 2 ) ) ) ) )
        ( LINES OF VALUE #(
            FOR <id_field> IN info-id_fields
            ( name = <id_field>-fieldname
              type = CAST #( cl_abap_typedescr=>describe_by_name( |{ tabname }-{ <id_field>-fieldname }| ) ) ) ) )
        ( LINES OF COND #( WHEN with_user_header = abap_true THEN VALUE #(
            FOR <fieldname> IN info-attr_fieldnames
            ( name = <fieldname>
              type = CAST #( cl_abap_typedescr=>describe_by_name( |{ tabname }-{ <fieldname> }| ) ) ) ) ) ) ) ).

    DATA(lo_table) = cl_abap_tabledescr=>create( p_line_type = lo_struct ).

    CREATE DATA ref_to_keytab TYPE HANDLE lo_table.

  ENDMETHOD.


  METHOD export_all.

    TYPES:
      BEGIN OF ty_pdat_line,
        paramname TYPE string,
        varname   TYPE c LENGTH 255,
      END OF ty_pdat_line,
      ty_pdat_lines TYPE STANDARD TABLE OF ty_pdat_line WITH EMPTY KEY,
      BEGIN OF ty_pdat_level3,
        p0 TYPE REF TO data,
        p1 TYPE REF TO data,
        p2 TYPE REF TO data,
        p3 TYPE REF TO data,
        p4 TYPE REF TO data,
        p5 TYPE REF TO data,
        p6 TYPE REF TO data,
        p7 TYPE REF TO data,
        p8 TYPE REF TO data,
        p9 TYPE REF TO data,
      END OF ty_pdat_level3,
      BEGIN OF ty_pdat_level2,
        p0 TYPE ty_pdat_level3,
        p1 TYPE ty_pdat_level3,
        p2 TYPE ty_pdat_level3,
        p3 TYPE ty_pdat_level3,
        p4 TYPE ty_pdat_level3,
        p5 TYPE ty_pdat_level3,
        p6 TYPE ty_pdat_level3,
        p7 TYPE ty_pdat_level3,
        p8 TYPE ty_pdat_level3,
        p9 TYPE ty_pdat_level3,
      END OF ty_pdat_level2,
      BEGIN OF ty_pdat_level1,
        p0 TYPE ty_pdat_level2,
        p1 TYPE ty_pdat_level2,
        p2 TYPE ty_pdat_level2,
        p3 TYPE ty_pdat_level2,
        p4 TYPE ty_pdat_level2,
        p5 TYPE ty_pdat_level2,
        p6 TYPE ty_pdat_level2,
        p7 TYPE ty_pdat_level2,
        p8 TYPE ty_pdat_level2,
        p9 TYPE ty_pdat_level2,
      END OF ty_pdat_level1.
    DATA:
      p         TYPE ty_pdat_level1,
      ref_line  TYPE REF TO data,
      xstring   TYPE xstring,
      ref_table TYPE REF TO data.
    FIELD-SYMBOLS:
      <id_field> TYPE ty_id_field,
      <srtf2>    TYPE i,
      <clustr>   TYPE numeric,
      <clustd>   TYPE any,
      <table>    TYPE STANDARD TABLE,
      <database_field> TYPE ANY.


    DATA(properties) = get_info( tabname ).


    DATA(level1) = 0.
    DATA(level2) = 0.
    DATA(level3) = 0.
    DATA(pdat_lines) = VALUE ty_pdat_lines( ).

    LOOP AT tab_cpar REFERENCE INTO DATA(cpar).

      DATA(varname) = |P-P{ level1 }-P{ level2 }-P{ level3 }|.
      ASSIGN (varname) TO FIELD-SYMBOL(<var>).
      <var> = cpar->dref.

      pdat_lines = VALUE #( BASE pdat_lines
          ( paramname = cpar->name varname = |{ varname }->*| ) ).

      level3 = level3 + 1.
      IF level3 = 10.
        level3 = 0.
        level2 = level2 + 1.
        IF level2 = 10.
          level2 = 0.
          level1 = level1 + 1.
          IF level1 = 10.
            " MORE THAN 1000 DATA OBJECTS !
            RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>export_too_many_objects.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.

    TRY.
        IF compression = abap_false.
          EXPORT (pdat_lines) TO DATA BUFFER xstring COMPRESSION OFF.
        ELSE.
          EXPORT (pdat_lines) TO DATA BUFFER xstring COMPRESSION ON.
        ENDIF.
      CATCH cx_sy_compression_error cx_sy_export_buffer_no_memory INTO DATA(lx_export).
        RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>export_data_buffer_error.
    ENDTRY.

    CREATE DATA ref_line TYPE (tabname).
    ASSIGN ref_line->* TO FIELD-SYMBOL(<line>).

    IF key IS NOT INITIAL.
      <line> = CORRESPONDING #( key ).
    ELSE.

      " CLIENT
      IF properties-client_fieldname IS NOT INITIAL.
        ASSIGN COMPONENT properties-client_fieldname OF STRUCTURE <line> TO FIELD-SYMBOL(<client>).
        ASSERT sy-subrc = 0.
        <client> = client.
      ENDIF.

      " AREA
      ASSIGN COMPONENT 'RELID' OF STRUCTURE <line> TO FIELD-SYMBOL(<relid>).
      ASSERT sy-subrc = 0.
      <relid> = area.

      " ID fields
      LOOP AT properties-id_fields ASSIGNING <id_field>.

        ASSIGN COMPONENT <id_field>-fieldname OF STRUCTURE <line> TO <database_field>.
        ASSERT sy-subrc = 0.

        IF id_new IS NOT INITIAL.
          ASSIGN COMPONENT <id_field>-fieldname OF STRUCTURE id_new TO FIELD-SYMBOL(<id_new_field>).
          IF sy-subrc = 0.
            <database_field> = <id_new_field>.
          ENDIF.
        ELSE.
          DATA(len) = nmin( val1 = <id_field>-length val2 = strlen( id ) - <id_field>-offset ).
          IF len = 0.
            EXIT.
          ENDIF.
          <database_field> = id+<id_field>-offset(<id_field>-length).
        ENDIF.

      ENDLOOP.

    ENDIF.

    " Attribute fields
    LOOP AT properties-attr_fieldnames ASSIGNING FIELD-SYMBOL(<attr_fieldname>).
      ASSIGN COMPONENT <attr_fieldname> OF STRUCTURE <line> TO <database_field>.
      ASSERT sy-subrc = 0.
      ASSIGN COMPONENT <attr_fieldname> OF STRUCTURE wa TO FIELD-SYMBOL(<wa_field>).
      <database_field> = <wa_field>.
    ENDLOOP.


    CREATE DATA ref_table TYPE TABLE OF (tabname).
    ASSIGN ref_table->* TO <table>.

    " XSTRING
    IF properties-is_structure_one_row = abap_false.

      ASSIGN COMPONENT 'SRTF2' OF STRUCTURE <line> TO <srtf2>.
      ASSERT sy-subrc = 0.
      ASSIGN COMPONENT 'CLUSTR' OF STRUCTURE <line> TO <clustr>.
      ASSERT sy-subrc = 0.
      ASSIGN COMPONENT 'CLUSTD' OF STRUCTURE <line> TO <clustd>.
      ASSERT sy-subrc = 0.

      DATA(offset) = 0.
      <srtf2> = 0.
      WHILE offset < xstrlen( xstring ).
        <clustd> = xstring+offset.
        offset = offset + properties-clustd_length.
        IF offset < xstrlen( xstring ).
          <clustr> = properties-clustd_length.
        ELSE.
          " last line
          <clustr> = xstrlen( xstring ) - offset + properties-clustd_length.
        ENDIF.
        APPEND <line> TO <table>.
        ADD 1 TO <srtf2>.
      ENDWHILE.

    ELSE.

      ASSIGN COMPONENT 'CLUSTD' OF STRUCTURE <line> TO <clustd>.
      ASSERT sy-subrc = 0.
      <clustd> = xstring.

    ENDIF.

    MODIFY (tabname) FROM TABLE <table>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>database_error.
    ENDIF.

  ENDMETHOD.


  METHOD get_info.

    DATA:
      area_index TYPE i,
      ref_line   TYPE REF TO data.
    FIELD-SYMBOLS:
      <first_field> TYPE any,
      <clustr>      TYPE any,
      <clustd>      TYPE any,
      <line>        TYPE any.

    info = VALUE #( tabname = tabname ).

    " Table must be active and transparent
    SELECT COUNT(*) FROM dd02l
      WHERE tabname  = @tabname
        AND as4local = 'A'
        AND as4vers  = 0
        AND tabclass = 'TRANSP'.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>table_does_not_exist.
    ENDIF.

    " Table columns
    SELECT tabname, fieldname, keyflag, position, rollname, datatype, leng, inttype, intlen
        FROM dd03l
      INTO TABLE @DATA(lt_dd03l)
      WHERE tabname = @tabname
        AND as4local = 'A'
        AND as4vers = 0
        AND fieldname NOT LIKE '.%'.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>table_does_not_exist.
    ENDIF.

    SORT lt_dd03l BY position.

    DATA(number_of_key_fields) = 0.
    DATA(clnt_position) = 0.
    DATA(relid_position) = 0.
    DATA(srtf2_position) = 0.
    DATA(clustr_position) = 0.
    DATA(clustd_position) = 0.

    info-area_offset = 0.
    info-id_offset = 2.

    LOOP AT lt_dd03l ASSIGNING FIELD-SYMBOL(<ls_dd03l>).
      DATA(field_position) = sy-tabix.

      IF <ls_dd03l>-keyflag = 'X'.
        ADD 1 TO number_of_key_fields.
      ENDIF.

      IF field_position = 1
            AND <ls_dd03l>-keyflag = 'X'
            AND <ls_dd03l>-datatype = 'CLNT'.
        clnt_position = field_position.
        info-client_fieldname = <ls_dd03l>-fieldname.
        ADD 3 TO info-total_key_length.
        ADD 3 TO info-area_offset.
        ADD 3 TO info-id_offset.
      ENDIF.

      CASE <ls_dd03l>-fieldname.

        WHEN 'RELID'.
          IF <ls_dd03l>-datatype <> 'CHAR'
                AND <ls_dd03l>-leng <> 2.
            RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
          ENDIF.
          relid_position = field_position.
          ADD 2 TO info-total_key_length.

        WHEN 'SRTF2'.
          IF <ls_dd03l>-datatype <> 'INT1'
                AND <ls_dd03l>-datatype <> 'INT2'
                AND <ls_dd03l>-datatype <> 'INT4'.
            RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
          ENDIF.
          srtf2_position = field_position.
          info-srtf2_length = <ls_dd03l>-intlen.

        WHEN 'CLUSTR'.
          IF <ls_dd03l>-datatype <> 'INT2'.
            RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
          ENDIF.
          clustr_position = field_position.
          ASSIGN <ls_dd03l> TO FIELD-SYMBOL(<dd03l_clustr>).

        WHEN 'CLUSTD'.
          IF <ls_dd03l>-datatype <> 'LRAW' " LRAW required for Multiple Rows data clusters
                AND <ls_dd03l>-datatype <> 'RSTR'. " RAWSTRING required for One Row data clusters
            RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
          ENDIF.
          clustd_position = field_position.
          ASSIGN <ls_dd03l> TO FIELD-SYMBOL(<dd03l_clustd>).
      ENDCASE.

      IF <ls_dd03l>-keyflag = 'X'
            AND field_position <> clnt_position
            AND field_position <> relid_position
            AND field_position <> srtf2_position.
        CASE <ls_dd03l>-inttype.
          WHEN 'C' OR 'N' OR 'D' OR 'T'.
            DATA(ls_id_field) = VALUE ty_id_field( ).
            ls_id_field-offset = info-id_length.
            ls_id_field-length = <ls_dd03l>-leng.
            ls_id_field-fieldname = <ls_dd03l>-fieldname.
            APPEND ls_id_field TO info-id_fields.
            ADD <ls_dd03l>-leng TO info-id_length.
            ADD <ls_dd03l>-leng TO info-total_key_length.
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
        ENDCASE.
      ENDIF.

      " WA
      IF <ls_dd03l>-keyflag = ' '
            AND field_position <> clustr_position
            AND field_position <> clustd_position.
        APPEND <ls_dd03l>-fieldname TO info-attr_fieldnames.
      ENDIF.

    ENDLOOP.

    " The RELID field must be present, EITHER at first position
    " OR, if the table is client-dependent, right after the client
    IF ( info-client_fieldname IS INITIAL AND relid_position <> 1 )
        OR ( info-client_fieldname IS NOT INITIAL AND relid_position <> 2 ).
      RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
    ENDIF.

    " There must be at least one ID field
    IF lines( info-id_fields ) = 0.
      RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
    ENDIF.

    " If SRTF2 is present, it must be the last key field
    IF srtf2_position <> 0 AND srtf2_position <> number_of_key_fields.
      RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
    ENDIF.

    " If CLUSTR is present, it must be the penultimate field
    IF clustr_position <> 0 AND clustr_position <> lines( lt_dd03l ) - 1.
      RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
    ENDIF.

    " CLUSTD must be present and must be the last field
    IF clustd_position <> lines( lt_dd03l ).
      RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
    ENDIF.

    " Check kind of export/import table
    IF srtf2_position <> 0
        AND clustr_position <> 0
        AND <dd03l_clustd>-inttype = 'X'. " VARC or LRAW
      info-is_structure_one_row = abap_false.
    ELSEIF sy-saprl >= '751'
        AND srtf2_position = 0
        AND clustr_position = 0
        AND <dd03l_clustd>-rollname = 'INDX_CLUST_BLOB'.
      info-is_structure_one_row = abap_true.
    ELSE.
      RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
    ENDIF.

    " Calculate the offsets of CLUSTR and CLUSTD
    CREATE DATA ref_line TYPE (tabname).
    ASSIGN ref_line->* TO <line>.

    ASSIGN COMPONENT 1 OF STRUCTURE <line> TO <first_field>.

    ASSIGN COMPONENT 'CLUSTR' OF STRUCTURE <line> TO <clustr>.
    IF sy-subrc = 0.
      DESCRIBE DISTANCE BETWEEN <first_field> AND <clustr> INTO info-offset_clustr IN BYTE MODE.
    ENDIF.

    ASSIGN COMPONENT 'CLUSTD' OF STRUCTURE <line> TO <clustd>.
    ASSERT sy-subrc = 0.
    DESCRIBE DISTANCE BETWEEN <first_field> AND <clustd> INTO info-offset_clustd IN BYTE MODE.
    info-clustd_length = <dd03l_clustd>-intlen.

  ENDMETHOD.


  METHOD get_keys.

    DATA(info) = get_info( tabname ).

    DATA(bugged) = COND abap_bool(
        WHEN sy-saprl > '752'
            THEN abap_false " Let be optimistic that SAP will correct the bugs in 753
        ELSE COND #( WHEN with_user_header = abap_true " BUG 2 and BUG 4
            THEN abap_true
        ELSE COND #( WHEN info-client_fieldname <> 'MANDT' " BUG 1
            THEN abap_true
        ELSE COND #( WHEN info-is_structure_one_row = abap_true " BUG 5
            THEN abap_true
        ELSE COND #( WHEN client_specified = abap_true " BUG 6
            AND client IS INITIAL AND area IS INITIAL
            AND id IS INITIAL AND generic_key = abap_true
            THEN abap_true ) ) ) ) ).

    IF bugged = abap_false.

      cl_abap_expimp_utilities=>db_get_keys(
        EXPORTING
          tabname          = tabname
          client           = client
          area             = area
          id               = id
          generic_key      = generic_key
          with_user_header = with_user_header
          client_specified = client_specified
        IMPORTING
          keytab           = keytab ).
      SORT keytab BY table_line. " <=== BUG 3
      DELETE ADJACENT DUPLICATES FROM keytab. " <=== BUG 3
      RETURN.

    ENDIF.

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


  METHOD import_all.

    zcl_expimp_table=>import_as_xstring(
      EXPORTING
        client  = client
        tabname = tabname
        area    = area
        id      = id
        id_new  = id_new
        key     = key
      IMPORTING
        xstring = DATA(xstring)
        wa      = wa ).

    IF xstring IS INITIAL.
      RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>id_not_found.
    ENDIF.

    TRY.
        cl_abap_expimp_utilities=>dbuf_get_directory(
          EXPORTING
            dbuf = xstring
          IMPORTING
            directory = DATA(directory) ).
        TRY.
            cl_abap_expimp_utilities=>dbuf_convert(
              EXPORTING
                dbuf_in  = xstring
                targ_rel = '752'
              IMPORTING
                dbuf_out = DATA(xstring2) ).
          CATCH cx_parameter_invalid_range.
            ASSERT 1 = 1.
        ENDTRY.
        tab_cpar = cl_abap_expimp_utilities=>dbuf_import_create_data( dbuf = xstring ).

      CATCH cx_sy_import_format_error INTO DATA(lx).
        RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING previous = lx.
    ENDTRY.

  ENDMETHOD.


  METHOD import_as_xstring.

    DATA:
      ref_table TYPE REF TO data,
      where     TYPE string,
      length    TYPE i.
    FIELD-SYMBOLS:
      <id_field>       TYPE ty_id_field,
      <id_new_field>   TYPE clike,
      <table>          TYPE STANDARD TABLE,
      <wa_fieldname>   TYPE any,
      <line_bytes>     TYPE x,
      <clustd>         TYPE any,
      <first_line>     TYPE any,
      <length2>        TYPE int2,
      <length4>        TYPE i,
      <database_field> TYPE any,
      <wa_field>       TYPE any.

    xstring = VALUE #( ).
    CLEAR wa.

    DATA(info) = get_info( tabname ).

    where = _build_where(
          tabname = tabname
          client  = client
          area    = area
          id      = id
          id_new  = id_new
          key     = key
          info    = info ).

    " The following
    " SELECT * FROM (table_name) CLIENT SPECIFIED
    "   WHERE <client-field> = client
    "     AND <area-field>   = area
    "     AND <id-field1>    = <substring-1-of-id>
    "     AND <id-field2>    = <substring-2-of-id>
    "     AND ...
    "   INTO TABLE <table>.
    "   wa-field1 = <table>[ 1 ]-field1.
    "   wa-field2 = <table>[ 1 ]-field2.
    " (which is equivalent to
    " IMPORT (all-fields) FROM DATABASE <table>(<area>) TO <wa> CLIENT <client> ID <id>
    " )
    CREATE DATA ref_table TYPE TABLE OF (tabname).
    ASSIGN ref_table->* TO <table>.
    SELECT * FROM (tabname) CLIENT SPECIFIED
          INTO TABLE <table>
          WHERE (where).
    IF sy-subrc <> 0.
      " EMPTY
      RETURN.
    ENDIF.

    " Sort by SRTF2 ascending
    SORT <table> BY table_line.

    " XSTRING
    IF info-is_structure_one_row = abap_false.
      CLEAR xstring.
      LOOP AT <table> ASSIGNING <line_bytes> CASTING.
        ASSIGN <line_bytes>+info-offset_clustr(2) TO <length2> CASTING.
        IF <length2> <> 0.
          CONCATENATE xstring <line_bytes>+info-offset_clustd(<length2>) INTO xstring IN BYTE MODE.
        ELSE.
          ASSERT 1 = 1. " Weird - That happened in table COVREF in 7.52 SP 0 developer edition.
        ENDIF.
      ENDLOOP.
    ELSE.
      ASSIGN <table>[ 1 ] TO <first_line>.
      ASSIGN COMPONENT 'CLUSTD' OF STRUCTURE <first_line> TO <clustd>.
      ASSERT sy-subrc = 0.
      xstring = <clustd>.
    ENDIF.

    " WA (+ avoid errors in case parameter WA is not passed/is not a structure)
    DATA(rtti_wa) = cl_abap_typedescr=>describe_by_data( wa ).
    IF rtti_wa->kind = rtti_wa->kind_struct.
      ASSIGN <table>[ 1 ] TO <first_line>.
      wa = CORRESPONDING #( <first_line> ).
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_expimp_table.

    TRY.
        zcl_expimp_table=>get_info( tabname ).
        result = abap_true.
      CATCH zcx_expimp_table.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD _build_where.

    DATA:
            dref_table_line TYPE REF TO data.
    FIELD-SYMBOLS:
      <table_line>   TYPE any,
      <client>       TYPE mandt,
      <area>         TYPE relid,
      <id_field>     TYPE zcl_expimp_table=>ty_id_field,
      <id_new_field> TYPE clike.

    IF key IS NOT INITIAL.
      CREATE DATA dref_table_line TYPE (tabname).
      ASSIGN dref_table_line->* TO <table_line>.
      <table_line> = CORRESPONDING #( key ).
      ASSIGN COMPONENT info-client_fieldname OF STRUCTURE <table_line> TO <client>.
      ASSIGN COMPONENT 'RELID' OF STRUCTURE <table_line> TO <area>.
    ENDIF.

    " Build WHERE
    r_where = ''.
    IF info-client_fieldname IS NOT INITIAL.
      r_where = |{ info-client_fieldname } = '{ COND #( WHEN key IS NOT INITIAL THEN <client> ELSE client ) }' AND |.
    ENDIF.

    r_where = |{ r_where }RELID = { cl_abap_dyn_prg=>quote( COND #( WHEN key IS NOT INITIAL THEN <area> ELSE area ) ) }|.

    " Fields part of the "ID" of the Export/Import Table.
    LOOP AT info-id_fields ASSIGNING <id_field>.
      IF key IS NOT INITIAL.
        ASSIGN COMPONENT <id_field>-fieldname OF STRUCTURE <table_line> TO <id_new_field>.
        IF sy-subrc = 0.
          r_where = |{ r_where } AND { <id_field>-fieldname } = {
              cl_abap_dyn_prg=>quote( CONV string( <id_new_field> ) ) }|.
        ENDIF.
      ELSEIF id_new IS NOT INITIAL.
        ASSIGN COMPONENT <id_field>-fieldname OF STRUCTURE id_new TO <id_new_field>.
        IF sy-subrc = 0.
          r_where = |{ r_where } AND { <id_field>-fieldname } = {
              cl_abap_dyn_prg=>quote( CONV string( <id_new_field> ) ) }|.
        ENDIF.
      ELSE.
        DATA(length) = nmin( val1 = <id_field>-length val2 = strlen( id ) - <id_field>-offset ).
        IF length <= 0.
          r_where = |{ r_where } AND { <id_field>-fieldname } = ' '|.
        ELSE.
          " Don't use "substring" because this function doesn't support character structures
          " (ID could be a structure like FUNCTDIR for the export/import table EUFUNC).
          r_where = |{ r_where } AND { <id_field>-fieldname } = {
              cl_abap_dyn_prg=>quote( CONV string( id+<id_field>-offset(length) ) ) }|.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
