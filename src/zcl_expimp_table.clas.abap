"! <p class="shorttext synchronized" lang="en">Utility for export/import tables</p>
"! See demonstration in program Z_EXPIMP_TABLE_DEMO.
"! Projects using it:<ul>
"! <li>https://github.com/sandraros/FM-Test-Data</li>
"! </ul>
"! NB:<ul>
"! <li>Dynamic deletion may be achieved using CL_ABAP_EXPIMP_UTILITIES=>DB_DELETE</li>
"! </ul>
CLASS zcl_expimp_table DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Advantages of the method IMPORT_ALL compared to the classic IMPORT ... FROM DATABASE ...:<br/>
    "! <ul>
    "! <li>Indicate the table name and the area dynamically</li>
    "! <li>Read all the data objects of the data cluster, no need of
    "! indicating the types and names of data objects to read.</li>
    "! </ul>
    "! Example:<br/>
    "! DATA(nummer) = CONV eufunc-nummer( 1 ).<br/>
    "! DATA(id_wa) = VALUE eufunc( gruppe = 'SCAL' name = 'DATE_GET_WEEK' nummer = nummer ).<br/>
    "! zcl_expimp_table=>import_all(<br/>
    "! &nbsp;&nbsp;&nbsp;EXPORTING client = '100' table_name = 'EUFUNC' area = 'FL'<br/>
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
    "! <p class="shorttext synchronized" lang="en">Read all data objects</p>
    "!
    "! @parameter client | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter TABNAME | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter area | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter tab_cpar | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS import_all
      IMPORTING
        client   TYPE mandt DEFAULT sy-mandt
        tabname  TYPE tabname
        area     TYPE relid
        id       TYPE clike OPTIONAL
        id_new   TYPE any OPTIONAL
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
    "! @parameter WA | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter tab_cpar | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter compression | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS export_all
      IMPORTING
        client      TYPE mandt DEFAULT sy-mandt
        tabname     TYPE tabname
        area        TYPE relid
        id          TYPE clike OPTIONAL
        id_new      TYPE any OPTIONAL
        wa          TYPE any
        tab_cpar    TYPE tab_cpar
        compression TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_expimp_table.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_id_field,
             fieldname TYPE fieldname,
             offset    TYPE i,
             length    TYPE i,
           END OF ty_id_field,
           BEGIN OF ty_descr_export_import_table,
             is_structure_751 TYPE abap_bool,
             client_fieldname TYPE fieldname,
             id_fields        TYPE STANDARD TABLE OF ty_id_field WITH EMPTY KEY,
             wa_fieldnames    TYPE STANDARD TABLE OF fieldname WITH EMPTY KEY,
             offset_clustr    TYPE i,
             clustr_length    TYPE i,
             offset_clustd    TYPE i,
             clustd_length    TYPE i,
           END OF ty_descr_export_import_table.

    CLASS-METHODS import_from_database_2_xstring
      IMPORTING
        client  TYPE mandt
        tabname TYPE tabname
        area    TYPE relid
        id      TYPE clike
        id_new  TYPE any
      EXPORTING
        xstring TYPE xstring
        wa      TYPE any
      RAISING
        zcx_expimp_table.

    CLASS-METHODS describe_export_import_table
      IMPORTING
        tabname       TYPE tabname
      RETURNING
        VALUE(result) TYPE ty_descr_export_import_table
      RAISING
        zcx_expimp_table.

    TYPES: BEGIN OF ty_expimp_table,
             has_client_field TYPE abap_bool,
             is_structure_751 TYPE abap_bool,
             columns          TYPE STANDARD TABLE OF dd03l-fieldname WITH EMPTY KEY,
             id_first         TYPE i,
             id_last          TYPE i,
             attr_first       TYPE i,
             attr_last        TYPE i,
           END OF ty_expimp_table.

ENDCLASS.



CLASS zcl_expimp_table IMPLEMENTATION.


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
      <table>    TYPE STANDARD TABLE.


    DATA(properties) = describe_export_import_table( tabname ).


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

      ASSIGN COMPONENT <id_field>-fieldname OF STRUCTURE <line> TO FIELD-SYMBOL(<database_field>).
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

    " XSTRING
    IF properties-is_structure_751 = abap_false.

      CREATE DATA ref_table TYPE TABLE OF (tabname).
      ASSIGN ref_table->* TO <table>.
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

      MODIFY (tabname) FROM TABLE <table>.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>database_error.
      ENDIF.

    ELSE.

      ASSIGN COMPONENT 'CLUSTD' OF STRUCTURE <line> TO <clustd>.
      ASSERT sy-subrc = 0.
      <clustd> = xstring.

      MODIFY (tabname) FROM <line>.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>database_error.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD import_all.

    zcl_expimp_table=>import_from_database_2_xstring(
      EXPORTING
        client  = client
        tabname = tabname
        area    = area
        id      = id
        id_new  = id_new
      IMPORTING
        xstring = DATA(xstring)
        wa      = wa ).

    tab_cpar = cl_abap_expimp_utilities=>dbuf_import_create_data( dbuf = xstring ).

  ENDMETHOD.


  METHOD import_from_database_2_xstring.

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

    CLEAR: xstring,
           wa.

    DATA(properties) = describe_export_import_table( tabname ).

    " Build WHERE
    where = ''.
    IF properties-client_fieldname IS NOT INITIAL.
      where = |{ properties-client_fieldname } = '{ client }' AND |.
    ENDIF.

    where = |{ where }RELID = { cl_abap_dyn_prg=>quote( area ) }|.

    " Fields part of the "ID" of the Export/Import Table.
    LOOP AT properties-id_fields ASSIGNING <id_field>.
      IF id_new IS NOT INITIAL.
        ASSIGN COMPONENT <id_field>-fieldname OF STRUCTURE id_new TO <id_new_field>.
        IF sy-subrc = 0.
          where = |{ where } AND { <id_field>-fieldname } = { cl_abap_dyn_prg=>quote( <id_new_field> ) }|.
        ENDIF.
      ELSE.
        DATA(len) = nmin( val1 = <id_field>-length val2 = strlen( id ) - <id_field>-offset ).
        IF len = 0.
          EXIT.
        ENDIF.
        where = |{ where } AND { <id_field>-fieldname } = { cl_abap_dyn_prg=>quote( id+<id_field>-offset(<id_field>-length) ) }|.
      ENDIF.
    ENDLOOP.

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

    SORT <table> BY table_line.

    " XSTRING
    IF properties-is_structure_751 = abap_false.
      CLEAR xstring.
      LOOP AT <table> ASSIGNING <line_bytes> CASTING.
        IF properties-clustr_length = 2.
          ASSIGN <line_bytes>+properties-offset_clustr(2) TO <length2> CASTING.
          CONCATENATE xstring <line_bytes>+properties-offset_clustd(<length2>) INTO xstring IN BYTE MODE.
        ELSE.
          ASSIGN <line_bytes>+properties-offset_clustr(4) TO <length4> CASTING.
          CONCATENATE xstring <line_bytes>+properties-offset_clustd(<length4>) INTO xstring IN BYTE MODE.
        ENDIF.
      ENDLOOP.
    ELSE.
      ASSIGN <table>[ 1 ] TO <first_line>.
      ASSIGN COMPONENT 'CLUSTD' OF STRUCTURE <first_line> TO <clustd>.
      ASSERT sy-subrc = 0.
      xstring = <clustd>.
    ENDIF.

    " WA
    ASSIGN <table>[ 1 ] TO <first_line>.
    LOOP AT properties-wa_fieldnames ASSIGNING <wa_fieldname>.
      ASSIGN COMPONENT <wa_fieldname> OF STRUCTURE wa TO <wa_field>.
      IF sy-subrc = 0.
        ASSIGN COMPONENT <wa_fieldname> OF STRUCTURE <first_line> TO <database_field>.
        ASSERT sy-subrc = 0.
        <wa_field> = <database_field>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD describe_export_import_table.

    DATA:
      ls_dd02l   TYPE dd02l,
      lt_dd03l   TYPE TABLE OF dd03l,
      area_index TYPE i,
      ref_line   TYPE REF TO data.
    FIELD-SYMBOLS:
      <ls_dd03l>     TYPE dd03l,
      <first_field>  TYPE any,
      <dd03l_clustr> TYPE dd03l,
      <dd03l_clustd> TYPE dd03l,
      <clustr>       TYPE any,
      <clustd>       TYPE any,
      <line>         TYPE any.

    " Table must be active and transparent
    SELECT SINGLE * FROM dd02l
      WHERE tabname  = @tabname
        AND as4local = 'A'
        AND as4vers  = 0
        AND tabclass = 'TRANSP'
      INTO @ls_dd02l.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>table_does_not_exist.
    ENDIF.

    " Table columns
    SELECT * FROM dd03l
      INTO TABLE lt_dd03l
      WHERE tabname = tabname
        AND as4local = 'A'
        AND as4vers = 0
        AND fieldname NOT LIKE '.%'.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>table_does_not_exist.
    ENDIF.

    SORT lt_dd03l BY position.

    DATA(number_of_key_fields) = 0.
    DATA(id_offset) = 0.
    DATA(clnt_position) = 0.
    DATA(relid_position) = 0.
    DATA(srtf2_position) = 0.
    DATA(clustr_position) = 0.
    DATA(clustd_position) = 0.

    LOOP AT lt_dd03l ASSIGNING <ls_dd03l>.
      DATA(field_position) = sy-tabix.

      IF <ls_dd03l>-keyflag = 'X'.
        ADD 1 TO number_of_key_fields.
      ENDIF.

      IF field_position = 1 AND <ls_dd03l>-keyflag = 'X' AND <ls_dd03l>-datatype = 'CLNT'.
        clnt_position = field_position.
        result-client_fieldname = abap_true.
      ENDIF.

      CASE <ls_dd03l>-fieldname.
        WHEN 'RELID'.
          IF <ls_dd03l>-datatype <> 'CHAR' AND <ls_dd03l>-leng <> 2.
            RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
          ENDIF.
          relid_position = field_position.
        WHEN 'SRTF2'.
          IF <ls_dd03l>-datatype <> 'INT4'.
            RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
          ENDIF.
          srtf2_position = field_position.
        WHEN 'CLUSTR'.
          IF <ls_dd03l>-datatype <> 'INT2' AND <ls_dd03l>-datatype <> 'INT4'.
            RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
          ENDIF.
          clustr_position = field_position.
          ASSIGN <ls_dd03l> TO <dd03l_clustr>.
        WHEN 'CLUSTD'.
          IF <ls_dd03l>-inttype <> 'X' AND <ls_dd03l>-datatype <> 'RSTR'.
            RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
          ENDIF.
          clustd_position = field_position.
          ASSIGN <ls_dd03l> TO <dd03l_clustd>.
      ENDCASE.

      IF <ls_dd03l>-keyflag = 'X'
            AND field_position <> clnt_position
            AND field_position <> relid_position
            AND field_position <> srtf2_position.
        CASE <ls_dd03l>-datatype.
          WHEN 'CHAR' OR 'NUM' OR 'DATS' OR 'TIMS'.
            DATA(ls_id_field) = VALUE ty_id_field( ).
            ls_id_field-offset = id_offset.
            ls_id_field-length = <ls_dd03l>-leng.
            ls_id_field-fieldname = <ls_dd03l>-fieldname.
            APPEND ls_id_field TO result-id_fields.
            ADD <ls_dd03l>-leng TO id_offset.
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
        ENDCASE.
      ENDIF.

      " WA
      IF <ls_dd03l>-keyflag = ' '
            AND field_position <> clustr_position
            AND field_position <> clustd_position.
        APPEND <ls_dd03l>-fieldname TO result-wa_fieldnames.
      ENDIF.

    ENDLOOP.

    " The RELID field must be present, EITHER at first position OR right after the client if it's present
    IF ( result-client_fieldname IS INITIAL AND relid_position <> 1 )
        OR ( result-client_fieldname IS NOT INITIAL AND relid_position <> 2 ).
      RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
    ENDIF.

    " There must be at least one ID field
    IF lines( result-id_fields ) = 0.
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
        AND <dd03l_clustd>-inttype = 'X'.
      result-is_structure_751 = abap_false.
    ELSEIF srtf2_position = 0
        AND clustr_position = 0
        AND <dd03l_clustd>-datatype = 'RSTR'.
      result-is_structure_751 = abap_true.
    ELSE.
      RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
    ENDIF.

    " Calculate the offsets of CLUSTR and CLUSTD
    CREATE DATA ref_line TYPE (tabname).
    ASSIGN ref_line->* TO <line>.

    ASSIGN COMPONENT 1 OF STRUCTURE <line> TO <first_field>.

    IF clustr_position <> 0.
      ASSIGN COMPONENT 'CLUSTR' OF STRUCTURE <line> TO <clustr>.
      ASSERT sy-subrc = 0.
      DESCRIBE DISTANCE BETWEEN <first_field> AND <clustr> INTO result-offset_clustr IN BYTE MODE.
      result-clustr_length = <dd03l_clustr>-intlen.
    ENDIF.

    ASSIGN COMPONENT 'CLUSTD' OF STRUCTURE <line> TO <clustd>.
    ASSERT sy-subrc = 0.
    DESCRIBE DISTANCE BETWEEN <first_field> AND <clustd> INTO result-offset_clustd IN BYTE MODE.
    result-clustd_length = <dd03l_clustd>-intlen.

  ENDMETHOD.

ENDCLASS.
