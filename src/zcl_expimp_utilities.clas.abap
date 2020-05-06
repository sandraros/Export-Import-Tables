CLASS zcl_expimp_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "==========================================================
    "
    "             code below taken from RSINDX00
    "
    "==========================================================

***** IMPLEMENTATION OF C-STRUCTURES FROM ABCONNE.C

* (1) Transport header

    TYPES: x1 TYPE x LENGTH 1,

           BEGIN OF ty_transport_header,
             id              TYPE x1,  "              /* Kennung              */
             version         TYPE x1,  "              /* Format-Version       */
             intformat       TYPE x1,  "              /* Integerformat        */
             floatformat     TYPE x1,  "              /* Floatformat          */
             compress        TYPE x1,  "              /* Komprimierung ?      */
             datadescription TYPE x1,  "              /* Datenbeschreib?      */
             unused1         TYPE x LENGTH 2,
             codepage        TYPE x LENGTH 4,  "              /* Zeichensatz          */
             unused2         TYPE x LENGTH 4,
           END OF ty_transport_header.

* (2) Object header

*     release 2.2 to 3.0F

    TYPES:
      BEGIN OF ty_object_header_30f,
        id   TYPE x1,  "         /* Kennung */
        ityp TYPE x1,  "         /* Feldtyp */
        leng TYPE x LENGTH 2,  "         /* Feldlaenge */
        next TYPE x LENGTH 2,  "         /* Offset naechstes Objekt */
        nlen TYPE x1,  "         /* Namenslaenge */
      END OF ty_object_header_30f.

*     release 3.1 to 4.6

    TYPES:
      BEGIN OF ty_object_header,
        id    TYPE x1,  "         /* Kennung */
        ityp  TYPE x1,  "         /* Feldtyp */
        leng  TYPE x LENGTH 2,  "         /* Feldlaenge */
        next  TYPE x LENGTH 2,  "         /* Offset naechstes Objekt */
        nlen  TYPE x1,  "         /* Namenslaenge */
        typid TYPE x LENGTH 8,  "         /* Typkennung (Checkinfo)  */
      END OF ty_object_header.

*     release 6.10 and higher (unicode)

    TYPES:
      BEGIN OF ty_object_header_uni,
        id    TYPE x1,  "         /* Kennung */
        ityp  TYPE x1,  "         /* Feldtyp */
        decs  TYPE x1,  "         /* Dezimalstellen bei Typ P
        leng  TYPE x LENGTH 4,  "         /* Feldlaenge */
        next  TYPE x LENGTH 4,  "         /* Offset naechstes Objekt */
        nlen  TYPE x1,  "         /* Namenslaenge */
        thash TYPE x LENGTH 4,  "         /* Hashwert fuer UID
        typid TYPE x LENGTH 16,  "         /* Typkennung (Checkinfo)  */
      END OF ty_object_header_uni.

* (3) data description

    TYPES:
      BEGIN OF ty_data_description,
        id   TYPE x1,
        type TYPE x1,
        flen TYPE x LENGTH 2,
      END OF ty_data_description.

    TYPES:
      BEGIN OF ty_data_description_uni,
        id   TYPE x1,
        type TYPE x1,
        decs TYPE x1,
        flen TYPE x LENGTH 4,
      END OF ty_data_description_uni.

* (4) data area headers

    TYPES:
      BEGIN OF ty_data_interval_descr,
        id  TYPE x1,
        len TYPE x LENGTH 2,
      END OF ty_data_interval_descr.

    TYPES:
      BEGIN OF ty_data_interval_descr_uni,
        id  TYPE x1,
        len TYPE x LENGTH 4,
      END OF ty_data_interval_descr_uni.

    TYPES:
      BEGIN OF ty_data_table_descr,
        id  TYPE x1,
        len TYPE x LENGTH 2,
      END OF ty_data_table_descr.

    TYPES:
      BEGIN OF ty_data_table_descr_uni,
        id    TYPE x1,
        len   TYPE x LENGTH 4,
        lines TYPE x LENGTH 4,
      END OF ty_data_table_descr_uni.

    "==========================================================

    TYPES:
      BEGIN OF ty_data_string_xstring,
        id  TYPE x1,
        len TYPE x LENGTH 4,
      END OF ty_data_string_xstring.

*    TYPES ty_byte TYPE x LENGTH 1.
    TYPES ty_object_id TYPE x LENGTH 1.

    CONSTANTS: BEGIN OF c_object_id,
                 scalar         TYPE ty_object_id VALUE '01',
                 flat_structure TYPE ty_object_id VALUE '02',
                 flat_table     TYPE ty_object_id VALUE '03',
                 end_of_data    TYPE ty_object_id VALUE '04',
                 deep_structure TYPE ty_object_id VALUE '05',
                 deep_table     TYPE ty_object_id VALUE '06',
                 string         TYPE ty_object_id VALUE '07',
               END OF c_object_id.

    "! <ul>
    "! <li>00: C (characters)</li>
    "! <li>01: D (date)</li>
    "! <li>02: P (packed number in BCD)</li>
    "! <li>03: T (time)</li>
    "! <li>04: X (bytes)</li>
    "! <li>05: h (internal table)</li>
    "! <li>06: N (numeric characters)</li>
    "! <li>07: F (binary floating number)</li>
    "! <li>08: I (4-bytes integer)</li>
    "! <li>09: s/int2 (2-bytes integer)</li>
    "! <li>0A: b/int1 (1-byte integer)</li>
    "! <li>0B: w (wide character - obsolete)</li>
    "! <li>0C: unknown 1</li>
    "! <li>0D: unknown 2</li>
    "! <li>0E: u (flat structure)</li>
    "! <li>0F: v (complex structure)</li>
    "! <li>10: ref?</li>
    "! <li>11: obj1?</li>
    "! <li>12: obj2?</li>
    "! <li>13: g/string (string of characters)</li>
    "! <li>14: y/xstring (string of bytes)</li>
    "! <li>15: fref?</li>
    "! <li>16: iref?</li>
    "! <li>17: decfloat16 (decimal floating number with 16-digit mantissa)</li>
    "! <li>18: decfloat34 (decimal floating number with 34-digit mantissa)</li>
    "! <li>1B: 8/int8 (8-bytes integer)</li>
    "! </ul>
    TYPES ty_ityp TYPE x LENGTH 1.
    CONSTANTS: BEGIN OF c_ityp,
                 char       TYPE ty_ityp VALUE '00',
                 date       TYPE ty_ityp VALUE '01',
                 packed     TYPE ty_ityp VALUE '02',
                 time       TYPE ty_ityp VALUE '03',
                 hex        TYPE ty_ityp VALUE '04',
                 table      TYPE ty_ityp VALUE '05',
                 num        TYPE ty_ityp VALUE '06',
                 float      TYPE ty_ityp VALUE '07',
                 int        TYPE ty_ityp VALUE '08',
                 int2       TYPE ty_ityp VALUE '09',
                 int1       TYPE ty_ityp VALUE '0A',
                 w          TYPE ty_ityp VALUE '0B',
                 u1         TYPE ty_ityp VALUE '0C',
                 u2         TYPE ty_ityp VALUE '0D',
                 struct1    TYPE ty_ityp VALUE '0E',
                 struct2    TYPE ty_ityp VALUE '0F',
                 ref        TYPE ty_ityp VALUE '10',
                 obj1       TYPE ty_ityp VALUE '11',
                 obj2       TYPE ty_ityp VALUE '12',
                 string     TYPE ty_ityp VALUE '13',
                 xstring    TYPE ty_ityp VALUE '14',
                 fref       TYPE ty_ityp VALUE '15',
                 iref       TYPE ty_ityp VALUE '16',
                 decfloat16 TYPE ty_ityp VALUE '17',
                 decfloat34 TYPE ty_ityp VALUE '18',
                 int8       TYPE ty_ityp VALUE '1B',
               END OF c_ityp.

    "! Method like CL_ABAP_EXPIMP_UTILITIES=>DBUF_IMPORT_CREATE_DATA
    "! (which does not support all kinds of data buffers).
    METHODS dbuf_import_create_data
      IMPORTING
        dbuf          TYPE xstring
      RETURNING
        VALUE(partab) TYPE tab_cpar
      RAISING
        cx_static_check
        cx_dynamic_check.
*        zcx_expimp_table.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS create_simple_element
      IMPORTING
        type        TYPE x1
        leng        TYPE i
        decs        TYPE i
      RETURNING
        VALUE(rtti) TYPE REF TO cl_abap_datadescr
      RAISING
        zcx_expimp_table.

    METHODS normalize_blob
      CHANGING
               blob TYPE xstring
      RAISING  cx_sy_compression_error
               zcx_expimp_table.

    METHODS read_blob
      EXPORTING
        data TYPE any.

    DATA:
      "! DBUF
      blob TYPE xstring,
      "! DBUF reader (which converts from bytes to characters if needed)
      conv TYPE REF TO cl_abap_conv_in_ce.

    CONSTANTS null_object TYPE REF TO object VALUE IS INITIAL.

ENDCLASS.



CLASS zcl_expimp_utilities IMPLEMENTATION.

  METHOD dbuf_import_create_data.

    TYPES:
      BEGIN OF ty_dd,
        id         TYPE x1,
        "! Internal type
        type       TYPE x1,
        decs       TYPE i,
        "! Number of bytes
        leng       TYPE i,
        components TYPE abap_component_tab,
      END OF ty_dd,
      ty_dds TYPE STANDARD TABLE OF ty_dd WITH EMPTY KEY.

    DATA:
      dds                  TYPE ty_dds,
      dd                   TYPE ty_dd,
      object_id            TYPE ty_object_id,
      transport_header     TYPE ty_transport_header,
      object_header        TYPE ty_object_header,
      object_header_uni    TYPE ty_object_header_uni,
      data_description     TYPE ty_data_description,
      data_description_uni TYPE ty_data_description_uni,
      dah_interval         TYPE ty_data_interval_descr,
      dah_interval_uni     TYPE ty_data_interval_descr_uni,
      dah_table            TYPE ty_data_table_descr,
      dah_table_uni        TYPE ty_data_table_descr_uni,
      dah_string_xstring   TYPE ty_data_string_xstring,
      dref                 TYPE REF TO data,
      rtti                 TYPE REF TO cl_abap_datadescr,
      rtti_s               TYPE TABLE OF REF TO cl_abap_datadescr,
      stack                LIKE TABLE OF rtti_s.
    FIELD-SYMBOLS:
      <fs>    TYPE any,
      <field> TYPE any,
      <table> TYPE table.

    me->blob = dbuf.
    normalize_blob( CHANGING blob = blob ).



    IF xstrlen( blob ) < 16.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.

    conv = cl_abap_conv_in_ce=>create( encoding = '1100' input = blob ).
    read_blob( IMPORTING data = transport_header ).

    DATA(version) = transport_header-version.
    DATA(sap_codepage) = CONV cpcodepage( cl_abap_codepage=>convert_from(
        source   = CONV xstring( transport_header-codepage )
        codepage = 'US-ASCII' ) ).
    DATA(codepage) = cl_abap_codepage=>sap_to_http( sap_codepage ).

    conv = cl_abap_conv_in_ce=>create(
            encoding = CONV #( sap_codepage )
            endian   = COND #( WHEN transport_header-intformat = '01' THEN 'B' ELSE 'L' )
            input    = blob ).
    read_blob( IMPORTING data = transport_header ).

    dd = VALUE #( ).
    dds = VALUE #( ).

    partab = VALUE tab_cpar( ).
    DATA(xstring) = VALUE xstring( ).

    TRY.

        DATA(block_type) = 'OH'.
        WHILE conv->position <= xstrlen( blob ).

          CASE block_type.

            WHEN 'OH'.
              "======================
              " Object Header
              "======================
              dds = VALUE #( ).
              rtti_s = VALUE #( ).
              object_id = blob+conv->position(1).
              IF object_id = c_object_id-end_of_data.
                "======================
                " END OF DATA
                "======================
                " (NB: should be the last byte of the blob)
                EXIT.
              ENDIF.
              CASE version.
                WHEN '06'.
                  read_blob( IMPORTING data = object_header_uni ).
*                  read_blob object_header_uni.
                WHEN OTHERS.
                  read_blob( IMPORTING data = object_header ).
*                  read_blob object_header.
              ENDCASE.
              rtti ?= null_object.
              block_type = 'FN'.

            WHEN 'FN'.
              "======================
              " Field Name
              "======================
              " NLEN contains the number of characters encoded with
              " TRANSPORT_HEADER-CODEPAGE (one character occupies
              " between 1 and 4 bytes).
              CASE version.
                WHEN '06'.
*                  j = object_header_uni-nlen * 4.
                  DATA nlen TYPE i.
                  nlen = object_header_uni-nlen.
                WHEN OTHERS.
*                  j = object_header-nlen * 4.
*                  xstring = read_bloblenx( j ).
                  nlen = object_header-nlen.
              ENDCASE.
              DATA(object_name) = ||.
              conv->read( EXPORTING n = nlen IMPORTING data = object_name ).
*              DATA(object_name) = read_bloblen_c( nlen ).
*              conv->convert(
*                    EXPORTING
*                        input = xstring
*                        n     = nlen
*                    IMPORTING
*                        data  = object_name
*                        len   = j ).
              dd = VALUE #( ).
              dds = VALUE #( ).
              block_type = 'DD'.

            WHEN 'DD'.
              "======================
              " Data Description
              "======================

              IF blob+conv->position(1) BETWEEN 'A0' AND 'AF'.

                CASE version.
                  WHEN '06'.
                    read_blob( IMPORTING data = data_description_uni ).
                    dd = VALUE #(
                        id   = data_description_uni-id
                        type = data_description_uni-type
                        leng = data_description_uni-flen
                        decs = data_description_uni-decs ).
                  WHEN OTHERS.
                    read_blob( IMPORTING data = data_description ).
                    dd = VALUE #(
                        id   = data_description-id
                        type = data_description-type
                        leng = data_description-flen
                        decs = 0 ).
                ENDCASE.

                CASE dd-id.
                  WHEN 'A0'.
                    " begin of structure include
                  WHEN 'A1'.
                    " end of structure include
                  WHEN 'A2'.
                    " begin of boxed component
                  WHEN 'A3'.
                    " end of boxed component
                  WHEN 'AA'.
                    " simple element
                    rtti = create_simple_element( type = dd-type leng = dd-leng decs = dd-decs ).
                  WHEN 'AB'.
                    " begin of structure
                    ASSERT 1 = 1.
                  WHEN 'AC'.
                    " end of structure
                    rtti = cl_abap_structdescr=>get(
                        p_components = VALUE #(
                            FOR rtti2 IN rtti_s INDEX INTO j2
                            ( name       = |CMP{ j2 WIDTH = 4 ALIGN = RIGHT PAD = '0' }|
                              type       = rtti2 ) )
                        p_strict     = abap_false ). "cx_sy_struct_creation
                  WHEN 'AD'.
                    " begin of table
                    ASSERT 1 = 1.
                  WHEN 'AE'.
                    " end of table
                    " NB: the table should have only one component
                    CASE dd-type.
                      WHEN c_ityp-struct1 OR c_ityp-struct2.
                        rtti = cl_abap_structdescr=>get(
                                    p_components = VALUE #(
                                        FOR rtti2 IN rtti_s INDEX INTO j2
                                        ( name       = |CMP{ j2 WIDTH = 4 ALIGN = RIGHT PAD = '0' }|
                                          type       = rtti2 ) )
                                    p_strict     = abap_false ). "cx_sy_struct_creation
                      WHEN OTHERS.
                        rtti = rtti_s[ 1 ].
                    ENDCASE.
                    rtti = cl_abap_tabledescr=>get( p_line_type = rtti ). "cx_sy_table_creation
                  WHEN 'AF'.
                    " alignment bytes
                ENDCASE.

                APPEND dd TO dds.

                IF dd-id <> 'AF'.
                  IF rtti IS BOUND.
                    " end of something
                    IF rtti IS NOT INSTANCE OF cl_abap_elemdescr.
                      " structure or table -> POP stack
                      READ TABLE stack INDEX lines( stack ) INTO rtti_s.
                      DELETE stack INDEX lines( stack ).
                    ENDIF.
                    APPEND rtti TO rtti_s.
                  ELSE.
                    APPEND rtti_s TO stack.
                    rtti_s = VALUE #( ).
                  ENDIF.
                ENDIF.

              ELSE.

                IF rtti IS NOT BOUND.
                  " No DD block -> Use data at Object Header
                  CASE version.
                    WHEN '06'.
                      dd = VALUE #(
                          id   = 'AA'
                          type = object_header_uni-ityp
                          leng = object_header_uni-leng
                          decs = object_header_uni-decs ).
                    WHEN OTHERS.
                      dd = VALUE #(
                          id   = 'AA'
                          type = object_header-ityp
                          leng = object_header-leng ).
                  ENDCASE.
                  dds = VALUE #( ( dd ) ).
                  rtti = create_simple_element( type = dd-type leng = dd-leng decs = dd-decs ).
                ENDIF.

                TYPES : BEGIN OF ty_dv,
                          dref     TYPE REF TO data,
                          rtti     TYPE REF TO cl_abap_datadescr,
                          dd_pos   TYPE i,
                          off      TYPE i,
                          comp_num TYPE i,
                        END OF ty_dv,
                        ty_dvs TYPE STANDARD TABLE OF ty_dv WITH EMPTY KEY.

                DATA(dv) = VALUE ty_dv( rtti = rtti ).
                DATA(dvs) = VALUE ty_dvs( ).

                CREATE DATA dv-dref TYPE HANDLE rtti.
                ASSIGN dv-dref->* TO FIELD-SYMBOL(<dv_field>).

                partab = VALUE #(
                    BASE partab
                    ( name = object_name
                      dref = dref ) ).

                block_type = 'DV'.

              ENDIF.

            WHEN 'DV'.
              "=====================
              "  Data Value
              "=====================

              CASE blob+conv->position(1).

                WHEN 'BD'   " end of interval
                    OR 'BF' " end of table data
                    OR 'CB' " end of string/xstring
                    OR 'CD'." end of boxed component
                  "---------------------
                  " end of something
                  "---------------------
                  conv->skip_x( 1 ).

                  IF lines( dvs ) = 0.
                    RAISE EXCEPTION TYPE zcx_expimp_table.
                  ENDIF.
                  dv = dvs[ lines( dvs ) ].
                  ASSIGN dv-dref->* TO <dv_field>.
                  DELETE dvs INDEX lines( dvs ).
                  IF lines( dvs ) = 0.
                    block_type = 'OH'.
                  ENDIF.

                WHEN 'BB'.
                  "---------------------
                  " single value - no Data Area Header
                  "---------------------
                  " It's the value of the whole field or structure (no filler)

                  DATA len TYPE i.
                  len = dd-leng.
                  read_blob( IMPORTING data = <dv_field> ).

                  IF lines( dvs ) = 0.
                    block_type = 'OH'.
                  ENDIF.

                WHEN 'BC'.
                  "---------------------
                  " Begin of interval (ends with BD)
                  "---------------------
                  " Storage according to Data Description

                  APPEND dv TO dvs.
                  dv = VALUE #( ).

                  CASE version.
                    WHEN '06'.
                      read_blob( IMPORTING data = dah_interval_uni ).
                      len = dah_interval_uni-len.
                    WHEN OTHERS.
                      read_blob( IMPORTING data = dah_interval ).
                      len = dah_interval-len.
                  ENDCASE.

                  conv->read( EXPORTING n = len IMPORTING data = xstring ).

                  WHILE dv-dd_pos < lines( dds ) AND dv-off <= 1.

                    dv-dd_pos = dv-dd_pos + 1.
                    dd = dds[ dv-dd_pos ].

                    CASE TYPE OF dv-rtti.
                      WHEN TYPE cl_abap_structdescr.
                        dv-comp_num = dv-comp_num + 1.
                        ASSIGN COMPONENT dv-comp_num OF STRUCTURE <dv_field> TO <field>.
                        IF sy-subrc <> 0.
                          RAISE EXCEPTION TYPE zcx_expimp_table.
                        ENDIF.
                      WHEN OTHERS.
                        ASSIGN <fs> TO <field>.
                    ENDCASE.
                    " dv_field = xstring+dv_off(dd-leng).

                    CASE dd-type.
                      WHEN c_ityp-char
                          OR c_ityp-date
                          OR c_ityp-num
                          OR c_ityp-time
                          OR c_ityp-string.
                        conv->read( IMPORTING data  = <field> ).
                      WHEN c_ityp-hex
                          OR c_ityp-packed
                          OR c_ityp-xstring.
                        "<field> = dv_field.
                      WHEN c_ityp-decfloat16
                          OR c_ityp-decfloat34
                          OR c_ityp-int
                          OR c_ityp-int1
                          OR c_ityp-int2
                          OR c_ityp-int8
                          OR c_ityp-float.
                        conv->read( IMPORTING data  = <field> ).
                      WHEN OTHERS.
                        RAISE EXCEPTION TYPE zcx_expimp_table.
                    ENDCASE.
                    dv-off = dv-off + dd-leng.

                    CASE dd-id.
                      WHEN 'AB'.
                        " begin of structure
                      WHEN 'AC'.
                        " end of structure
                        "READ TABLE drefs INDEX lines( drefs ) INTO dref.
                        "DELETE drefs INDEX lines( drefs ).
                        "ASSIGN dref->* TO <fs>.
                      WHEN 'AF'.
                        " Filler
                        dv-off = dv-off + dd-leng.
                    ENDCASE.

                    dv-dd_pos = dv-dd_pos + 1.
                  ENDWHILE.

                WHEN 'BE'.
                  "---------------------
                  " begin of table data (ends with BF)
                  "---------------------

                  APPEND dv TO dvs.
                  dv = VALUE #( ).

                  " read width and number of lines of the internal table
                  CASE version.
                    WHEN '06'.
                      read_blob( IMPORTING data = dah_table_uni ).
                    WHEN OTHERS.
                      read_blob( IMPORTING data = dah_table ).
                  ENDCASE.

                  " Create a work area for the line
                  FIELD-SYMBOLS <dv_table> TYPE ANY TABLE.
                  ASSIGN <dv_field> TO <dv_table>.
                  CREATE DATA dv-dref LIKE LINE OF <dv_table>.
                  ASSIGN dv-dref->* TO <dv_field>.
                  dv-rtti ?= cl_abap_typedescr=>describe_by_data( <dv_field> ).

                WHEN 'CA'.
                  "---------------------
                  " begin of string/xstring (ends with CB)
                  "---------------------

                  APPEND dv TO dvs.

                  read_blob( IMPORTING data = dah_string_xstring ).
                  conv->read( IMPORTING data  = <dv_field> ).

                WHEN 'CC'.
                  "---------------------
                  " begin of boxed component (ends with CD)
                  "---------------------

                  APPEND dv TO dvs.

                  " TODO
                  RAISE EXCEPTION TYPE zcx_expimp_table.

                WHEN OTHERS.
                  RAISE EXCEPTION TYPE zcx_expimp_table.

              ENDCASE.
          ENDCASE.

          IF rtti IS INSTANCE OF cl_abap_tabledescr
              AND dv-off = dd-leng.
            " Will not work with sorted and hashed internal tables,
            " as the key components cannot be changed.
*                ASSIGN <fs> TO <table>.
*                INSERT INITIAL LINE INTO TABLE <table> ASSIGNING <fs>.
            INSERT <fs> INTO TABLE <table>.
            rtti ?= cl_abap_typedescr=>describe_by_data( <fs> ).
            dv-comp_num = 0.
            dv-dd_pos = 1.
          ENDIF.

*      ENDCASE.

        ENDWHILE.

      CATCH cx_root INTO DATA(lx).
        RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING previous = lx.
    ENDTRY.

  ENDMETHOD.

  METHOD normalize_blob.

    " This code is copied from subroutine READ_BLOB of program RSINDX00 and adapted.

    DATA l_off TYPE i.
    DATA l_len TYPE i.
    DATA lr_blob TYPE REF TO indx_clust_blob ##NEEDED.
    DATA  datalen      TYPE p.
    DATA:
      BEGIN OF l_xdat,
        srtf2  TYPE indx-srtf2,
        clustr TYPE indx-clustr,
        clustd TYPE x LENGTH 1000,
      END OF l_xdat.
    DATA l_otab LIKE TABLE OF l_xdat.
    DATA l_stab LIKE TABLE OF l_xdat.

    datalen = xstrlen( blob ).

    IF datalen < 4.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.

    IF blob+4(1) = '02'. "compressed data
      l_off = 0.
      DO.
        IF l_off < datalen.
          IF datalen - l_off >= 1000.
            l_len = 1000.
          ELSE.
            l_len = datalen - l_off.
          ENDIF.
          l_xdat-srtf2 = sy-index - 1.
          l_xdat-clustr = l_len.
          l_xdat-clustd = blob+l_off(l_len).
          APPEND l_xdat TO l_stab.
          l_off = l_off + 1000.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.

      CLEAR blob.

      CALL 'AB_IMPORT_DECOMPRESS'
        ID 'SRCTAB' FIELD l_stab[]
        ID 'DSTTAB' FIELD l_otab[].

      CLEAR l_stab.

      LOOP AT l_otab REFERENCE INTO DATA(l_o).
        CONCATENATE blob l_o->clustd(l_o->clustr) INTO blob IN BYTE MODE.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD create_simple_element.

    TRY.
        CASE type.
          WHEN c_ityp-char.
            rtti = cl_abap_elemdescr=>get_c( leng ).
          WHEN c_ityp-date.
            rtti = cl_abap_elemdescr=>get_d( ).
          WHEN c_ityp-decfloat16.
            rtti = cl_abap_elemdescr=>get_decfloat16( ).
          WHEN c_ityp-decfloat34.
            rtti = cl_abap_elemdescr=>get_decfloat34( ).
          WHEN c_ityp-float.
            rtti = cl_abap_elemdescr=>get_f( ).
          WHEN c_ityp-hex.
            rtti = cl_abap_elemdescr=>get_x( leng ).
          WHEN c_ityp-int.
            rtti = cl_abap_elemdescr=>get_i( ).
          WHEN c_ityp-int1.
            rtti = cl_abap_elemdescr=>get_int1( ).
          WHEN c_ityp-int2.
            rtti = cl_abap_elemdescr=>get_int2( ).
          WHEN c_ityp-int8.
            rtti = cl_abap_elemdescr=>get_int8( ).
          WHEN c_ityp-num.
            rtti = cl_abap_elemdescr=>get_n( leng ).
          WHEN c_ityp-packed.
            rtti = cl_abap_elemdescr=>get_p( p_length = leng p_decimals = decs ).
          WHEN c_ityp-string.
            rtti = cl_abap_elemdescr=>get_string( ).
          WHEN c_ityp-time.
            rtti = cl_abap_elemdescr=>get_t( ).
          WHEN c_ityp-xstring.
            rtti = cl_abap_elemdescr=>get_xstring( ).
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_expimp_table.
        ENDCASE.
      CATCH cx_parameter_invalid_range INTO DATA(lx).
        " error has occurred at GET_C, GET_N, GET_P or GET_X.
        RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING previous = lx.
    ENDTRY.

  ENDMETHOD.

  METHOD read_blob.

    FIELD-SYMBOLS <x> TYPE x.

    DATA(xstring) = VALUE xstring( ).

    DESCRIBE FIELD data LENGTH DATA(length) IN BYTE MODE.
    conv->read( EXPORTING n = length IMPORTING data = xstring ).
    ASSIGN data TO <x> CASTING.
    <x> = xstring.

  ENDMETHOD.

ENDCLASS.
