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

    TYPES ty_object_id TYPE x LENGTH 1.
    CONSTANTS: BEGIN OF c_object_id,
                 primitive      TYPE ty_object_id VALUE '01',
                 flat_structure TYPE ty_object_id VALUE '02',
                 flat_table     TYPE ty_object_id VALUE '03',
                 end_of_data    TYPE ty_object_id VALUE '04',
                 deep_structure TYPE ty_object_id VALUE '05',
                 deep_table     TYPE ty_object_id VALUE '06',
                 string         TYPE ty_object_id VALUE '07',
               END OF c_object_id.

    TYPES: x1 TYPE x LENGTH 1,

           BEGIN OF ty_transport_header,
             "! X
             id              TYPE ty_object_id,
             "! Format version
             version         TYPE x1,
             "! Integer format
             intformat       TYPE x1,
             "! Float format
             floatformat     TYPE x1,
             "! Compress
             compress        TYPE x1,
             "! Data description
             datadescription TYPE x1,
             unused1         TYPE x LENGTH 2,
             "! SAP Code Page (4 numeric characters in encoding US-ASCII
             codepage        TYPE x LENGTH 4,
             unused2         TYPE x LENGTH 4,
           END OF ty_transport_header.

* (2) Object header

*     release 2.2 to 3.0F

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


    TYPES:
      BEGIN OF ty_object_header_30f,
        id   TYPE x1,  "         /* Kennung */
        ityp TYPE ty_ityp,
        leng TYPE x LENGTH 2,  "         /* Feldlaenge */
        next TYPE x LENGTH 2,  "         /* Offset naechstes Objekt */
        nlen TYPE x1,  "         /* Namenslaenge */
      END OF ty_object_header_30f.

*     release 3.1 to 4.6

    TYPES:
      BEGIN OF ty_object_header,
        "! Field type category
        id    TYPE ty_object_id,
        "! Field type
        ityp  TYPE ty_ityp,
        "! Field length in number of bytes
        leng  TYPE x LENGTH 2,
        "! Offset to next object (usually zero)
        next  TYPE x LENGTH 2,
        "! Length of object name in number of characters
        nlen  TYPE x1,
        "! Type identifier (check info)
        typid TYPE x LENGTH 8,
      END OF ty_object_header.

*     release 6.10 and higher (unicode)

    TYPES:
      BEGIN OF ty_object_header_uni,
        "! Field type category
        id    TYPE ty_object_id,
        "! Field type
        ityp  TYPE ty_ityp,
        "! Decimals for type P
        decs  TYPE x1,
        "! Field length in number of bytes
        leng  TYPE x LENGTH 4,
        "! Offset to next object (usually zero)
        next  TYPE x LENGTH 4,
        "! Length of object name in number of characters
        nlen  TYPE x1,
        "! Hash key for UID
        thash TYPE x LENGTH 4,
        "! Type identifier (check info)
        typid TYPE x LENGTH 16,
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
      BEGIN OF ty_dah_interval,
        id  TYPE x1,
        len TYPE x LENGTH 2,
      END OF ty_dah_interval.

    TYPES:
      BEGIN OF ty_dah_interval_uni,
        id  TYPE x1,
        len TYPE x LENGTH 4,
      END OF ty_dah_interval_uni.

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
      ty_dds TYPE STANDARD TABLE OF ty_dd WITH EMPTY KEY,
      BEGIN OF ty_current_dv_block,
        id   TYPE x LENGTH 1,
        off  TYPE i,
        len  TYPE i,
        dref TYPE REF TO data,
      END OF ty_current_dv_block.


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

    METHODS read_data_value
      IMPORTING
        xdds TYPE ty_dds
      EXPORTING
        dv   TYPE any
        off  TYPE i
      RAISING
        zcx_expimp_table.

    METHODS read_data_object
      IMPORTING
        xdds TYPE ty_dds
      EXPORTING
        dv   TYPE any
        off  TYPE i
      RAISING
        zcx_expimp_table.

    METHODS read_data_primitive
      IMPORTING
        xdds TYPE ty_dds
      EXPORTING
        dv   TYPE any
        off  TYPE i
      RAISING
        zcx_expimp_table.

    METHODS read_data_structure
      IMPORTING
        xdds TYPE ty_dds
      EXPORTING
        dv   TYPE any
        off  TYPE i
      RAISING
        zcx_expimp_table.

    METHODS read_data_table
      IMPORTING
        xdds TYPE ty_dds
      EXPORTING
        dv   TYPE any
        off  TYPE i
      RAISING
        zcx_expimp_table.

    METHODS read_data_value_single
      IMPORTING
        xdds TYPE ty_dds
      EXPORTING
        dv   TYPE any
        off  TYPE i
      RAISING
        zcx_expimp_table.

    METHODS read_data_value_interval
      IMPORTING
        xdds TYPE ty_dds
      EXPORTING
        dv   TYPE any
        off  TYPE i
      RAISING
        zcx_expimp_table.

    METHODS read_data_value_table
      IMPORTING
        xdds TYPE ty_dds
      EXPORTING
        dv   TYPE any
        off  TYPE i
      RAISING
        zcx_expimp_table.

    METHODS read_data_value_string_xstring
      IMPORTING
        xdds TYPE ty_dds
      EXPORTING
        dv   TYPE any
        off  TYPE i
      RAISING
        zcx_expimp_table.

    METHODS read_data_value_boxed
      IMPORTING
        xdds TYPE ty_dds
      EXPORTING
        dv   TYPE any
        off  TYPE i
      RAISING
        zcx_expimp_table.

    METHODS process_object
      RAISING
        zcx_expimp_table.

    METHODS process_data_description
      RAISING
        zcx_expimp_table.

    METHODS create_default_description
      RAISING
        zcx_expimp_table.

    DATA:
      "! DBUF
      blob    TYPE xstring,
      "! DBUF reader (which converts from bytes to characters if needed)
      conv    TYPE REF TO cl_abap_conv_in_ce,
      "! Version:<ul>
      "! <li>06: >= 6.10 Unicode</li>
      "! <li>05: </li>
      "! </ul>
      version TYPE zcl_expimp_utilities=>ty_transport_header-version,
      "! Return value of method DBUF_IMPORT_CREATE_DATA
      partab  TYPE tab_cpar,
      "! Temporary variables for retaining current element processed
      BEGIN OF current,
        BEGIN OF object,
          "! Current object header
          header_uni TYPE ty_object_header_uni,
          "! Current object name
          name       TYPE string,
        END OF object,
        BEGIN OF dd,
          "! Data Descriptions
          lines           TYPE ty_dds,
          "! RTTI
          rtti            TYPE REF TO cl_abap_datadescr,
          rtti_s          TYPE TABLE OF REF TO cl_abap_datadescr,
          stack_of_rtti_s LIKE TABLE OF current-dd-rtti_s,
        END OF dd,
        BEGIN OF dv,
          "! Index current Data Description while parsing Data Values
          dd_index TYPE i,
          "! current Data Value block (BB, BC, etc.)
          block    TYPE ty_current_dv_block,
        END OF dv,
      END OF current.

    CONSTANTS null_object TYPE REF TO object VALUE IS INITIAL.

ENDCLASS.



CLASS zcl_expimp_utilities IMPLEMENTATION.

  METHOD dbuf_import_create_data.

    DATA:
*      dd               TYPE ty_dd,
*      object_id        TYPE ty_object_id,
      transport_header TYPE ty_transport_header.
*      object_header        TYPE ty_object_header,
*      object_header_uni    TYPE ty_object_header_uni,
*      data_description     TYPE ty_data_description,
*      data_description_uni TYPE ty_data_description_uni,
*      dref             TYPE REF TO data,
*      rtti             TYPE REF TO cl_abap_datadescr,
*      rtti_s           TYPE TABLE OF REF TO cl_abap_datadescr,
*      stack            LIKE TABLE OF rtti_s.
*    FIELD-SYMBOLS:
*      <fs>    TYPE any,
*      <field> TYPE any,
*      <table> TYPE table.

    me->blob = dbuf.
    normalize_blob( CHANGING blob = blob ).



    IF xstrlen( blob ) < 16.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.

    conv = cl_abap_conv_in_ce=>create( encoding = '1100' input = blob ).
    read_blob( IMPORTING data = transport_header ).

    version = transport_header-version.
    DATA(sap_codepage) = CONV cpcodepage( cl_abap_codepage=>convert_from(
        source   = CONV xstring( transport_header-codepage )
        codepage = 'US-ASCII' ) ).
    DATA(codepage) = cl_abap_codepage=>sap_to_http( sap_codepage ).

    conv = cl_abap_conv_in_ce=>create(
            encoding = CONV #( sap_codepage )
            endian   = COND #( WHEN transport_header-intformat = '01' THEN 'B' ELSE 'L' )
            input    = blob ).
    " CONV restart the blob from position zero, so move after the transport header.
    DESCRIBE FIELD transport_header LENGTH DATA(length) IN BYTE MODE.
    conv->skip_x( length ).

*    dd = VALUE #( ).
    current-dd-lines = VALUE #( ).

    partab = VALUE tab_cpar( ).
    DATA(xstring) = VALUE xstring( ).

    DO.

      CASE blob+conv->position(1).
        WHEN c_object_id-end_of_data. "04"
          EXIT.
        WHEN OTHERS.
          process_object( ).
      ENDCASE.

    ENDDO.

*    TRY.
*
*        DATA(block_type) = 'OH'.
*        WHILE conv->position <= xstrlen( blob ).
*
*          CASE block_type.
*
*            WHEN 'OH'.
*              "======================
*              " Object Header
*              "======================
*              dds = VALUE #( ).
*              rtti_s = VALUE #( ).
*              object_id = blob+conv->position(1).
*              IF object_id = c_object_id-end_of_data.
*                "======================
*                " END OF DATA
*                "======================
*                " (NB: should be the last byte of the blob)
*                EXIT.
*              ENDIF.
*              CASE version.
*                WHEN '06'.
*                  read_blob( IMPORTING data = object_header_uni ).
**                  read_blob object_header_uni.
*                WHEN OTHERS.
*                  read_blob( IMPORTING data = object_header ).
**                  read_blob object_header.
*              ENDCASE.
*              rtti ?= null_object.
*              block_type = 'FN'.
*
*            WHEN 'FN'.
*              "======================
*              " Field Name
*              "======================
*              " NLEN contains the number of characters encoded with
*              " TRANSPORT_HEADER-CODEPAGE (one character occupies
*              " between 1 and 4 bytes).
*              CASE version.
*                WHEN '06'.
*                  DATA nlen TYPE i.
*                  nlen = object_header_uni-nlen.
*                WHEN OTHERS.
*                  nlen = object_header-nlen.
*              ENDCASE.
*              DATA(object_name) = ||.
*              conv->read( EXPORTING n = nlen IMPORTING data = object_name ).
*              dd = VALUE #( ).
*              dds = VALUE #( ).
*              block_type = 'DD'.
*
*            WHEN 'DD'.
*              "======================
*              " Data Description
*              "======================
*
*              IF blob+conv->position(1) BETWEEN 'A0' AND 'AF'.
*
*                CASE version.
*                  WHEN '06'.
*                    read_blob( IMPORTING data = data_description_uni ).
*                    dd = VALUE #(
*                        id   = data_description_uni-id
*                        type = data_description_uni-type
*                        leng = data_description_uni-flen
*                        decs = data_description_uni-decs ).
*                  WHEN OTHERS.
*                    read_blob( IMPORTING data = data_description ).
*                    dd = VALUE #(
*                        id   = data_description-id
*                        type = data_description-type
*                        leng = data_description-flen
*                        decs = 0 ).
*                ENDCASE.
*
*                CASE dd-id.
*                  WHEN 'A0'.
*                    " begin of structure include
*                  WHEN 'A1'.
*                    " end of structure include
*                  WHEN 'A2'.
*                    " begin of boxed component
*                  WHEN 'A3'.
*                    " end of boxed component
*                  WHEN 'AA'.
*                    " simple element
*                    rtti = create_simple_element( type = dd-type leng = dd-leng decs = dd-decs ).
*                  WHEN 'AB'.
*                    " begin of structure
*                    ASSERT 1 = 1.
*                  WHEN 'AC'.
*                    " end of structure
*                    rtti = cl_abap_structdescr=>get(
*                        p_components = VALUE #(
*                            FOR rtti2 IN rtti_s INDEX INTO j2
*                            ( name       = |CMP{ j2 WIDTH = 4 ALIGN = RIGHT PAD = '0' }|
*                              type       = rtti2 ) )
*                        p_strict     = abap_false ). "cx_sy_struct_creation
*                  WHEN 'AD'.
*                    " begin of table
*                    ASSERT 1 = 1.
*                  WHEN 'AE'.
*                    " end of table
*                    " NB: the table should have only one component
*                    CASE dd-type.
*                      WHEN c_ityp-struct1 OR c_ityp-struct2.
*                        rtti = cl_abap_structdescr=>get(
*                                    p_components = VALUE #(
*                                        FOR rtti2 IN rtti_s INDEX INTO j2
*                                        ( name       = |CMP{ j2 WIDTH = 4 ALIGN = RIGHT PAD = '0' }|
*                                          type       = rtti2 ) )
*                                    p_strict     = abap_false ). "cx_sy_struct_creation
*                      WHEN OTHERS.
*                        rtti = rtti_s[ 1 ].
*                    ENDCASE.
*                    rtti = cl_abap_tabledescr=>get( p_line_type = rtti ). "cx_sy_table_creation
*                  WHEN 'AF'.
*                    " alignment bytes
*                ENDCASE.
*
*                APPEND dd TO dds.
*
*                IF dd-id <> 'AF'.
*                  IF rtti IS BOUND.
*                    " end of something
*                    IF rtti IS NOT INSTANCE OF cl_abap_elemdescr.
*                      " structure or table -> POP stack
*                      READ TABLE stack INDEX lines( stack ) INTO rtti_s.
*                      DELETE stack INDEX lines( stack ).
*                    ENDIF.
*                    APPEND rtti TO rtti_s.
*                  ELSE.
*                    APPEND rtti_s TO stack.
*                    rtti_s = VALUE #( ).
*                  ENDIF.
*                ENDIF.
*
*              ELSE.
*
*                IF rtti IS NOT BOUND.
*                  " No DD block -> Use data at Object Header
*                  CASE version.
*                    WHEN '06'.
*                      dd = VALUE #(
*                          id   = 'AA'
*                          type = object_header_uni-ityp
*                          leng = object_header_uni-leng
*                          decs = object_header_uni-decs ).
*                    WHEN OTHERS.
*                      dd = VALUE #(
*                          id   = 'AA'
*                          type = object_header-ityp
*                          leng = object_header-leng ).
*                  ENDCASE.
*                  dds = VALUE #( ( dd ) ).
*                  rtti = create_simple_element( type = dd-type leng = dd-leng decs = dd-decs ).
*                ENDIF.
*
*                TYPES : BEGIN OF ty_dv,
*                          dref     TYPE REF TO data,
*                          rtti     TYPE REF TO cl_abap_datadescr,
*                          dd_pos   TYPE i,
*                          off      TYPE i,
*                          comp_num TYPE i,
*                        END OF ty_dv,
*                        ty_dvs TYPE STANDARD TABLE OF ty_dv WITH EMPTY KEY.
*
*                DATA(dv) = VALUE ty_dv( rtti = rtti ).
*                DATA(dvs) = VALUE ty_dvs( ).
*
*                CREATE DATA dv-dref TYPE HANDLE rtti.
*                ASSIGN dv-dref->* TO FIELD-SYMBOL(<dv_field>).
*
*                partab = VALUE #(
*                    BASE partab
*                    ( name = object_name
*                      dref = dref ) ).
*
*                block_type = 'DV'.
*
*              ENDIF.
*
*
*
*
*            WHEN 'DV'.
*              "=====================
*              "  Data Value
*              "=====================
*              read_data_value( EXPORTING xdds = xdds IMPORTING dv = <dv_field> ).
*
*          ENDCASE.
*
*
*
*
*
*          IF rtti IS INSTANCE OF cl_abap_tabledescr
*              AND dv-off = dd-leng.
*            " Will not work with sorted and hashed internal tables,
*            " as the key components cannot be changed.
**                ASSIGN <fs> TO <table>.
**                INSERT INITIAL LINE INTO TABLE <table> ASSIGNING <fs>.
*            INSERT <fs> INTO TABLE <table>.
*            rtti ?= cl_abap_typedescr=>describe_by_data( <fs> ).
*            dv-comp_num = 0.
*            dv-dd_pos = 1.
*          ENDIF.
*
**      ENDCASE.
*
*        ENDWHILE.
*
*      CATCH cx_root INTO DATA(lx).
*        RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING previous = lx.
*    ENDTRY.

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


  METHOD read_data_object.

    ADD 1 TO current-dv-dd_index.
    CASE current-dd-lines[ current-dv-dd_index ]-id.
      WHEN 'AA'.
        read_data_primitive( EXPORTING xdds = xdds IMPORTING dv = dv off = off ).
      WHEN 'AB'.
        read_data_structure( EXPORTING xdds = xdds IMPORTING dv = dv off = off ).
      WHEN 'AD'.
        read_data_structure( EXPORTING xdds = xdds IMPORTING dv = dv off = off ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDCASE.

  ENDMETHOD.


  METHOD read_data_primitive.

    CASE current-dv-block-id.
      WHEN 'BB'.
        read_blob( IMPORTING data = dv ).
      WHEN 'BC'.
        IF current-dv-block-off = current-dv-block-len.
          IF blob+conv->position(1) <> 'BD'.
            RAISE EXCEPTION TYPE zcx_expimp_table.
          ENDIF.
          conv->skip_x( 1 ).
          read_data_value( EXPORTING xdds = xdds IMPORTING dv = dv off = off ).
        ENDIF.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDCASE.

  ENDMETHOD.


  METHOD read_data_structure.

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE dv TO FIELD-SYMBOL(<dv_component>).
      read_data_object( EXPORTING xdds = xdds IMPORTING dv = <dv_component> off = off ).
    ENDDO.

  ENDMETHOD.


  METHOD read_data_table.

  ENDMETHOD.


  METHOD read_data_value.

    DATA:
      dah_interval     TYPE ty_dah_interval,
      dah_interval_uni TYPE ty_dah_interval_uni.
    FIELD-SYMBOLS:
      <dah_interval>     TYPE ty_dah_interval,
      <dah_interval_uni> TYPE ty_dah_interval_uni.

    DATA(dd) = current-dd-lines[ 1 ].

*    IF current_dv_block IS INITIAL.

    current-dv-block-id = blob+conv->position(1).
    CASE current-dv-block-id.
      WHEN 'BB'.
        " Flat value
        current-dv-block-len = dd-leng.
        conv->skip_x( 1 ).
      WHEN 'BC'.
        " interval
        CASE version.
          WHEN '06'.
            CREATE DATA current-dv-block-dref TYPE ty_dah_interval_uni.
            ASSIGN current-dv-block-dref->* TO <dah_interval_uni>.
            read_blob( IMPORTING data = <dah_interval_uni> ).
*        IF dah_interval_uni-id <> 'BC'.
*          RAISE EXCEPTION TYPE zcx_expimp_table.
*        ENDIF.
            current-dv-block-len = dah_interval_uni-len.
          WHEN OTHERS.
            read_blob( IMPORTING data = dah_interval ).
            IF dah_interval-id <> 'BC'.
              RAISE EXCEPTION TYPE zcx_expimp_table.
            ENDIF.
            current-dv-block-len = dah_interval-len.
        ENDCASE.
      WHEN 'BE'.
      " begin of internal table
      WHEN 'CA'.
      "
      WHEN 'CC'.
      "
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDCASE.

*    ENDIF.
*    CASE current_dv_block-id.
*      WHEN 'BB'.
*        IF dd-id <> 'AA'.
*          RAISE EXCEPTION TYPE zcx_expimp_table.
*        ENDIF.
*        read_data_value_single( EXPORTING xdds = xdds IMPORTING dv = dv off = off ).
*      WHEN 'BC'.
*        IF dd-id <> 'AB'.
*          RAISE EXCEPTION TYPE zcx_expimp_table.
*        ENDIF.
*        read_data_value_interval( EXPORTING xdds = xdds IMPORTING dv = dv off = off ).
*      WHEN 'BE'.
*        IF dd-id <> 'AD'.
*          RAISE EXCEPTION TYPE zcx_expimp_table.
*        ENDIF.
*        read_data_value_table( EXPORTING xdds = xdds IMPORTING dv = dv off = off ).
*      WHEN 'CA'.
*        CASE dd-id.
*          WHEN 'AA' OR ''.
*            read_data_value_string_xstring( EXPORTING xdds = xdds IMPORTING dv = dv off = off ).
*          WHEN OTHERS.
*            RAISE EXCEPTION TYPE zcx_expimp_table.
*        ENDCASE.
*      WHEN 'CC'.
*        read_data_value_boxed( EXPORTING xdds = xdds IMPORTING dv = dv off = off ).
*      WHEN OTHERS.
*        RAISE EXCEPTION TYPE zcx_expimp_table.
*    ENDCASE.

  ENDMETHOD.


  METHOD read_data_value_single.

    "---------------------
    " single value - no Data Area Header
    "---------------------
    " It's the value of the whole field or structure (no filler)

*    DATA: len TYPE i.
*    len = dd-leng.

    read_blob( IMPORTING data = dv ).

    off = off.

*    IF lines( dvs ) = 0.
*      block_type = 'OH'.
*    ENDIF.

  ENDMETHOD.


  METHOD read_data_value_interval.

    "---------------------
    " starts with BC and ends with BD
    "---------------------

    DATA:
      dah_interval     TYPE ty_dah_interval,
      dah_interval_uni TYPE ty_dah_interval_uni,
      len              TYPE i,
      xstring          TYPE xstring.

    CASE version.
      WHEN '06'.
        read_blob( IMPORTING data = dah_interval_uni ).
        IF dah_interval_uni-id <> 'BC'.
          RAISE EXCEPTION TYPE zcx_expimp_table.
        ENDIF.
        len = dah_interval_uni-len.
      WHEN OTHERS.
        read_blob( IMPORTING data = dah_interval ).
        IF dah_interval-id <> 'BC'.
          RAISE EXCEPTION TYPE zcx_expimp_table.
        ENDIF.
        len = dah_interval-len.
    ENDCASE.

*data(Dd_indexes) = get_dd_indexes( dd len ).

    DATA(rtti) = cl_abap_typedescr=>describe_by_data( dv ).

*    LOOP AT dds INTO DATA(dd) FROM dd_index.
*
*      ADD 1 TO dd_index.
*
*      ASSIGN COMPONENT dv-comp_num OF STRUCTURE dv TO FIELD-SYMBOL(<dv_field>).
*
*      conv->read( EXPORTING n = dd-leng IMPORTING data = xstring ).
*
*      read_data_value( EXPORTING xstring = xstring IMPORTING dv = dv off = off2 ).
*
*    ENDLOOP.
*
*    IF blob+conv->position(1) <> 'BD'. " end of interval
*      RAISE EXCEPTION TYPE zcx_expimp_table.
*    ENDIF.
*
*    conv->skip_x( 1 ).
*
**    read_data_value( IMPORTING dv = dv off = off2 ).
*
*
*
*    WHILE dv-dd_pos < lines( dds ) AND dv-off <= 1.
*
*      dv-dd_pos = dv-dd_pos + 1.
*      dd = dds[ dv-dd_pos ].
*
*      CASE TYPE OF rtti.
*        WHEN TYPE cl_abap_structdescr.
*          dv-comp_num = dv-comp_num + 1.
*          ASSIGN COMPONENT dv-comp_num OF STRUCTURE <dv_field> TO <field>.
*          IF sy-subrc <> 0.
*            RAISE EXCEPTION TYPE zcx_expimp_table.
*          ENDIF.
*        WHEN OTHERS.
*          ASSIGN <fs> TO <field>.
*      ENDCASE.
*      " dv_field = xstring+dv_off(dd-leng).
*
*      CASE dd-type.
*        WHEN c_ityp-char
*            OR c_ityp-date
*            OR c_ityp-num
*            OR c_ityp-time
*            OR c_ityp-string.
*          conv->read( IMPORTING data  = <field> ).
*        WHEN c_ityp-hex
*            OR c_ityp-packed
*            OR c_ityp-xstring.
*          "<field> = dv_field.
*        WHEN c_ityp-decfloat16
*            OR c_ityp-decfloat34
*            OR c_ityp-int
*            OR c_ityp-int1
*            OR c_ityp-int2
*            OR c_ityp-int8
*            OR c_ityp-float.
*          conv->read( IMPORTING data  = <field> ).
*        WHEN OTHERS.
*          RAISE EXCEPTION TYPE zcx_expimp_table.
*      ENDCASE.
*      dv-off = dv-off + dd-leng.
*
*      CASE dd-id.
*        WHEN 'AB'.
*          " begin of structure
*        WHEN 'AC'.
*          " end of structure
*          "READ TABLE drefs INDEX lines( drefs ) INTO dref.
*          "DELETE drefs INDEX lines( drefs ).
*          "ASSIGN dref->* TO <fs>.
*        WHEN 'AF'.
*          " Filler
*          dv-off = dv-off + dd-leng.
*      ENDCASE.
*
*      dv-dd_pos = dv-dd_pos + 1.
*    ENDWHILE.
*
*
**                WHEN 'BD'   " end of interval
**                  "---------------------
**                  " end of something
**                  "---------------------
**                  conv->skip_x( 1 ).
**
**                  IF lines( dvs ) = 0.
**                    RAISE EXCEPTION TYPE zcx_expimp_table.
**                  ENDIF.
**                  dv = dvs[ lines( dvs ) ].
**                  ASSIGN dv-dref->* TO <dv_field>.
**                  DELETE dvs INDEX lines( dvs ).
**                  IF lines( dvs ) = 0.
**                    block_type = 'OH'.
**                  ENDIF.

  ENDMETHOD.


  METHOD read_data_value_table.

    "---------------------
    " starts with BE and ends with BF
    "---------------------

    DATA:
      dah_table     TYPE ty_data_table_descr,
      dah_table_uni TYPE ty_data_table_descr_uni,
      dref          TYPE REF TO data.

    " read width and number of lines of the internal table
    CASE version.
      WHEN '06'.
        read_blob( IMPORTING data = dah_table_uni ).
        IF dah_table_uni-id <> 'BE'.
          RAISE EXCEPTION TYPE zcx_expimp_table.
        ENDIF.
      WHEN OTHERS.
        read_blob( IMPORTING data = dah_table ).
        IF dah_table-id <> 'BE'.
          RAISE EXCEPTION TYPE zcx_expimp_table.
        ENDIF.
    ENDCASE.

    " Create a work area for containing each line of the table
    FIELD-SYMBOLS <dv_table> TYPE ANY TABLE.
    ASSIGN dv TO <dv_table>.
    CREATE DATA dref LIKE LINE OF <dv_table>.
    ASSIGN dref->* TO FIELD-SYMBOL(<dv_line>).
    DATA(rtti) = cl_abap_typedescr=>describe_by_data( <dv_line> ).

    DATA(len_line) = 0.

    DO.

*      LOOP AT dds INTO DATA(dd) FROM from.
*        DATA(dd_tabix) = sy-tabix.
*
*        IF blob+conv->position(1) = 'BF'. " end of table
*          IF len_line <> 0.
*            RAISE EXCEPTION TYPE zcx_expimp_table.
*          ENDIF.
*          EXIT.
*        ENDIF.
*
*        read_data_value(
*            EXPORTING
*                dds  = dds
**                from = dd_tabix
*            IMPORTING
*                dv   = <dv_line>
*                off  = DATA(off2) ).
*
*        len_line = len_line + off2.
*
*        IF ( version = '06' AND len_line = dah_table_uni-len )
*        OR ( version <> '06' AND len_line = dah_table-len ).
*          INSERT <dv_line> INTO TABLE <dv_table>.
*          len_line = 0.
*          CLEAR <dv_line>.
*        ENDIF.
*
*      ENDLOOP.
*
*      IF blob+conv->position(1) = 'BF'. " end of table
*        conv->skip_x( 1 ).
*        EXIT.
*      ENDIF.

    ENDDO.


    " an internal table occupies 8 bytes
    off = 8.

  ENDMETHOD.


  METHOD read_data_value_string_xstring.

    "---------------------
    " starts with CA and ends with CB
    "---------------------

    DATA: dah_string_xstring TYPE ty_data_string_xstring.

    read_blob( IMPORTING data = dah_string_xstring ).

    IF dah_string_xstring-id <> 'CA'.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.

    conv->read( EXPORTING n = CONV #( dah_string_xstring-len ) IMPORTING data = dv ).

    IF blob+conv->position(1) = 'CB'.
      conv->skip_x( 1 ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.

    off = 8.

  ENDMETHOD.


  METHOD read_data_value_boxed.

    "---------------------
    " starts with CC and ends with CD
    "---------------------

    " TODO
    RAISE EXCEPTION TYPE zcx_expimp_table.

*                    OR 'CD'." end of boxed component
*                  "---------------------
*                  " end of something
*                  "---------------------
*                  conv->skip_x( 1 ).
*
*                  IF lines( dvs ) = 0.
*                    RAISE EXCEPTION TYPE zcx_expimp_table.
*                  ENDIF.
*                  dv = dvs[ lines( dvs ) ].
*                  ASSIGN dv-dref->* TO <dv_field>.
*                  DELETE dvs INDEX lines( dvs ).
*                  IF lines( dvs ) = 0.
*                    block_type = 'OH'.
*                  ENDIF.

  ENDMETHOD.


  METHOD process_object.

    TYPES : BEGIN OF ty_dv,
              dref     TYPE REF TO data,
              rtti     TYPE REF TO cl_abap_datadescr,
              dd_pos   TYPE i,
              off      TYPE i,
              comp_num TYPE i,
            END OF ty_dv,
            ty_dvs TYPE STANDARD TABLE OF ty_dv WITH EMPTY KEY.
    DATA: object_header        TYPE ty_object_header.


    current-dd-lines = VALUE #( ).
*    rtti_s = VALUE #( ).
*    object_id = blob+conv->position(1).
*    rtti ?= null_object.


    CASE version.
      WHEN '06'.
        read_blob( IMPORTING data = current-object-header_uni ).
      WHEN OTHERS.
        read_blob( IMPORTING data = object_header ).
        current-object-header_uni = VALUE #(
            BASE object_header
            leng = CONV i( object_header-leng )
            next = CONV i( object_header-next )
            nlen = CONV i( object_header-nlen ) ).
    ENDCASE.
    conv->read(
        EXPORTING n = CONV #( current-object-header_uni-nlen )
        IMPORTING data = current-object-name ).
*              "======================
*              " Field Name
*              "======================
*              " NLEN contains the number of characters encoded with
*              " TRANSPORT_HEADER-CODEPAGE (one character occupies
*              " between 1 and 4 bytes).
**              CASE version.
**                WHEN '06'.
**                  DATA nlen TYPE i.
**                  nlen = object_header_uni-nlen.
**                WHEN OTHERS.
**                  nlen = object_header-nlen.
**              ENDCASE.
*              DATA(object_name) = ||.
**              dd = VALUE #( ).
*              dds = VALUE #( ).
**              block_type = 'DD'.
*    process_object_name( ).

    DO.

      IF blob+conv->position(1) NOT BETWEEN 'A0' AND 'AF'.
        " End of Data description
        " (note that data description is optional)
        EXIT.
      ENDIF.

      process_data_description( ).

    ENDDO.


    IF current-dd-rtti IS NOT BOUND.
      " No DD block -> Use data at Object Header
      create_default_description( ).
    ENDIF.

    DATA(dv) = VALUE ty_dv( rtti = current-dd-rtti ).
    CREATE DATA dv-dref TYPE HANDLE current-dd-rtti.
    ASSIGN dv-dref->* TO FIELD-SYMBOL(<dv_field>).

    partab = VALUE #(
        BASE partab
        ( name = current-object-name
          dref = dv-dref ) ).

    "=====================
    "  Data Value
    "=====================
    DATA(dvs) = VALUE ty_dvs( ).

    current-dv-block-id = blob+conv->position(1).

    "process( EXPORTING xdds = current-dd-lines IMPORTING dv = <dv_field> ).
    read_data_object( EXPORTING xdds = current-dd-lines IMPORTING dv = <dv_field> ).

  ENDMETHOD.


  METHOD process_data_description.

    DATA: data_description     TYPE ty_data_description,
          data_description_uni TYPE ty_data_description_uni,
          dd                   TYPE ty_dd.


    IF blob+conv->position(1) NOT BETWEEN 'A0' AND 'AF'.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.

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
        current-dd-rtti = create_simple_element( type = dd-type leng = dd-leng decs = dd-decs ).
      WHEN 'AB'.
        " begin of structure
        ASSERT 1 = 1.
      WHEN 'AC'.
        " end of structure
        current-dd-rtti = cl_abap_structdescr=>get(
            p_components = VALUE #(
                FOR rtti2 IN current-dd-rtti_s INDEX INTO j2
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
            current-dd-rtti = cl_abap_structdescr=>get(
                        p_components = VALUE #(
                            FOR rtti2 IN current-dd-rtti_s INDEX INTO j2
                            ( name       = |CMP{ j2 WIDTH = 4 ALIGN = RIGHT PAD = '0' }|
                              type       = rtti2 ) )
                        p_strict     = abap_false ).
            "cx_sy_struct_creation
          WHEN OTHERS.
            current-dd-rtti = current-dd-rtti_s[ 1 ].
        ENDCASE.
        current-dd-rtti = cl_abap_tabledescr=>get( p_line_type = current-dd-rtti ).
        "cx_sy_table_creation
      WHEN 'AF'.
        " alignment bytes (FILLER)
    ENDCASE.

    APPEND dd TO current-dd-lines.

    IF dd-id <> 'AF'. " alignment bytes (FILLER)
      IF current-dd-rtti IS BOUND.
        " end of something
        IF current-dd-rtti IS NOT INSTANCE OF cl_abap_elemdescr.
          " structure or table -> POP stack
          current-dd-rtti_s = current-dd-stack_of_rtti_s[ lines( current-dd-stack_of_rtti_s ) ].
          DELETE current-dd-stack_of_rtti_s INDEX lines( current-dd-stack_of_rtti_s ).
        ENDIF.
        APPEND current-dd-rtti TO current-dd-rtti_s.
      ELSE.
        APPEND current-dd-rtti_s TO current-dd-stack_of_rtti_s.
        current-dd-rtti_s = VALUE #( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD create_default_description.

    " No DD block -> Use data at Object Header

    current-dd-lines = VALUE #(
        ( id   = 'AA'
          type = current-object-header_uni-ityp
          leng = current-object-header_uni-leng
          decs = current-object-header_uni-decs ) ).

    current-dd-rtti = create_simple_element(
        type = current-object-header_uni-ityp
        leng = CONV #( current-object-header_uni-leng )
        decs = CONV #( current-object-header_uni-decs ) ).

  ENDMETHOD.


ENDCLASS.
