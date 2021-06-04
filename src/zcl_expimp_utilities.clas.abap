"! <p class="shorttext synchronized" lang="en">Utilities</p>
"! TODO:<ul>
"! <li></li>
"! </ul>
CLASS zcl_expimp_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    "! Method like CL_ABAP_EXPIMP_UTILITIES=>DBUF_IMPORT_CREATE_DATA
    "! (which does not support all kinds of data buffers).
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter DBUF | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter PARTAB | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_expimp_table | <p class="shorttext synchronized" lang="en"></p>
    METHODS dbuf_import_create_data
      CHANGING
        dbuf          TYPE xstring
      RETURNING
        VALUE(partab) TYPE tab_cpar
      RAISING
        zcx_expimp_table.

    CLASS-METHODS dbuf_export
      IMPORTING
        tab_cpar    TYPE tab_cpar
        compression TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(dbuf) TYPE xstring
      RAISING
        zcx_expimp_table.

    CLASS-METHODS dbuf_import
      IMPORTING
        dbuf     TYPE xstring
        tab_cpar TYPE tab_cpar
      RAISING
        zcx_expimp_table.

    "! Version <ul>
    "! <li>06: >= 6.10 UNICODE - Lengths are up to 4 bytes - 2 bytes per character?</li>
    "! <li>05: >= 3.0G and < 6.10 ??? - Lengths are on 2 bytes - 1 byte per character?</li>
    "! <li>04: >= 3.0G and < 6.10 ???</li>
    "! <li>03: <= 3.0F ???</li>
    "! <li>02: <= 3.0F ???</li>
    "! <li>01: <= 3.0F ???</li>
    "! </ul>
    "! Miscellaneous <ul>
    "! <li>ABAP release notes: In Release 3.x, the ABAP types 1 and 2 were still supported in some areas in a very basic manner to retain R/2 compatibility. This is no longer the case in Release 4.0</li>
    "! <li>note 178482: The data types TYP1 and TYP2 are no longer allowed for data specifications. Instead, you must use the type D. Otherwise, a syntax error occurs.</li>
    "! </ul>
    TYPES ty_version TYPE x LENGTH 1.
    CONSTANTS: BEGIN OF c_version,
                 v06 TYPE ty_version VALUE '06',
                 v05 TYPE ty_version VALUE '05',
                 v04 TYPE ty_version VALUE '04',
                 v03 TYPE ty_version VALUE '03',
                 v02 TYPE ty_version VALUE '02',
                 v01 TYPE ty_version VALUE '01',
               END OF c_version.
    TYPES: BEGIN OF ty_version_and_reader,
             version TYPE ty_version,
             reader  TYPE REF TO cl_abap_conv_in_ce,
           END OF ty_version_and_reader.
    METHODS get_version_and_reader
      IMPORTING
        dbuf          TYPE xstring
      RETURNING
        VALUE(result) TYPE ty_version_and_reader
      RAISING
        cx_sy_compression_error
        zcx_expimp_table
        cx_parameter_invalid_range
        cx_sy_codepage_converter_init
        cx_sy_conversion_codepage
        cx_parameter_invalid_type.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
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
      END OF ty_pdat_level1,
      BEGIN OF ty_ptab_line,
        paramname TYPE string,
        varname   TYPE c LENGTH 255,
      END OF ty_ptab_line,
      ty_ptab TYPE STANDARD TABLE OF ty_ptab_line WITH EMPTY KEY,
      BEGIN OF ty_ptab_group,
        p    TYPE ty_pdat_level1,
        ptab TYPE ty_ptab,
      END OF ty_ptab_group.

    "! Simplifies the use of EXPORT (ptab) and IMPORT (ptab), to handle any number
    "! of data objects (up to 1000)
    CLASS-METHODS tab_cpar_to_ptab
      IMPORTING
        varname           TYPE clike
        tab_cpar          TYPE tab_cpar
      RETURNING
        VALUE(ptab_group) TYPE ty_ptab_group
      RAISING
        zcx_expimp_table.

    "==========================================================
    "
    "             code below taken from RSINDX00
    "
    "==========================================================

***** IMPLEMENTATION OF C-STRUCTURES FROM ABCONNE.C

* (1) Transport header

    "! <ul>
    "! <li>01: BIG ENDIAN</li>
    "! <li>02: LITTLE ENDIAN</li>
    "! </ul>
    TYPES ty_intformat TYPE x LENGTH 1.
    CONSTANTS: BEGIN OF c_intformat,
                 big_endian    TYPE ty_intformat VALUE '01',
                 little_endian TYPE ty_intformat VALUE '02',
               END OF c_intformat.

    "! <ul>
    "! <li>01: BIG ENDIAN</li>
    "! <li>02: LITTLE ENDIAN</li>
    "! </ul>
    TYPES ty_floatformat TYPE x LENGTH 1.
    CONSTANTS: BEGIN OF c_floatformat,
                 big_endian    TYPE ty_floatformat VALUE '01',
                 little_endian TYPE ty_floatformat VALUE '02',
               END OF c_floatformat.

    "! <ul>
    "! <li>01: not compressed</li>
    "! <li>02: compressed</li>
    "! </ul>
    TYPES ty_compress TYPE x LENGTH 1.
    CONSTANTS: BEGIN OF c_compress,
                 not_compressed TYPE ty_compress VALUE '01',
                 compressed     TYPE ty_compress VALUE '02',
               END OF c_compress.

    TYPES: ty_byte TYPE x LENGTH 1,
           x1      TYPE x LENGTH 1,

           "! Header (16 bytes)
           BEGIN OF ty_transport_header,
             "! FF
             id              TYPE x1,
             "! Version
             version         TYPE x1,
             "! Integer format
             intformat       TYPE ty_intformat,
             "! Float format
             floatformat     TYPE ty_floatformat,
             "! Compress
             compress        TYPE ty_compress,
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
    "! <li>0C: typ1?</li>
    "! <li>0D: typ2?</li>
    "! <li>0E: u (flat structure)</li>
    "! <li>0F: v (deep structure)</li>
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
                 u1         TYPE ty_ityp VALUE '0C', " type 1 ?
                 u2         TYPE ty_ityp VALUE '0D', " type 2 ?
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
                 unknown1   TYPE ty_ityp VALUE '19',
                 unknown2   TYPE ty_ityp VALUE '1A',
                 int8       TYPE ty_ityp VALUE '1B',
               END OF c_ityp.

    "! <ul>
    "! <li>01 : primitive</li>
    "! <li>02 : flat structure</li>
    "! <li>03 : flat table</li>
    "! <li>04 : end of data</li>
    "! <li>05 : deep structure</li>
    "! <li>06 : deep table</li>
    "! <li>07 : string</li>
    "! </ul>
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


    TYPES:
      "! Object header
      BEGIN OF ty_object_header,
        "! Object header before release 3.1 (versions 01 to 03) - 7 bytes
        BEGIN OF v01_03,
          "! Field type category
          id   TYPE ty_object_id,
          "! Field type
          ityp TYPE ty_ityp,
          "! <p>Field length in number of bytes</p>
          "! <p>For CLIKE types, how to know if one character is one or two bytes? Let's say version 06 means 2 bytes per character</p>
          "! <p>For deep types, it's 8 bytes (STRING, XSTRING, internal table, boxed)</p>
          leng TYPE x LENGTH 2,
          "! Length of the whole Object block including the header, useful to reach directly the next object; or zero if it's the last object
          next TYPE x LENGTH 2,
          "! Length of object name in number of characters
          nlen TYPE x1,
        END OF v01_03,
        "! Object header for release 3.1 to 4.6 (versions 04 and 05) - 15 bytes
        BEGIN OF v04_05,
          "! Field type category
          id    TYPE ty_object_id,
          "! Field type
          ityp  TYPE ty_ityp,
          "! Field length in number of bytes
          leng  TYPE x LENGTH 2,
          "! Length of the whole Object block including the header, useful to reach directly the next object; or zero if it's the last object
          next  TYPE x LENGTH 2,
          "! Length of object name in number of characters
          nlen  TYPE x1,
          "! Type identifier (check info)
          typid TYPE x LENGTH 8,
        END OF v04_05,
        "! Object header for release 6.10 and higher (UNICODE) (version 06) - 32 bytes
        BEGIN OF uni,
          "! Field type category
          id    TYPE ty_object_id,
          "! Field type
          ityp  TYPE ty_ityp,
          "! Decimals for type P
          decs  TYPE x1,
          "! Field length in number of bytes
          leng  TYPE x LENGTH 4,
          "! Length of the whole Object block including the header, useful to reach directly the next object; or zero if it's the last object
          next  TYPE x LENGTH 4,
          "! Length of object name in number of characters
          nlen  TYPE x1,
          "! Hash key for UID
          thash TYPE x LENGTH 4,
          "! Type identifier (check info)
          typid TYPE x LENGTH 16,
        END OF uni,
      END OF ty_object_header.

    TYPES:
      "! ID of data description
      "! <ul>
      "! <li>A0 : begin of structure include</li>
      "! <li>A1 : end of structure include</li>
      "! <li>A2 : begin of boxed component</li>
      "! <li>A3 : end of boxed component</li>
      "! <li>AA : primitive field</li>
      "! <li>AB : begin of structure</li>
      "! <li>AC : end of structure</li>
      "! <li>AD : begin of table</li>
      "! <li>AE : end of table</li>
      "! <li>AF : filler</li>
      "! </ul>
      ty_dd_id TYPE x1,
      "! data description
      BEGIN OF ty_data_description,
        BEGIN OF v00_46,
          id   TYPE ty_dd_id,
          type TYPE x1,
          flen TYPE x LENGTH 2,
        END OF v00_46,
        BEGIN OF uni,
          id   TYPE ty_dd_id,
          type TYPE x1,
          decs TYPE x1,
          flen TYPE x LENGTH 4,
        END OF uni,
      END OF ty_data_description.

    CONSTANTS:
      BEGIN OF c_dd_id,
        BEGIN OF structure_include,
          start TYPE ty_dd_id VALUE 'A0',
          end   TYPE ty_dd_id VALUE 'A1',
        END OF structure_include,
        BEGIN OF boxed_component,
          start TYPE ty_dd_id VALUE 'A2',
          end   TYPE ty_dd_id VALUE 'A3',
        END OF boxed_component,
        primitive TYPE ty_dd_id VALUE 'AA',
        BEGIN OF structure,
          start TYPE ty_dd_id VALUE 'AB',
          end   TYPE ty_dd_id VALUE 'AC',
        END OF structure,
        BEGIN OF table,
          start TYPE ty_dd_id VALUE 'AD',
          end   TYPE ty_dd_id VALUE 'AE',
        END OF table,
        filler    TYPE ty_dd_id VALUE 'AF',
      END OF c_dd_id.

    TYPES:
      "! ID of data value
      "! <ul>
      "! <li>A0 : begin of structure include</li>
      "! <li>A1 : end of structure include</li>
      "! <li>A2 : begin of boxed component</li>
      "! <li>A3 : end of boxed component</li>
      "! <li>AA : primitive field</li>
      "! <li>AB : begin of structure</li>
      "! <li>AC : end of structure</li>
      "! <li>AD : begin of table</li>
      "! <li>AE : end of table</li>
      "! <li>AF : filler</li>
      "! </ul>
      ty_dv_id TYPE x1,
      "! data area headers
      BEGIN OF ty_dah,

        BEGIN OF interval,
          BEGIN OF v00_46,
            id  TYPE ty_dv_id,
            len TYPE x LENGTH 2,
          END OF v00_46,
          BEGIN OF uni,
            id  TYPE ty_dv_id,
            len TYPE x LENGTH 4,
          END OF uni,
        END OF interval,

        BEGIN OF table,
          BEGIN OF v00_46,
            id  TYPE ty_dv_id,
            len TYPE x LENGTH 2,
          END OF v00_46,
          BEGIN OF uni,
            id    TYPE ty_dv_id,
            len   TYPE x LENGTH 4,
            lines TYPE x LENGTH 4,
          END OF uni,
        END OF table,

        BEGIN OF string_xstring,
          id  TYPE ty_dv_id,
          len TYPE x LENGTH 4,
        END OF string_xstring,

      END OF ty_dah.

    CONSTANTS:
      BEGIN OF c_dv_id,
        "! Whole length of Data Description
        single TYPE ty_dd_id VALUE 'BB',
        BEGIN OF interval,
          "! Spans several fields
          start TYPE ty_dd_id VALUE 'BC',
          end   TYPE ty_dd_id VALUE 'BD',
        END OF interval,
        BEGIN OF table,
          start TYPE ty_dd_id VALUE 'BE',
          end   TYPE ty_dd_id VALUE 'BF',
        END OF table,
        BEGIN OF string_xstring,
          start TYPE ty_dd_id VALUE 'CA',
          end   TYPE ty_dd_id VALUE 'CB',
        END OF string_xstring,
        BEGIN OF boxed_component,
          start TYPE ty_dd_id VALUE 'CC',
          end   TYPE ty_dd_id VALUE 'CD',
        END OF boxed_component,
      END OF c_dv_id.

    TYPES:
      BEGIN OF ty_dd,
        id   TYPE ty_dd_id,
        "! Internal type
        type TYPE x1,
        decs TYPE i,
        "! Number of bytes
        leng TYPE i,
      END OF ty_dd,
      ty_dds TYPE STANDARD TABLE OF ty_dd WITH EMPTY KEY,
      BEGIN OF ty_dd2,
        id         TYPE ty_dd_id,
        "! Internal type
        type       TYPE ty_ityp,
        decs       TYPE i,
        "! Number of bytes
        leng       TYPE i,
        components TYPE REF TO data, "abap_component_tab,
      END OF ty_dd2,
      ty_dd2_lines TYPE STANDARD TABLE OF ty_dd2 WITH EMPTY KEY,
      BEGIN OF ty_dv_stack_line,
        id  TYPE ty_dv_id,
        off TYPE i,
        len TYPE i,
      END OF ty_dv_stack_line,
      ty_dv_stack TYPE STANDARD TABLE OF ty_dv_stack_line WITH EMPTY KEY.


    METHODS create_data_primitive
      IMPORTING
        type        TYPE x1
        leng        TYPE i
        decs        TYPE i
      RETURNING
        VALUE(rtti) TYPE REF TO cl_abap_datadescr
      RAISING
        zcx_expimp_table.

    "! If the 5th byte = '02', the BLOB must be uncompressed by kernel AB_IMPORT_DECOMPRESS
    CLASS-METHODS normalize_blob
      CHANGING
               blob TYPE xstring
      RAISING  cx_sy_compression_error
               zcx_expimp_table.

    METHODS read_blob_dv
      EXPORTING
        data TYPE any.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter data | <p class="shorttext synchronized" lang="en">Structure</p>
    "! @parameter length | <p class="shorttext synchronized" lang="en"></p>
    METHODS read_blob_struct
      EXPORTING
        data   TYPE any
        length TYPE i.

    METHODS read_data_object2
      IMPORTING
        dd2 TYPE ty_dd2
      EXPORTING
        dv  TYPE any
        off TYPE i
      RAISING
        zcx_expimp_table.

    METHODS read_data_primitive
      IMPORTING
        dd2 TYPE ty_dd2
      EXPORTING
        dv  TYPE any
        off TYPE i
      RAISING
        zcx_expimp_table.

    METHODS read_data_structure
      IMPORTING
        dd2_lines TYPE ty_dd2_lines
      EXPORTING
        dv        TYPE any
        off       TYPE i
      RAISING
        zcx_expimp_table.

    METHODS read_data_table
      IMPORTING
        dd2_lines TYPE ty_dd2_lines
        dd_index  TYPE i
      EXPORTING
        dv        TYPE ANY TABLE
      RAISING
        zcx_expimp_table.

    METHODS process_data_object
      RAISING
        zcx_expimp_table.

    METHODS process_data_description
      RAISING
        zcx_expimp_table.

    METHODS process_data_description_line
      RAISING
        zcx_expimp_table.

    METHODS create_default_description
      RAISING
        zcx_expimp_table.

    METHODS normalize_data_description
      IMPORTING
        up_to  TYPE ty_dd_id OPTIONAL
      EXPORTING
        lines2 TYPE ty_dd2_lines
      CHANGING
        lines  TYPE ty_dds
      RAISING
        zcx_expimp_table.

    METHODS normalize_data_description_v3
      IMPORTING
        up_to                   TYPE ty_dd_id OPTIONAL
        VALUE(alignment_offset) TYPE i DEFAULT 0
      EXPORTING
        lines2                  TYPE ty_dd2_lines
      CHANGING
        lines                   TYPE ty_dds
      RAISING
        zcx_expimp_table.

    METHODS normalize_data_description_v6
      IMPORTING
        up_to  TYPE ty_dd_id OPTIONAL
      EXPORTING
        lines2 TYPE ty_dd2_lines
      CHANGING
        lines  TYPE ty_dds
      RAISING
        zcx_expimp_table.

    METHODS normalize_data_description_end
      CHANGING
        lines2 TYPE ty_dd2_lines
      RAISING
        zcx_expimp_table.

    METHODS assert_blob_curr_byte_and_skip
      IMPORTING
        expected_byte TYPE ty_byte
      RAISING
        zcx_expimp_table.

    METHODS blob_curr_byte
      RETURNING
        VALUE(byte) TYPE ty_byte
      RAISING
        zcx_expimp_table.

    METHODS create_data_object
      IMPORTING
        dd2         TYPE ty_dd2
      RETURNING
        VALUE(rtti) TYPE REF TO cl_abap_typedescr
      RAISING
        zcx_expimp_table.

    METHODS create_data_structure
      IMPORTING
        dd2         TYPE ty_dd2
      RETURNING
        VALUE(rtti) TYPE REF TO cl_abap_typedescr
      RAISING
        zcx_expimp_table.
    METHODS create_data_table
      IMPORTING
        dd2         TYPE ty_dd2
      RETURNING
        VALUE(rtti) TYPE REF TO cl_abap_typedescr
      RAISING
        zcx_expimp_table.

    DATA:
      "! DBUF reader (which converts from bytes to characters if needed)
      conv    TYPE REF TO cl_abap_conv_in_ce,
      "! Version
      version TYPE ty_version,
      "! Return value of method DBUF_IMPORT_CREATE_DATA
      partab  TYPE tab_cpar,
      "! Temporary variables for retaining current element processed
      BEGIN OF current,
        BEGIN OF object,
          "! Current object header
          header_uni TYPE ty_object_header-uni,
          "! Current object name
          name       TYPE string,
        END OF object,
        BEGIN OF dd,
          "! Data Descriptions
          lines           TYPE ty_dds,
          "! Data Description normalized
          line2           TYPE ty_dd2,
          "! RTTI
          rtti            TYPE REF TO cl_abap_datadescr,
          rtti_s          TYPE TABLE OF REF TO cl_abap_datadescr,
          stack_of_rtti_s LIKE TABLE OF current-dd-rtti_s,
        END OF dd,
        BEGIN OF dv,
          "! Index current Data Description while parsing Data Values
          dd_line  TYPE ty_dd,
          dd_index TYPE i,
          id       TYPE ty_dv_id,
          dd_off   TYPE i,
          len      TYPE i,
        END OF dv,
      END OF current,
      "! Last byte read by method BLOB_CURR_BYTE
      curr_byte TYPE ty_byte.

    CONSTANTS null_object TYPE REF TO object VALUE IS INITIAL.

ENDCLASS.



CLASS zcl_expimp_utilities IMPLEMENTATION.


  METHOD assert_blob_curr_byte_and_skip.

    DATA: actual_byte TYPE ty_byte.

    TRY.

        conv->read( IMPORTING data = actual_byte ).

      CATCH cx_root INTO DATA(lx).
        RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING previous = lx.
    ENDTRY.

    IF actual_byte <> expected_byte.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.

  ENDMETHOD.


  METHOD blob_curr_byte.

    conv->read( IMPORTING data = curr_byte len = DATA(len) ).
    IF len = 0.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.
    conv->skip_x( -1 ).
    byte = curr_byte.

  ENDMETHOD.


  METHOD create_data_object.

*    DATA: data_description     TYPE ty_data_description-v00_46,
*          data_description_uni TYPE ty_data_description-uni,
*          dd                   TYPE ty_dd.


    CASE dd2-id.

      WHEN c_dd_id-structure_include-start.
        " TODO
        RAISE EXCEPTION TYPE zcx_expimp_table.

      WHEN c_dd_id-boxed_component-start.
        " TODO
        RAISE EXCEPTION TYPE zcx_expimp_table.

      WHEN c_dd_id-primitive.
        rtti = create_data_primitive( type = dd2-type leng = dd2-leng decs = dd2-decs ).

      WHEN c_dd_id-structure-start.
        rtti = create_data_structure( dd2 ).

      WHEN c_dd_id-table-start.
        " NB: the table should have only one component
        rtti = create_data_table( dd2 ).

      WHEN c_dd_id-structure-end
          OR c_dd_id-table-end
          OR c_dd_id-filler
      OR c_dd_id-boxed_component-end
      OR c_dd_id-structure_include-end.
        ASSERT 0 = 1.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDCASE.

*      APPEND dd TO current-dd-lines.
*
*      CASE dd-id.
*        WHEN c_dd_id-primitive.
*          " Classic field
*          APPEND current-dd-rtti TO current-dd-rtti_s.
*        WHEN c_dd_id-structure-end
*            OR c_dd_id-table-end.
*          " End of structure or table -> POP from stack
*          current-dd-rtti_s = current-dd-stack_of_rtti_s[ lines( current-dd-stack_of_rtti_s ) ].
*          DELETE current-dd-stack_of_rtti_s INDEX lines( current-dd-stack_of_rtti_s ).
*          APPEND current-dd-rtti TO current-dd-rtti_s.
*        WHEN c_dd_id-structure-start
*            OR c_dd_id-table-start.
*          " Begin of structure or table -> PUSH to stack
*          APPEND current-dd-rtti_s TO current-dd-stack_of_rtti_s.
*          current-dd-rtti_s = VALUE #( ).
*      ENDCASE.

  ENDMETHOD.


  METHOD create_data_primitive.

    TRY.
        CASE type.
          WHEN c_ityp-char.
            rtti = cl_abap_elemdescr=>get_c( SWITCH #( conv->encoding WHEN '4102' OR '4103' THEN leng / 2 ELSE leng ) ).
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
            rtti = cl_abap_elemdescr=>get_n( SWITCH #( conv->encoding WHEN '4102' OR '4103' THEN leng / 2 ELSE leng ) ).
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


  METHOD create_data_structure.

    DATA: table_of_rtti TYPE TABLE OF REF TO cl_abap_typedescr.
    FIELD-SYMBOLS <dd2_lines> TYPE ty_dd2_lines.

    ASSIGN dd2-components->* TO <dd2_lines>.

    LOOP AT <dd2_lines> REFERENCE INTO DATA(dd2_bis)
        WHERE id <> c_dd_id-filler.
      APPEND create_data_object( dd2_bis->* ) TO table_of_rtti.
    ENDLOOP.

    rtti = cl_abap_structdescr=>get(
                p_strict     = abap_false
                p_components = VALUE #(
                    FOR rtti2 IN table_of_rtti INDEX INTO j2
                        ( name = |CMP{ j2 WIDTH = 4 ALIGN = RIGHT PAD = '0' }|
                          type = CAST #( rtti2 ) ) ) ).

  ENDMETHOD.


  METHOD create_data_table.

    DATA: rtti_line_type TYPE REF TO cl_abap_datadescr.
    FIELD-SYMBOLS <dd2_lines> TYPE ty_dd2_lines.

    ASSIGN dd2-components->* TO <dd2_lines>.
    IF lines( <dd2_lines> ) <> 1.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.

    DATA(dd2_bis) = <dd2_lines>[ 1 ].

    IF dd2_bis-id = c_dd_id-structure-start.

      rtti_line_type ?= create_data_structure( dd2_bis ).

    ELSE.

      rtti_line_type = create_data_primitive( type = dd2_bis-type leng = dd2_bis-leng decs = dd2_bis-decs ).

    ENDIF.

*    CASE dd2_bis-type.
*      WHEN c_ityp-struct1 OR c_ityp-struct2.
*        " Either its lines are structured
*        rtti_line_type ?= create_data_structure( <dd2_lines> ).
*      WHEN OTHERS.
*        " Or its lines have an elementary type
*        IF lines( <dd2_lines> ) > 1.
*          RAISE EXCEPTION TYPE zcx_expimp_table.
*        ENDIF.
*        DATA(dd) = <dd2_lines>[ 1 ].
*        rtti_line_type = create_data_primitive( type = dd-type leng = dd-leng decs = dd-decs ).
*    ENDCASE.

    rtti = cl_abap_tabledescr=>get( p_line_type = rtti_line_type ).
    "cx_sy_table_creation

  ENDMETHOD.


  METHOD create_default_description.

    " No DD block -> Use information from Object Header

    current-dd-lines = VALUE #(
        ( id   = c_dd_id-primitive
          type = current-object-header_uni-ityp
          leng = current-object-header_uni-leng
          decs = current-object-header_uni-decs ) ).

    current-dd-rtti = create_data_primitive(
        type = current-object-header_uni-ityp
        leng = CONV #( current-object-header_uni-leng )
        decs = CONV #( current-object-header_uni-decs ) ).

  ENDMETHOD.


  METHOD dbuf_export.

    DATA:
      p TYPE ty_ptab_group.

    p = zcl_expimp_utilities=>tab_cpar_to_ptab( varname = 'P' tab_cpar = tab_cpar ).

    IMPORT (p-ptab) FROM DATA BUFFER dbuf.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.

  ENDMETHOD.


  METHOD dbuf_import.

    DATA:
      p TYPE ty_ptab_group.

    p = zcl_expimp_utilities=>tab_cpar_to_ptab( varname = 'P' tab_cpar = tab_cpar ).

    IMPORT (p-ptab) FROM DATA BUFFER dbuf.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.

  ENDMETHOD.


  METHOD get_version_and_reader.

    TYPES: ty_byte TYPE x LENGTH 1,
           x1      TYPE x LENGTH 1,

           "! Header (16 bytes)
           BEGIN OF ty_transport_header,
             "! FF
             id              TYPE x LENGTH 1,
             "! Version
             version         TYPE x LENGTH 1,
             "! Integer format
             intformat       TYPE x LENGTH 1,
             "! Float format
             floatformat     TYPE x LENGTH 1,
             "! Compress
             compress        TYPE x LENGTH 1,
             "! Data description
             datadescription TYPE x LENGTH 1,
             unused1         TYPE x LENGTH 2,
             "! SAP Code Page (4 numeric characters in encoding US-ASCII
             codepage        TYPE x LENGTH 4,
             unused2         TYPE x LENGTH 4,
           END OF ty_transport_header.
    DATA:
      transport_header        TYPE ty_transport_header,
      transport_header_x      TYPE x LENGTH 16,
      sap_codepage            TYPE cpcodepage,
      dbuf2                   TYPE xstring,
      transport_header_length TYPE i,
      codepage                TYPE string.

    " decompress export/import document if compressed
    dbuf2 = dbuf.
    normalize_blob( CHANGING blob = dbuf2 ).

    " TRANSPORT HEADER
    IF xstrlen( dbuf ) < 16.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.
    transport_header_x = dbuf(16).
    result-reader = cl_abap_conv_in_ce=>create( encoding = '1100' input = transport_header_x ).
    read_blob_struct( IMPORTING data = transport_header length = transport_header_length ).

    result-version = transport_header-version.

    sap_codepage = cl_abap_codepage=>convert_from(
        source   = CONV xstring( transport_header-codepage )
        codepage = 'US-ASCII' ).
    codepage = cl_abap_codepage=>sap_to_http( sap_codepage ).

    " Recreate CONV with right code page
    result-reader = cl_abap_conv_in_ce=>create(
            encoding = CONV #( sap_codepage )
            endian   = COND #( WHEN transport_header-intformat = '01' THEN 'B' ELSE 'L' )
            input    = dbuf ).
    result-reader->skip_x( transport_header_length ).

  ENDMETHOD.

  METHOD dbuf_import_create_data.

    IF xstrlen( dbuf ) < 16.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.
    CASE dbuf+1(1).
      WHEN '06'.
*        partab = NEW zcl_expimp_v6_importer( dbuf )->run( ).
      WHEN OTHERS.
    ENDCASE.

*    DATA:
*      transport_header TYPE ty_transport_header.
*
*    normalize_blob( CHANGING blob = dbuf ).
*
*    IF xstrlen( dbuf ) < 16.
*      RAISE EXCEPTION TYPE zcx_expimp_table.
*    ENDIF.
*
*    " TRANSPORT HEADER
*    DATA: transport_header_x TYPE x LENGTH 16.
*    transport_header_x = dbuf(16).
*    conv = cl_abap_conv_in_ce=>create( encoding = '1100' input = transport_header_x ).
*    read_blob_struct( IMPORTING data = transport_header length = DATA(transport_header_length) ).
*
*    version = transport_header-version.
*
*    DATA(sap_codepage) = CONV cpcodepage( cl_abap_codepage=>convert_from(
*        source   = CONV xstring( transport_header-codepage )
*        codepage = 'US-ASCII' ) ).
*    DATA(codepage) = cl_abap_codepage=>sap_to_http( sap_codepage ).
*
*    " Recreate CONV with right code page
*    conv = cl_abap_conv_in_ce=>create(
*            encoding = CONV #( sap_codepage )
*            endian   = COND #( WHEN transport_header-intformat = '01' THEN 'B' ELSE 'L' )
*            input    = dbuf ).
*    conv->skip_x( transport_header_length ).
*    FREE dbuf. " Not needed anymore because it's in CONV
*
*    DATA(xstring) = VALUE xstring( ).
*
*    DO.
*
*      CASE blob_curr_byte( ).
*        WHEN c_object_id-end_of_data. "04"
*          EXIT.
*        WHEN OTHERS.
*          current-dd = VALUE #( ).
*          current-dv = VALUE #( ).
*          process_data_object( ).
*      ENDCASE.
*
*    ENDDO.
*
*    partab = me->partab.

  ENDMETHOD.


  METHOD normalize_blob.

    " This code is copied from subroutine READ_BLOB of program RSINDX00 and adapted.

    DATA: l_off   TYPE i,
          l_len   TYPE i,
          lr_blob TYPE REF TO indx_clust_blob ##NEEDED,
          datalen TYPE p,
          BEGIN OF l_xdat,
            srtf2  TYPE indx-srtf2,
            clustr TYPE indx-clustr,
            clustd TYPE x LENGTH 1000,
          END OF l_xdat,
          l_otab LIKE TABLE OF l_xdat,
          l_stab LIKE TABLE OF l_xdat.

    datalen = xstrlen( blob ).

    IF datalen < 4.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.

    IF blob+4(1) = '02'. "compressed data
      " re-format the XSTRING into data cluster
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


  METHOD process_data_object.

    TYPES : BEGIN OF ty_dv,
              dref     TYPE REF TO data,
              rtti     TYPE REF TO cl_abap_datadescr,
              dd_pos   TYPE i,
              off      TYPE i,
              comp_num TYPE i,
            END OF ty_dv,
            ty_dvs TYPE STANDARD TABLE OF ty_dv WITH EMPTY KEY.
    DATA: object_header_v04_05 TYPE ty_object_header-v04_05,
          object_header_v01_03 TYPE ty_object_header-v01_03,
          lines2               TYPE zcl_expimp_utilities=>ty_dd2_lines.


    "=====================
    "  Data Object general type and name
    "=====================
    CASE version.
      WHEN c_version-v06.
        read_blob_struct( IMPORTING data = current-object-header_uni ).
      WHEN c_version-v04
        OR c_version-v05.
        read_blob_struct( IMPORTING data = object_header_v04_05 ).
        current-object-header_uni = VALUE #(
            BASE object_header_v04_05
            leng = CONV i( object_header_v04_05-leng )
            next = CONV i( object_header_v04_05-next )
            nlen = CONV i( object_header_v04_05-nlen ) ).
      WHEN OTHERS.
        read_blob_struct( IMPORTING data = object_header_v01_03 ).
        current-object-header_uni = VALUE #(
            BASE object_header_v01_03
            leng = CONV i( object_header_v01_03-leng )
            next = CONV i( object_header_v01_03-next )
            nlen = CONV i( object_header_v01_03-nlen ) ).
    ENDCASE.
    conv->read(
        EXPORTING n = CONV #( current-object-header_uni-nlen )
        IMPORTING data = current-object-name ).

    "=====================
    "  Data Description (type details)
    "=====================
    process_data_description( ).

    IF current-dd-rtti IS NOT BOUND.
      " No DD block -> Use the simple data definition described at Object Header
      create_default_description( ).
    ENDIF.

    normalize_data_description(
        IMPORTING
            lines2 = lines2
        CHANGING
            lines  = current-dd-lines ).

    IF lines( lines2 ) <> 1.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.
    current-dd-line2 = lines2[ 1 ].

    current-dd-rtti ?= create_data_object( current-dd-line2 ).

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

    DATA(current_byte) = blob_curr_byte( ).
    IF current_byte BETWEEN 'B0' AND 'CF'.
      current-dv = VALUE #( id = current_byte ).
      read_data_object2(
          EXPORTING
              dd2 = current-dd-line2
          IMPORTING
              dv  = <dv_field> ).
    ELSE.
      " other value could be the next Data Description
    ENDIF.

  ENDMETHOD.


  METHOD normalize_data_description.

    CASE version.

      WHEN c_version-v01
          OR c_version-v02
          OR c_version-v03.

        normalize_data_description_v3(
            IMPORTING
                lines2 = lines2
            CHANGING
                lines  = current-dd-lines ).
        normalize_data_description_end(
            CHANGING
                lines2 = lines2 ).

      WHEN c_version-v04
          OR c_version-v05
          OR c_version-v06.

        normalize_data_description_v6(
            IMPORTING
                lines2 = lines2
            CHANGING
                lines  = current-dd-lines ).
    ENDCASE.

  ENDMETHOD.


  METHOD normalize_data_description_v3.

    DATA:
      dd_line2          TYPE ty_dd2,
      filler_byte_count TYPE i.
    FIELD-SYMBOLS:
      <dd2_lines> TYPE ty_dd2_lines.

    LOOP AT lines REFERENCE INTO DATA(dd_line).

      IF dd_line->id = up_to.
        DELETE lines USING KEY loop_key.
        EXIT.
      ENDIF.

      dd_line2 = CORRESPONDING #( dd_line->* ).

      DELETE lines USING KEY loop_key.

      DATA(up_to_2) = SWITCH ty_dd_id( dd_line2-id
        WHEN c_dd_id-structure_include-start THEN c_dd_id-structure_include-end
        WHEN c_dd_id-boxed_component-start   THEN c_dd_id-boxed_component-end
        WHEN c_dd_id-structure-start         THEN c_dd_id-structure-end
        WHEN c_dd_id-table-start             THEN c_dd_id-table-end ).

      IF up_to_2 IS NOT INITIAL.

        CREATE DATA dd_line2-components TYPE ty_dd2_lines.
        ASSIGN dd_line2-components->* TO <dd2_lines>.

        CASE dd_line2-type.
          WHEN c_ityp-int.
            filler_byte_count = alignment_offset MOD 4.
          WHEN c_ityp-float.
            filler_byte_count = alignment_offset MOD 8.
          WHEN c_ityp-struct1
              OR c_ityp-struct2.
            IF dd_line2-id = c_dd_id-table-start.
              " Add a dummy structure AB...AC to reflect the actual type
              DATA(dd2_line_struct) = VALUE ty_dd2(
                  id         = c_dd_id-structure-start
                  type       = dd_line2-type
                  leng       = dd_line2-leng
                  components = NEW ty_dd2_lines( ) ).
              APPEND dd2_line_struct TO <dd2_lines>.
              ASSIGN dd2_line_struct-components->* TO <dd2_lines>.
            ENDIF.
        ENDCASE.

        IF filler_byte_count <> 0.
          DATA(dd2_line_filler) = VALUE ty_dd2(
              id         = c_dd_id-filler
              type       = c_ityp-hex
              leng       = filler_byte_count
              components = NEW ty_dd2_lines( ) ).
          APPEND dd2_line_filler TO lines2.
        ENDIF.

        normalize_data_description_v3(
          EXPORTING
              up_to  = up_to_2
              alignment_offset = alignment_offset
          IMPORTING
              lines2 = <dd2_lines>
          CHANGING
              lines  = lines ).

      ENDIF.

      APPEND dd_line2 TO lines2.

      alignment_offset = alignment_offset + dd_line2-leng.

    ENDLOOP.

  ENDMETHOD.


  METHOD normalize_data_description_v6.

    DATA:
      dd_line2          TYPE ty_dd2,
      filler_byte_count TYPE i.
    FIELD-SYMBOLS:
      <dd2_lines> TYPE ty_dd2_lines.

    LOOP AT lines REFERENCE INTO DATA(dd_line).

      IF dd_line->id = up_to.
        DELETE lines USING KEY loop_key.
        EXIT.
      ENDIF.

      dd_line2 = CORRESPONDING #( dd_line->* ).

      DELETE lines USING KEY loop_key.

      DATA(up_to_2) = SWITCH ty_dd_id( dd_line2-id
        WHEN c_dd_id-structure_include-start THEN c_dd_id-structure_include-end
        WHEN c_dd_id-boxed_component-start   THEN c_dd_id-boxed_component-end
        WHEN c_dd_id-structure-start         THEN c_dd_id-structure-end
        WHEN c_dd_id-table-start             THEN c_dd_id-table-end ).

      IF up_to_2 IS NOT INITIAL.

        CREATE DATA dd_line2-components TYPE ty_dd2_lines.
        ASSIGN dd_line2-components->* TO <dd2_lines>.

        normalize_data_description_v6(
          EXPORTING
              up_to  = up_to_2
          IMPORTING
              lines2 = <dd2_lines>
          CHANGING
              lines  = lines ).
      ENDIF.

      APPEND dd_line2 TO lines2.

    ENDLOOP.

  ENDMETHOD.


  METHOD normalize_data_description_end.

    " TODO :
    "   - what about non-structured tables?
    "   - what about nested tables?
    CASE version.
      WHEN c_version-v01
        OR c_version-v02
        OR c_version-v03.
        CASE current-object-header_uni-id.
          WHEN c_object_id-flat_structure
              OR c_object_id-deep_structure.
            lines2 = VALUE #(
                  LET save_lines2 = lines2 IN
                  ( id         = c_dd_id-structure-start
                    components = NEW ty_dd2_lines( save_lines2 ) ) ).
          WHEN c_object_id-flat_table
              OR c_object_id-deep_table.
            lines2 = VALUE #(
                  LET save_lines2 = lines2 IN
                  ( id         = c_dd_id-table-start
                    components = NEW ty_dd2_lines(
                      ( id         = c_dd_id-structure-start
                        components = NEW ty_dd2_lines( save_lines2 ) ) ) ) ).
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_expimp_table.
        ENDCASE.
    ENDCASE.
*    ENDIF.

  ENDMETHOD.


  METHOD process_data_description.

    DO.

      IF blob_curr_byte( ) NOT BETWEEN 'A0' AND 'AF'.
        " The current byte is either a byte for "start of data value" or "end of data"
        " (note that data description is optional)
        EXIT.
      ENDIF.

      process_data_description_line( ).

    ENDDO.

  ENDMETHOD.


  METHOD process_data_description_line.

    DATA: data_description     TYPE ty_data_description-v00_46,
          data_description_uni TYPE ty_data_description-uni,
          dd                   TYPE ty_dd.


    " Fail fast to avoid the error that would occur next if there are not enough bytes to read
    IF blob_curr_byte( ) NOT BETWEEN 'A0' AND 'AF'.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.

    " Read the next Data Description block
    CASE version.
      WHEN c_version-v06.
        read_blob_struct( IMPORTING data = data_description_uni ).
        dd = VALUE #(
            id   = data_description_uni-id
            type = data_description_uni-type
            leng = data_description_uni-flen
            decs = data_description_uni-decs ).
      WHEN OTHERS.
        read_blob_struct( IMPORTING data = data_description ).
        dd = VALUE #(
            id   = data_description-id
            type = data_description-type
            leng = data_description-flen
            decs = 0 ).
    ENDCASE.

    CASE dd-id.

      WHEN c_dd_id-structure_include-start.
        " TODO
        RAISE EXCEPTION TYPE zcx_expimp_table.

      WHEN c_dd_id-structure_include-end.
        " TODO
        RAISE EXCEPTION TYPE zcx_expimp_table.

      WHEN c_dd_id-boxed_component-start.
        " TODO
        RAISE EXCEPTION TYPE zcx_expimp_table.

      WHEN c_dd_id-boxed_component-end.
        " TODO
        RAISE EXCEPTION TYPE zcx_expimp_table.

      WHEN c_dd_id-primitive.
        current-dd-rtti = create_data_primitive( type = dd-type leng = dd-leng decs = dd-decs ).

      WHEN c_dd_id-structure-end.
        current-dd-rtti = cl_abap_structdescr=>get(
            p_components = VALUE #(
                FOR rtti2 IN current-dd-rtti_s INDEX INTO j2
                ( name       = |CMP{ j2 WIDTH = 4 ALIGN = RIGHT PAD = '0' }|
                  type       = rtti2 ) )
            p_strict     = abap_false ). "cx_sy_struct_creation

      WHEN c_dd_id-table-end.
        " NB: the table should have only one component
        CASE dd-type.
          WHEN c_ityp-struct1 OR c_ityp-struct2.
            " Either its lines are structured
            current-dd-rtti = cl_abap_structdescr=>get(
                        p_components = VALUE #(
                            FOR rtti2 IN current-dd-rtti_s INDEX INTO j2
                            ( name       = |CMP{ j2 WIDTH = 4 ALIGN = RIGHT PAD = '0' }|
                              type       = rtti2 ) )
                        p_strict     = abap_false ).
            "cx_sy_struct_creation
          WHEN OTHERS.
            " Or its lines have an elementary type
            IF lines( current-dd-rtti_s ) > 1.
              RAISE EXCEPTION TYPE zcx_expimp_table.
            ENDIF.
            current-dd-rtti = current-dd-rtti_s[ 1 ].
        ENDCASE.
        current-dd-rtti = cl_abap_tabledescr=>get( p_line_type = current-dd-rtti ).
        "cx_sy_table_creation

      WHEN c_dd_id-structure-start
          OR c_dd_id-table-start
          OR c_dd_id-filler.
        " Nothing special
        ASSERT 1 = 1.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDCASE.

    APPEND dd TO current-dd-lines.

    CASE dd-id.
      WHEN c_dd_id-primitive.
        " Classic field
        APPEND current-dd-rtti TO current-dd-rtti_s.
      WHEN c_dd_id-structure-end
          OR c_dd_id-table-end.
        " End of structure or table -> POP from stack
        current-dd-rtti_s = current-dd-stack_of_rtti_s[ lines( current-dd-stack_of_rtti_s ) ].
        DELETE current-dd-stack_of_rtti_s INDEX lines( current-dd-stack_of_rtti_s ).
        APPEND current-dd-rtti TO current-dd-rtti_s.
      WHEN c_dd_id-structure-start
          OR c_dd_id-table-start.
        " Begin of structure or table -> PUSH to stack
        APPEND current-dd-rtti_s TO current-dd-stack_of_rtti_s.
        current-dd-rtti_s = VALUE #( ).
    ENDCASE.

  ENDMETHOD.


  METHOD read_blob_dv.

    DATA(position) = conv->position.

    conv->read( IMPORTING data = data ).

    current-dv-dd_off = current-dv-dd_off + conv->position - position.

  ENDMETHOD.


  METHOD read_blob_struct.

*    FIELD-SYMBOLS:
*      <x> TYPE x.
    DATA:
      conv2 TYPE REF TO cl_abap_conv_in_ce,
      view  TYPE REF TO cl_abap_view_offlen.

    DATA(xstring) = VALUE xstring( ).
    DESCRIBE FIELD data LENGTH length IN BYTE MODE.
    conv->read( EXPORTING n = length IMPORTING data = xstring ).

    conv2 = cl_abap_conv_in_ce=>create(
              encoding = conv->encoding
              endian   = conv->endian ).
    view = cl_abap_view_offlen=>create_legacy_view( data ).
    conv->convert_struc(
          EXPORTING input = xstring
                    view  = view
          IMPORTING data  = data ).

*  ENDMETHOD.
*
*
*  METHOD read_blob.
*
*    FIELD-SYMBOLS <x> TYPE x.
*
*    DATA(xstring) = VALUE xstring( ).
*    DESCRIBE FIELD data LENGTH DATA(length) IN BYTE MODE.
*    conv->read( EXPORTING n = length IMPORTING data = xstring ).
*    ASSIGN data TO <x> CASTING.
*    <x> = xstring.

  ENDMETHOD.


  METHOD read_data_object2.

    FIELD-SYMBOLS:
      <dd2_lines> TYPE ty_dd2_lines.

    DATA(rtti) = cl_abap_typedescr=>describe_by_data( dv ).

    CASE dd2-id.

      WHEN c_dd_id-primitive.

        IF rtti->kind <> rtti->kind_elem.
          RAISE EXCEPTION TYPE zcx_expimp_table.
        ENDIF.

        read_data_primitive(
            EXPORTING dd2 = dd2
            IMPORTING dv = dv off = off ).

      WHEN c_dd_id-structure-start.

        IF rtti->kind <> rtti->kind_struct.
          RAISE EXCEPTION TYPE zcx_expimp_table.
        ENDIF.
        ASSIGN dd2-components->* TO <dd2_lines>.

        read_data_structure(
            EXPORTING
                dd2_lines = <dd2_lines>
            IMPORTING
                dv = dv
                off = off ).

      WHEN c_dd_id-table-start.

        IF rtti->kind <> rtti->kind_table.
          RAISE EXCEPTION TYPE zcx_expimp_table.
        ENDIF.
        ASSIGN dd2-components->* TO <dd2_lines>.

        read_data_table(
            EXPORTING
                dd2_lines = <dd2_lines>
                dd_index = current-dv-dd_index
            IMPORTING dv = dv ).

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDCASE.

  ENDMETHOD.


  METHOD read_data_primitive.

    DATA:
      dah_interval       TYPE ty_dah-interval-v00_46,
      dah_interval_uni   TYPE ty_dah-interval-uni,
      dah_string_xstring TYPE ty_dah-string_xstring.


    IF current-dv-dd_off = 0.
      " Start of Data Value

      CASE current-dv-id.

        WHEN c_dv_id-single.
          conv->skip_x( 1 ).

        WHEN c_dv_id-interval-start.
          CASE version.
            WHEN c_version-v06.
              read_blob_struct( IMPORTING data = dah_interval_uni ).
              ASSERT dah_interval_uni-id = c_dv_id-interval-start.
              current-dv-len = dah_interval_uni-len.
            WHEN OTHERS.
              read_blob_struct( IMPORTING data = dah_interval ).
              ASSERT dah_interval-id = c_dv_id-interval-start.
              current-dv-len = dah_interval-len.
          ENDCASE.

        WHEN c_dv_id-string_xstring-start.

          read_blob_struct( IMPORTING data = dah_string_xstring ).
          ASSERT dah_string_xstring-id = c_dv_id-string_xstring-start.

        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_expimp_table.
      ENDCASE.

    ENDIF.



    CASE current-dv-id.

      WHEN c_dv_id-single.
        read_blob_dv( IMPORTING data = dv ).
        IF current-dv-dd_off = current-dv-len.
          current-dv = VALUE #( id = blob_curr_byte( ) ).
        ENDIF.

      WHEN c_dv_id-interval-start.
        read_blob_dv( IMPORTING data = dv ).
        IF current-dv-dd_off = current-dv-len.
          assert_blob_curr_byte_and_skip( c_dv_id-interval-end ).
          current-dv = VALUE #( id = blob_curr_byte( ) ).
        ENDIF.

      WHEN c_dv_id-string_xstring-start.
        conv->read(
            EXPORTING n = COND #( WHEN version = c_version-v06
                THEN dah_string_xstring-len / 2 ELSE dah_string_xstring-len )
            IMPORTING data = dv ).

        current-dv-dd_off = current-dv-dd_off + 8.

        assert_blob_curr_byte_and_skip( c_dv_id-string_xstring-end ).
        current-dv = VALUE #( id = blob_curr_byte( ) ).

    ENDCASE.


*      WHEN c_dv_id-string_xstring-start.
*
*        read_blob_struct( IMPORTING data = dah_string_xstring ).
*        ASSERT dah_string_xstring-id = c_dv_id-string_xstring-start.
*
*        conv->read(
*            EXPORTING n = COND #( WHEN version = c_version-v06
*                THEN dah_string_xstring-len / 2 ELSE dah_string_xstring-len )
*            IMPORTING data = dv ).
*
*        current-dv-dd_off = current-dv-dd_off + 8.
*
*        assert_blob_curr_byte_and_skip( c_dv_id-string_xstring-end ).
*        current-dv = VALUE #( id = blob_curr_byte( ) ).
*
*      WHEN OTHERS.
*        RAISE EXCEPTION TYPE zcx_expimp_table.
*    ENDCASE.

  ENDMETHOD.


  METHOD read_data_structure.

    DATA(comp_number) = 0.

    LOOP AT dd2_lines REFERENCE INTO DATA(dd2).

      IF dd2->id = c_dd_id-filler.

        conv->skip_x( dd2->leng ).

        IF current-dv-id = c_dv_id-interval-start.
          current-dv-dd_off = current-dv-dd_off + dd2->leng.
          IF current-dv-dd_off = current-dv-len.
            assert_blob_curr_byte_and_skip( c_dv_id-interval-end ).
            current-dv = VALUE #( id = blob_curr_byte( ) ).
          ENDIF.
        ENDIF.

      ELSE.

        ADD 1 TO comp_number.
        ASSIGN COMPONENT comp_number OF STRUCTURE dv TO FIELD-SYMBOL(<dv_component>).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_expimp_table.
        ENDIF.

        read_data_object2(
          EXPORTING
              dd2 = dd2->*
          IMPORTING
              dv  = <dv_component>
              off = off ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD read_data_table.

    DATA:
      dah_table     TYPE ty_dah-table-v00_46,
      dah_table_uni TYPE ty_dah-table-uni,
      line          TYPE REF TO data.
    FIELD-SYMBOLS:
      <line> TYPE any.

    CASE version.
      WHEN c_version-v06.
        read_blob_struct( IMPORTING data = dah_table_uni ).
        ASSERT dah_table_uni-id = c_dv_id-table-start.
        current-dv-len = dah_table_uni-len.
      WHEN c_version-v04
            OR c_version-v05.
        read_blob_struct( IMPORTING data = dah_table ).
        ASSERT dah_table-id = c_dv_id-table-start.
        current-dv-len = dah_table-len.
      WHEN OTHERS.
        " No table start
        current-dv-len = current-object-header_uni-leng.
    ENDCASE.
    current-dv = VALUE #( id = blob_curr_byte( ) ).

    CREATE DATA line LIKE LINE OF dv.
    ASSIGN line->* TO <line>.


    DO.

      CASE version.
        WHEN c_version-v01
            OR c_version-v02
            OR c_version-v03.
          IF blob_curr_byte( ) <> c_dv_id-single.
            EXIT.
          ENDIF.
        WHEN OTHERS.
          IF blob_curr_byte( ) = c_dv_id-table-end.
            conv->skip_x( 1 ).
            EXIT.
          ENDIF.
      ENDCASE.

      CLEAR <line>.
      read_data_object2(
        EXPORTING
            dd2 = dd2_lines[ 1 ] "describes the type of line
        IMPORTING
            dv  = <line> ).

      INSERT <line> INTO TABLE dv.

    ENDDO.

  ENDMETHOD.


  METHOD tab_cpar_to_ptab.

    DATA(level1) = 0.
    DATA(level2) = 0.
    DATA(level3) = 0.

    LOOP AT tab_cpar REFERENCE INTO DATA(cpar).

      DATA(subname) = |P-P{ level1 }-P{ level2 }-P{ level3 }|.
      DATA(result_component_name) = |PTAB_GROUP-{ subname }|.
      ASSIGN (result_component_name) TO FIELD-SYMBOL(<var>).
      <var> = cpar->dref.

      ptab_group-ptab = VALUE #( BASE ptab_group-ptab
          ( paramname = cpar->name varname = |{ varname }-{ subname }->*| ) ).

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

  ENDMETHOD.
ENDCLASS.
