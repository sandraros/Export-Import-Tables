CLASS zcl_expimp_importer DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        dbuf TYPE xstring
      RAISING
        cx_sy_compression_error
        zcx_expimp_table.

  PROTECTED SECTION.

    TYPES: ty_byte TYPE x LENGTH 1.

    "! If the 5th byte = '02', the BLOB must be uncompressed by kernel AB_IMPORT_DECOMPRESS
    METHODS uncompress
      IMPORTING
        dbuf          TYPE xstring
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        cx_sy_compression_error
        zcx_expimp_table.

    METHODS assert_blob_curr_byte_and_skip
      IMPORTING
        expected_byte TYPE ty_byte
      RAISING
        zcx_expimp_table.

    "! OBSOLETE -> use ZCL_EXPIMP_READER->GET_CURRENT_BYTE <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter byte | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_expimp_table | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_current_byte
      RETURNING
        VALUE(byte) TYPE ty_byte
      RAISING
        zcx_expimp_table.

    DATA: transport_header TYPE zif_expimp_vx=>ty_transport_header,
          version          TYPE zif_expimp_vx=>ty_transport_header-version,
          reader           TYPE REF TO zcl_expimp_reader,
          current_byte     TYPE ty_byte.

  PRIVATE SECTION.

    METHODS get_reader
      IMPORTING
        dbuf          TYPE xstring
      RETURNING
        VALUE(reader) TYPE REF TO zcl_expimp_reader.

ENDCLASS.



CLASS zcl_expimp_importer IMPLEMENTATION.


  METHOD constructor.

    IF xstrlen( dbuf ) < 16.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.

    IF dbuf+4(1) = '02'. "compressed data
      " decompress export/import document if compressed
      DATA(dbuf2) = uncompress( dbuf ).
    ELSE.
      dbuf2 = dbuf.
    ENDIF.

    reader = get_reader( dbuf ).

  ENDMETHOD.


  METHOD uncompress.

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

    datalen = xstrlen( dbuf ).

    IF datalen < 4.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.

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
        l_xdat-clustd = dbuf+l_off(l_len).
        APPEND l_xdat TO l_stab.
        l_off = l_off + 1000.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.


    CLEAR result.

    CALL 'AB_IMPORT_DECOMPRESS'
      ID 'SRCTAB' FIELD l_stab[]
      ID 'DSTTAB' FIELD l_otab[].

    CLEAR l_stab.


    LOOP AT l_otab REFERENCE INTO DATA(l_o).
      CONCATENATE result l_o->clustd(l_o->clustr) INTO result IN BYTE MODE.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_reader.

    " TRANSPORT HEADER
    DATA: transport_header_x TYPE x LENGTH 16.

    transport_header_x = dbuf(16).
    reader = zcl_expimp_reader=>create( encoding = '1100' input = transport_header_x ).
    reader->read_structure( IMPORTING data = transport_header length = DATA(transport_header_length) ).

    DATA(sap_codepage) = CONV cpcodepage( cl_abap_codepage=>convert_from(
        source   = CONV xstring( transport_header-codepage )
        codepage = 'US-ASCII' ) ).
    DATA(codepage) = cl_abap_codepage=>sap_to_http( sap_codepage ).

    " Recreate CONV with right code page
    reader = zcl_expimp_reader=>create(
            encoding = CONV #( sap_codepage )
            endian   = COND #( WHEN transport_header-intformat = '01' THEN 'B' ELSE 'L' )
            input    = dbuf ).
    reader->skip_x( transport_header_length ).

  ENDMETHOD.


  METHOD assert_blob_curr_byte_and_skip.

    DATA: actual_byte TYPE ty_byte.

    TRY.

        reader->read( IMPORTING data = actual_byte ).

      CATCH cx_root INTO DATA(lx).
        RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING previous = lx.
    ENDTRY.

    IF actual_byte <> expected_byte.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.

  ENDMETHOD.


  METHOD get_current_byte.

    reader->read( IMPORTING data = current_byte len = DATA(len) ).
    IF len = 0.
      RAISE EXCEPTION TYPE zcx_expimp_table.
    ENDIF.
    reader->skip_x( -1 ).
    byte = current_byte.

  ENDMETHOD.


ENDCLASS.
