CLASS zcl_expimp_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
             ty_byte type x length 1.

    METHODS read_structure
      EXPORTING
        data   TYPE any
        length TYPE i.

    METHODS get_position
      RETURNING
        VALUE(result) TYPE i.

    METHODS read
      IMPORTING
        n    TYPE i DEFAULT -1
        view TYPE REF TO cl_abap_view_offlen OPTIONAL
      EXPORTING
        data TYPE any
        len  TYPE i
      RAISING
        cx_sy_conversion_codepage
        cx_sy_codepage_converter_init
        cx_parameter_invalid_type
        cx_parameter_invalid_range.

    METHODS convert_struc
      IMPORTING
        input           TYPE xsequence
        view            TYPE REF TO cl_abap_view_offlen
      EXPORTING
        data            TYPE any
        input_too_short TYPE abap_bool
      RAISING
        cx_sy_conversion_codepage
        cx_sy_codepage_converter_init
        cx_parameter_invalid_type
        cx_parameter_invalid_range.

    METHODS skip_x
      IMPORTING n   TYPE i DEFAULT 1
      EXPORTING len TYPE i.

    CLASS-METHODS create
      IMPORTING
        encoding    TYPE abap_encoding DEFAULT 'DEFAULT'
        endian      TYPE abap_endian OPTIONAL
*  replacement  type abap_repl default '#'
*  ignore_cerr  type abap_bool default abap_false
        input       TYPE xsequence OPTIONAL
      RETURNING
        VALUE(conv) TYPE REF TO zcl_expimp_reader
      RAISING
        cx_parameter_invalid_range
        cx_sy_codepage_converter_init.

    "! Read the current byte without changing the position <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter byte | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_expimp_table | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_current_byte
      RETURNING
        VALUE(byte) TYPE ty_byte
      RAISING
        zcx_expimp_table.

data: current_byte type ty_byte READ-ONLY.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: reader TYPE REF TO cl_abap_conv_in_ce.

ENDCLASS.



CLASS zcl_expimp_reader IMPLEMENTATION.


  METHOD read.

    reader->read(
      EXPORTING
        n    = n
        view = view
      IMPORTING
        data = data
        len  = len ).

  ENDMETHOD.


  METHOD read_structure.

    DATA:
      view TYPE REF TO cl_abap_view_offlen.

    DATA(xstring) = VALUE xstring( ).
    DESCRIBE FIELD data LENGTH length IN BYTE MODE.
    reader->read( EXPORTING n = length IMPORTING data = xstring ).

    view = cl_abap_view_offlen=>create_legacy_view( data ).
    reader->convert_struc(
          EXPORTING input = xstring
                    view  = view
          IMPORTING data  = data ).

  ENDMETHOD.

  METHOD get_position.

    result = reader->position.

  ENDMETHOD.

  METHOD create.

    conv = NEW zcl_expimp_reader( ).
    conv->reader = cl_abap_conv_in_ce=>create(
            encoding = encoding
            endian   = endian
            input    = input ).

  ENDMETHOD.

  METHOD skip_x.

    reader->skip_x(
      EXPORTING
        n   = n
      IMPORTING
        len = len ).

  ENDMETHOD.

  METHOD convert_struc.

    reader->convert_struc(
      EXPORTING
        input           = input
        view            = view
      IMPORTING
        data            = data
        input_too_short = input_too_short ).

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
