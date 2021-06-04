CLASS zcl_expimp_v6_importer DEFINITION
  INHERITING FROM zcl_expimp_importer
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        dbuf TYPE xstring
      RAISING
        cx_sy_compression_error
        zcx_expimp_table.

    METHODS get_next_data_object
      RETURNING
        VALUE(result) TYPE cpar
      RAISING
        zcx_expimp_table.

*    EVENTS data_object_imported
*      EXPORTING
*        VALUE(data_object) TYPE REF TO zcl_expimp_data_object.

  PROTECTED SECTION.

  PRIVATE SECTION.

*    METHODS get_data_description
*      RAISING
*        zcx_expimp_table.
*
*    METHODS get_dd
*      RETURNING
*        VALUE(result) type ref to lcl_dd.

    CONSTANTS:
      c_object_id LIKE zif_expimp_v6=>c_object_id VALUE zif_expimp_v6=>c_object_id,
      c_dd_id     LIKE zif_expimp_v6=>c_dd_id VALUE zif_expimp_v6=>c_dd_id.

ENDCLASS.



CLASS zcl_expimp_v6_importer IMPLEMENTATION.

  METHOD constructor.

    super->constructor( dbuf ).

    ASSERT version = '06'.

  ENDMETHOD.

  METHOD get_next_data_object.

    TYPES : BEGIN OF ty_dv,
              dref     TYPE REF TO data,
              rtti     TYPE REF TO cl_abap_datadescr,
              dd_pos   TYPE i,
              off      TYPE i,
              comp_num TYPE i,
            END OF ty_dv,
            ty_dvs TYPE STANDARD TABLE OF ty_dv WITH EMPTY KEY.
    DATA: object_header TYPE zif_expimp_v6=>ty_object_header,
          data_object_name type string.
*          lines2        TYPE zcl_expimp_utilities=>ty_dd2_lines.


    "=====================
    "  Data Object general type and name
    "=====================
    reader->read_structure( IMPORTING data = object_header ).
    reader->read(
        EXPORTING n = CONV #( object_header-nlen )
        IMPORTING data = data_object_name ).

    "=====================
    "  Data Description (type details)
    "=====================
    IF get_current_byte( ) BETWEEN 'A0' AND 'AF'.
      DATA(dd) = lcl_dd=>create( reader ).
    ELSE.
*      " No DD block -> Use the simple data definition described at Object Header
*      create_default_description( ).
    ENDIF.

*    normalize_data_description(
*        IMPORTING
*            lines2 = lines2
*        CHANGING
*            lines  = current-dd-lines ).
*
*    IF lines( lines2 ) <> 1.
*      RAISE EXCEPTION TYPE zcx_expimp_table.
*    ENDIF.
*    current-dd-line2 = lines2[ 1 ].
*
*    current-dd-rtti ?= create_data_object( current-dd-line2 ).
*
*    DATA(dv) = VALUE ty_dv( rtti = current-dd-rtti ).
*    CREATE DATA dv-dref TYPE HANDLE current-dd-rtti.
*    ASSIGN dv-dref->* TO FIELD-SYMBOL(<dv_field>).
*
*    partab = VALUE #(
*        BASE partab
*        ( name = current-object-name
*          dref = dv-dref ) ).
*
*    "=====================
*    "  Data Value
*    "=====================
*    DATA(dvs) = VALUE ty_dvs( ).
*
*    DATA(current_byte) = blob_curr_byte( ).
*    IF current_byte BETWEEN 'B0' AND 'CF'.
*      current-dv = VALUE #( id = current_byte ).
*      read_data_object2(
*          EXPORTING
*              dd2 = current-dd-line2
*          IMPORTING
*              dv  = <dv_field> ).
*    ELSE.
*      " other value could be the next Data Description
*    ENDIF.

  ENDMETHOD.

ENDCLASS.
