*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_dd DEFINITION ABSTRACT.
  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING
        reader TYPE REF TO zcl_expimp_reader
    RETURNING
    VALUE(result) type ref to lcl_dd
      RAISING
        zcx_expimp_table.
    METHODS constructor
      IMPORTING
        reader TYPE REF TO zcl_expimp_reader.
    METHODS read ABSTRACT
      RETURNING
        VALUE(result) TYPE REF TO lcl_dd
      RAISING
        zcx_expimp_table.
    DATA: reader   TYPE REF TO zcl_expimp_reader,
          data_description type zif_expimp_v6=>ty_data_description,
          position TYPE i.
    CONSTANTS:
*      c_object_id LIKE zif_expimp_v6=>c_object_id VALUE zif_expimp_v6=>c_object_id,
      c_dd_id     LIKE zif_expimp_v6=>c_dd_id VALUE zif_expimp_v6=>c_dd_id.
ENDCLASS.
