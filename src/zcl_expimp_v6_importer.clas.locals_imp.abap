*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_dd_table DEFINITION
  INHERITING FROM lcl_dd.
  PUBLIC SECTION.
    METHODS read REDEFINITION.
    DATA:
          table_header type zif_expimp_v6=>ty_dah-table,
          line_type TYPE REF TO lcl_dd.
ENDCLASS.

CLASS lcl_dd_structure DEFINITION
  INHERITING FROM lcl_dd.
  PUBLIC SECTION.
    METHODS read REDEFINITION.
    DATA:
          components type table of ref to lcl_dd.
ENDCLASS.

CLASS lcl_dd IMPLEMENTATION.
  METHOD constructor.
    " Read the next Data Description block
    me->reader = reader.
    me->position = reader->get_position( ).
    reader->read_structure( IMPORTING data = data_description ).
  ENDMETHOD.
  METHOD create.
    CASE reader->get_current_byte( ).
      WHEN c_dd_id-structure_include-start.
*        get_dd_structure_include( ).
      WHEN c_dd_id-boxed_component-start.
*        get_dd_boxed_component( ).
      WHEN c_dd_id-filler.
*        skip_dd_filler( ).
      WHEN c_dd_id-primitive.
*        get_dd_primitive( ).
      WHEN c_dd_id-structure-start.
        result = new lcl_dd_structure( reader )->read( ).
      WHEN c_dd_id-table-start.
        result = new lcl_dd_table( reader )->read( ).
      WHEN OTHERS.
        " The current byte is either a byte for "start of data value" or "end of data"
        " (note that data description is optional)
        EXIT.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_dd_table IMPLEMENTATION.
    METHOD read.
    reader->read_structure( IMPORTING data = table_header ).
    line_type = lcl_dd=>create( reader )->read( ).
*    while reader->get_current_byte( ) <> c_dd_id-table-end.
*    endwhile.
    ENDMETHOD.
ENDCLASS.

CLASS lcl_dd_structure IMPLEMENTATION.
    METHOD read.
    while reader->get_current_byte( ) <> c_dd_id-table-end.
      append lcl_dd=>create( reader )->read( ) to components.
    endwhile.
    ENDMETHOD.
ENDCLASS.
