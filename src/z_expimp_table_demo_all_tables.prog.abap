*&---------------------------------------------------------------------*
*& Report z_expimp_table_list_tables
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_expimp_table_demo_all_tables.

DATA gv_tabname TYPE tabname.
SELECT-OPTIONS tabnames FOR gv_tabname.

TYPES: ty_info TYPE zcl_expimp_table=>ty_expimp_table_info,
       BEGIN OF ty_alv,
         tabname           TYPE dd03l-tabname,
         count_id_fields   TYPE i,
         count_attr_fields TYPE i,
         info              TYPE zcl_expimp_table=>ty_expimp_table_info,
       END OF ty_alv,
       tt_alv TYPE STANDARD TABLE OF ty_alv WITH EMPTY KEY.
DATA: gt_alv TYPE tt_alv.

START-OF-SELECTION.

  SELECT DISTINCT tabname
      FROM dd03l
      WHERE tabname  IN @tabnames
        AND as4local = 'A'
        AND fieldname = 'CLUSTD'
      INTO TABLE @DATA(dd03l_s).

  LOOP AT dd03l_s INTO DATA(dd03l).
    TRY.
        DATA(info) = zcl_expimp_table=>get_info( dd03l-tabname ).
        gt_alv = VALUE #(
            BASE gt_alv
            ( tabname         = dd03l-tabname
              count_id_fields = lines( info-id_fields )
              count_attr_fields = lines( info-attr_fieldnames )
              info            = info ) ).
      CATCH zcx_expimp_table ##NO_HANDLER.
    ENDTRY.
  ENDLOOP.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = DATA(alv)
    CHANGING
      t_table      = gt_alv ).

  DATA(lo_columns) = alv->get_columns( ).

  lo_columns->set_optimize( ).

  DATA(lo_functions_list) = alv->get_functions( ).
  lo_functions_list->set_all( ).
  lo_functions_list->set_layout_load( IF_SALV_C_BOOL_SAP=>TRUE ).
  lo_functions_list->set_layout_change( IF_SALV_C_BOOL_SAP=>TRUE ).
  lo_functions_list->set_layout_maintain( IF_SALV_C_BOOL_SAP=>TRUE ).
  lo_functions_list->set_layout_save( IF_SALV_C_BOOL_SAP=>TRUE ).

  LOOP AT lo_columns->get( ) ASSIGNING FIELD-SYMBOL(<ls_column>)
        WHERE r_column IS BOUND.
    IF <ls_column>-r_column->get_short_text( ) IS INITIAL.
      <ls_column>-r_column->set_short_text( CONV #( <ls_column>-columnname ) ).
    ENDIF.
    IF <ls_column>-r_column->get_medium_text( ) IS INITIAL.
      <ls_column>-r_column->set_medium_text( CONV #( <ls_column>-columnname ) ).
    ENDIF.
    IF <ls_column>-r_column->get_long_text( ) IS INITIAL.
      <ls_column>-r_column->set_long_text( CONV #( <ls_column>-columnname ) ).
    ENDIF.
  ENDLOOP.

  alv->display( ).
