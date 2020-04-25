*&---------------------------------------------------------------------*
*& Report z_expimp_table_list_tables
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_expimp_table_list_tables.

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

  alv->display( ).
