*"* use this source file for your ABAP unit test classes

CLASS ltc_check_custom_tables DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test FOR TESTING.

    CLASS-DATA environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.
ENDCLASS.

CLASS ltc_check_custom_tables IMPLEMENTATION.

  METHOD class_setup.

    DATA: dd02l_s TYPE TABLE OF dd02l,
          dd03l_s TYPE TABLE OF dd03l.

    environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #(
        ( 'DD02L' )
        ( 'DD03L' ) ) ).

    dd02l_s = VALUE #(
          (
           tabname = 'ZEXPIMP_TEST1'
           as4local = 'A'
           tabclass = 'TRANSP'
           )
          (
           tabname = 'ZEXPIMP_TEST2'
           as4local = 'A'
           tabclass = 'TRANSP'
           )
          (
           tabname = 'ZEXPIMP_TEST3'
           as4local = 'A'
           tabclass = 'TRANSP'
           )
          (
           tabname = 'ZEXPIMP_TEST4'
           as4local = 'A'
           tabclass = 'TRANSP'
           )
          (
           tabname = 'ZEXPIMP_TEST5'
           as4local = 'A'
           tabclass = 'TRANSP'
           )
          (
           tabname = 'ZEXPIMP_TEST6'
           as4local = 'A'
           tabclass = 'TRANSP'
           )
          ).

    environment->insert_test_data( dd02l_s ).

    dd03l_s = VALUE #(
          (
           TABNAME = 'ZEXPIMP_TEST1'
           FIELDNAME = 'CARRID'
           AS4LOCAL = 'A'
           KEYFLAG = 'X'
           POSITION = '0003'
           ROLLNAME = 'S_CARR_ID'
           DATATYPE = 'CHAR'
           LENG = '000003'
           INTTYPE = 'C'
           INTLEN = '000006'
           )
          (
           TABNAME = 'ZEXPIMP_TEST1'
           FIELDNAME = 'CLIENT'
           AS4LOCAL = 'A'
           KEYFLAG = 'X'
           POSITION = '0001'
           DATATYPE = 'CLNT'
           LENG = '000003'
           INTTYPE = 'C'
           INTLEN = '000006'
           )
          (
           TABNAME = 'ZEXPIMP_TEST1'
           FIELDNAME = 'CLUSTD'
           AS4LOCAL = 'A'
           POSITION = '0012'
           ROLLNAME = 'INDX_CLUST'
           DATATYPE = 'LRAW'
           LENG = '002886'
           INTTYPE = 'X'
           INTLEN = '002886'
           )
          (
           TABNAME = 'ZEXPIMP_TEST1'
           FIELDNAME = 'CLUSTR'
           AS4LOCAL = 'A'
           POSITION = '0011'
           ROLLNAME = 'INDX_CLSTR'
           DATATYPE = 'INT2'
           LENG = '000005'
           INTTYPE = 'X'
           INTLEN = '000002'
           )
          (
           TABNAME = 'ZEXPIMP_TEST1'
           FIELDNAME = 'CONNID'
           AS4LOCAL = 'A'
           KEYFLAG = 'X'
           POSITION = '0004'
           ROLLNAME = 'S_FLCONN'
           DATATYPE = 'NUMC'
           LENG = '000004'
           INTTYPE = 'N'
           INTLEN = '000008'
           )
          (
           TABNAME = 'ZEXPIMP_TEST1'
           FIELDNAME = 'DEP_TIME'
           AS4LOCAL = 'A'
           KEYFLAG = 'X'
           POSITION = '0006'
           ROLLNAME = 'S_DEP_TIME'
           DATATYPE = 'TIMS'
           LENG = '000006'
           INTTYPE = 'T'
           INTLEN = '000012'
           )
          (
           TABNAME = 'ZEXPIMP_TEST1'
           FIELDNAME = 'ENDDT'
           AS4LOCAL = 'A'
           POSITION = '0010'
           ROLLNAME = 'SYDATS'
           DATATYPE = 'DATS'
           LENG = '000008'
           INTTYPE = 'D'
           INTLEN = '000016'
           )
          (
           TABNAME = 'ZEXPIMP_TEST1'
           FIELDNAME = 'FLDATE'
           AS4LOCAL = 'A'
           KEYFLAG = 'X'
           POSITION = '0005'
           ROLLNAME = 'S_FLDATE1'
           DATATYPE = 'DATS'
           LENG = '000008'
           INTTYPE = 'D'
           INTLEN = '000016'
           )
          (
           TABNAME = 'ZEXPIMP_TEST1'
           FIELDNAME = 'LANG'
           AS4LOCAL = 'A'
           KEYFLAG = 'X'
           POSITION = '0007'
           ROLLNAME = 'SYLANGU'
           DATATYPE = 'LANG'
           LENG = '000001'
           INTTYPE = 'C'
           INTLEN = '000002'
           )
          (
           TABNAME = 'ZEXPIMP_TEST1'
           FIELDNAME = 'LOEKZ'
           AS4LOCAL = 'A'
           POSITION = '0009'
           ROLLNAME = 'SYCHAR01'
           DATATYPE = 'CHAR'
           LENG = '000001'
           INTTYPE = 'C'
           INTLEN = '000002'
           )
          (
           TABNAME = 'ZEXPIMP_TEST1'
           FIELDNAME = 'RELID'
           AS4LOCAL = 'A'
           KEYFLAG = 'X'
           POSITION = '0002'
           ROLLNAME = 'INDX_RELID'
           DATATYPE = 'CHAR'
           LENG = '000002'
           INTTYPE = 'C'
           INTLEN = '000004'
           )
          (
           TABNAME = 'ZEXPIMP_TEST1'
           FIELDNAME = 'SRTF2'
           AS4LOCAL = 'A'
           KEYFLAG = 'X'
           POSITION = '0008'
           ROLLNAME = 'INDX_SRTF2'
           DATATYPE = 'INT4'
           LENG = '000010'
           INTTYPE = 'X'
           INTLEN = '000004'
           )
          (
           tabname = 'ZEXPIMP_TEST2'
           fieldname = 'CARRID'
           as4local = 'A'
           keyflag = 'X'
           position = '0003'
           rollname = 'S_CARR_ID'
           datatype = 'CHAR'
           leng = '000003'
           inttype = 'C'
           intlen = '000006'
           )
          (
           tabname = 'ZEXPIMP_TEST2'
           fieldname = 'CLIENT'
           as4local = 'A'
           keyflag = 'X'
           position = '0001'
           datatype = 'CLNT'
           leng = '000003'
           inttype = 'C'
           intlen = '000006'
           )
          (
           tabname = 'ZEXPIMP_TEST2'
           fieldname = 'CLUSTD'
           as4local = 'A'
           position = '0009'
           rollname = 'INDX_CLUST_BLOB'
           datatype = 'RSTR'
           inttype = 'y'
           intlen = '000008'
           )
          (
           tabname = 'ZEXPIMP_TEST2'
           fieldname = 'CONNID'
           as4local = 'A'
           keyflag = 'X'
           position = '0004'
           rollname = 'S_FLCONN'
           datatype = 'NUMC'
           leng = '000004'
           inttype = 'N'
           intlen = '000008'
           )
          (
           tabname = 'ZEXPIMP_TEST2'
           fieldname = 'DEP_TIME'
           as4local = 'A'
           keyflag = 'X'
           position = '0006'
           rollname = 'S_DEP_TIME'
           datatype = 'TIMS'
           leng = '000006'
           inttype = 'T'
           intlen = '000012'
           )
          (
           tabname = 'ZEXPIMP_TEST2'
           fieldname = 'ENDDT'
           as4local = 'A'
           position = '0008'
           rollname = 'SYDATS'
           datatype = 'DATS'
           leng = '000008'
           inttype = 'D'
           intlen = '000016'
           )
          (
           tabname = 'ZEXPIMP_TEST2'
           fieldname = 'FLDATE'
           as4local = 'A'
           keyflag = 'X'
           position = '0005'
           rollname = 'S_FLDATE1'
           datatype = 'DATS'
           leng = '000008'
           inttype = 'D'
           intlen = '000016'
           )
          (
           tabname = 'ZEXPIMP_TEST2'
           fieldname = 'LOEKZ'
           as4local = 'A'
           position = '0007'
           rollname = 'SYCHAR01'
           datatype = 'CHAR'
           leng = '000001'
           inttype = 'C'
           intlen = '000002'
           )
          (
           tabname = 'ZEXPIMP_TEST2'
           fieldname = 'RELID'
           as4local = 'A'
           keyflag = 'X'
           position = '0002'
           rollname = 'INDX_RELID'
           datatype = 'CHAR'
           leng = '000002'
           inttype = 'C'
           intlen = '000004'
           )
          (
           tabname = 'ZEXPIMP_TEST3'
           fieldname = 'CARRID'
           as4local = 'A'
           keyflag = 'X'
           position = '0003'
           rollname = 'S_CARR_ID'
           datatype = 'CHAR'
           leng = '000003'
           inttype = 'C'
           intlen = '000006'
           )
          (
           tabname = 'ZEXPIMP_TEST3'
           fieldname = 'CLIENT'
           as4local = 'A'
           keyflag = 'X'
           position = '0001'
           datatype = 'CLNT'
           leng = '000003'
           inttype = 'C'
           intlen = '000006'
           )
          (
           tabname = 'ZEXPIMP_TEST3'
           fieldname = 'CLUSTD'
           as4local = 'A'
           position = '0009'
           rollname = 'INDX_CLUST'
           datatype = 'LRAW'
           leng = '002886'
           inttype = 'X'
           intlen = '002886'
           )
          (
           tabname = 'ZEXPIMP_TEST3'
           fieldname = 'CLUSTR'
           as4local = 'A'
           position = '0008'
           rollname = 'INDX_CLSTR'
           datatype = 'INT2'
           leng = '000005'
           inttype = 'X'
           intlen = '000002'
           )
          (
           tabname = 'ZEXPIMP_TEST3'
           fieldname = 'ENDDT'
           as4local = 'A'
           position = '0007'
           rollname = 'SYDATS'
           datatype = 'DATS'
           leng = '000008'
           inttype = 'D'
           intlen = '000016'
           )
          (
           tabname = 'ZEXPIMP_TEST3'
           fieldname = 'LOEKZ'
           as4local = 'A'
           position = '0006'
           rollname = 'SYCHAR01'
           datatype = 'CHAR'
           leng = '000001'
           inttype = 'C'
           intlen = '000002'
           )
          (
           tabname = 'ZEXPIMP_TEST3'
           fieldname = 'POWER'
           as4local = 'A'
           keyflag = 'X'
           position = '0004'
           rollname = 'INT4'
           datatype = 'INT4'
           leng = '000010'
           inttype = 'X'
           intlen = '000004'
           )
          (
           tabname = 'ZEXPIMP_TEST3'
           fieldname = 'RELID'
           as4local = 'A'
           keyflag = 'X'
           position = '0002'
           rollname = 'INDX_RELID'
           datatype = 'CHAR'
           leng = '000002'
           inttype = 'C'
           intlen = '000004'
           )
          (
           tabname = 'ZEXPIMP_TEST3'
           fieldname = 'SRTF2'
           as4local = 'A'
           keyflag = 'X'
           position = '0005'
           rollname = 'INDX_SRTF2'
           datatype = 'INT4'
           leng = '000010'
           inttype = 'X'
           intlen = '000004'
           )
          (
           tabname = 'ZEXPIMP_TEST4'
           fieldname = 'CARRID'
           as4local = 'A'
           keyflag = 'X'
           position = '0002'
           rollname = 'S_CARR_ID'
           datatype = 'CHAR'
           leng = '000003'
           inttype = 'C'
           intlen = '000006'
           )
          (
           tabname = 'ZEXPIMP_TEST4'
           fieldname = 'CLUSTD'
           as4local = 'A'
           position = '0005'
           rollname = 'INDX_CLUST'
           datatype = 'LRAW'
           leng = '002886'
           inttype = 'X'
           intlen = '002886'
           )
          (
           tabname = 'ZEXPIMP_TEST4'
           fieldname = 'CLUSTR'
           as4local = 'A'
           position = '0004'
           rollname = 'INDX_CLSTR'
           datatype = 'INT2'
           leng = '000005'
           inttype = 'X'
           intlen = '000002'
           )
          (
           tabname = 'ZEXPIMP_TEST4'
           fieldname = 'RELID'
           as4local = 'A'
           keyflag = 'X'
           position = '0001'
           rollname = 'INDX_RELID'
           datatype = 'CHAR'
           leng = '000002'
           inttype = 'C'
           intlen = '000004'
           )
          (
           tabname = 'ZEXPIMP_TEST4'
           fieldname = 'SRTF2'
           as4local = 'A'
           keyflag = 'X'
           position = '0003'
           rollname = 'INDX_SRTF2'
           datatype = 'INT4'
           leng = '000010'
           inttype = 'X'
           intlen = '000004'
           )
          (
           tabname = 'ZEXPIMP_TEST5'
           fieldname = 'CARRID'
           as4local = 'A'
           keyflag = 'X'
           position = '0002'
           rollname = 'S_CARR_ID'
           datatype = 'CHAR'
           leng = '000003'
           inttype = 'C'
           intlen = '000006'
           )
          (
           tabname = 'ZEXPIMP_TEST5'
           fieldname = 'CLUSTD'
           as4local = 'A'
           position = '0003'
           rollname = 'INDX_CLUST_BLOB'
           datatype = 'RSTR'
           inttype = 'y'
           intlen = '000008'
           )
          (
           tabname = 'ZEXPIMP_TEST5'
           fieldname = 'RELID'
           as4local = 'A'
           keyflag = 'X'
           position = '0001'
           rollname = 'INDX_RELID'
           datatype = 'CHAR'
           leng = '000002'
           inttype = 'C'
           intlen = '000004'
           )
          (
           tabname = 'ZEXPIMP_TEST6'
           fieldname = 'CARRID'
           as4local = 'A'
           keyflag = 'X'
           position = '0002'
           rollname = 'S_CARR_ID'
           datatype = 'CHAR'
           leng = '000003'
           inttype = 'C'
           intlen = '000006'
           )
          (
           tabname = 'ZEXPIMP_TEST6'
           fieldname = 'CLUSTD'
           as4local = 'A'
           position = '0004'
           rollname = 'INDX_CLUST_BLOB'
           datatype = 'RSTR'
           inttype = 'y'
           intlen = '000008'
           )
          (
           tabname = 'ZEXPIMP_TEST6'
           fieldname = 'RELID'
           as4local = 'A'
           keyflag = 'X'
           position = '0001'
           rollname = 'INDX_RELID'
           datatype = 'CHAR'
           leng = '000002'
           inttype = 'C'
           intlen = '000004'
           )
          (
           tabname = 'ZEXPIMP_TEST6'
           fieldname = 'SRTF2'
           as4local = 'A'
           keyflag = 'X'
           position = '0003'
           rollname = 'INDX_SRTF2'
           datatype = 'INT4'
           leng = '000010'
           inttype = 'X'
           intlen = '000004'
           )
          ).


    environment->insert_test_data( dd03l_s ).

  ENDMETHOD.

  METHOD test.

    cl_abap_unit_assert=>assert_true( zcl_expimp_table=>is_valid_expimp_table( tabname = 'ZEXPIMP_TEST1' ) ).
    cl_abap_unit_assert=>assert_true( zcl_expimp_table=>is_valid_expimp_table( tabname = 'ZEXPIMP_TEST2' ) ).
    cl_abap_unit_assert=>assert_false( zcl_expimp_table=>is_valid_expimp_table( tabname = 'ZEXPIMP_TEST3' ) ).
    cl_abap_unit_assert=>assert_true( zcl_expimp_table=>is_valid_expimp_table( tabname = 'ZEXPIMP_TEST4' ) ).
    cl_abap_unit_assert=>assert_true( zcl_expimp_table=>is_valid_expimp_table( tabname = 'ZEXPIMP_TEST5' ) ).
    cl_abap_unit_assert=>assert_false( zcl_expimp_table=>is_valid_expimp_table( tabname = 'ZEXPIMP_TEST6' ) ).

  ENDMETHOD.

  METHOD class_teardown.

    environment->destroy( ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_check_known_standard_table DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test FOR TESTING.

ENDCLASS.

CLASS ltc_check_known_standard_table IMPLEMENTATION.

  METHOD test.

    cl_abap_unit_assert=>assert_true( zcl_expimp_table=>is_valid_expimp_table( tabname = 'VARI' ) ).
    cl_abap_unit_assert=>assert_true( zcl_expimp_table=>is_valid_expimp_table( tabname = 'STXL' ) ).
    cl_abap_unit_assert=>assert_true( zcl_expimp_table=>is_valid_expimp_table( tabname = 'EUFUNC' ) ).
    cl_abap_unit_assert=>assert_true( zcl_expimp_table=>is_valid_expimp_table( tabname = 'EUDB' ) ).

  ENDMETHOD.

ENDCLASS.
