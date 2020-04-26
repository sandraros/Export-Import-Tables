CLASS zcx_expimp_table DEFINITION
  INHERITING FROM cx_static_check
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_text REDEFINITION.
    CONSTANTS:
      table_does_not_exist       TYPE sotr_conc VALUE '0800271CCEE91EDAA0C0942F145143BE',
      export_too_many_objects    TYPE sotr_conc VALUE '0800271CCEE91EDAA0C65754BBAD86C6',
      not_an_export_import_table TYPE sotr_conc VALUE '0800271CCEE91EDAA0C6028C47A7469A',
      export_data_buffer_error   TYPE sotr_conc VALUE '0800271CCEE91EDAA1823B6128B5DA40',
      database_error             TYPE sotr_conc VALUE '0800271CCEE91EDAA1823C2A76631A40',
      import_format_error        TYPE sotr_conc VALUE '690BB8396F051547E10000000A11447B'.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_EXPIMP_TABLE IMPLEMENTATION.


  METHOD get_text.
    CASE textid.
      WHEN table_does_not_exist.
        result = 'Table does not exist'(001).
      WHEN export_too_many_objects.
        result = 'Export of 1000 data objects maximum'(002).
      WHEN not_an_export_import_table.
        result = 'Table is not of type export/import'(003).
      WHEN export_data_buffer_error.
        result = 'Error during export to data buffer'(004).
      WHEN database_error.
        result = 'Error during database operation'(005).
      WHEN import_format_error.
        result = 'Import format error'(006).
      WHEN OTHERS.
        super->get_text( ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
