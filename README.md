# Export-Import-Tables
Utility for Export/Import Tables

Restriction 1:
- With `IMPORT ... FROM DATABASE ...`
  - you cannot indicate the table name and the area dynamically
  - you must define the exact types statically in your program, that is fine but sometimes you don't know the exact types
- Solution:
  - Use the method `IMPORT_ALL` or `IMPORT_AS_XSTRING` of `ZCL_EXPIMP_TABLE`
  - Example in program `Z_EXPIMP_TABLE_DEMO`

The class `ZCL_EXPIMP_TABLE` has the following methods:
- create_keytab_for_get_keys
- export_all
  - the parameter TAB_CPAR is a table of all objects to export
- get_info
  - Returns information about the export/import table
- get_keys
- import_all
  - the parameter TAB_CPAR is a table of all objects to export
- import_as_xstring
  - 
- is_valid_expimp_table
  - Same as `GET_INFO` but returns a boolean
