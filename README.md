# Export-Import-Tables
API (class `ZCL_EXPIMP_TABLE`) to enhance `IMPORT ... FROM DATABASE ...` and `EXPORT ... TO DATABASE ...` to read and write Export/Import Tables:
- To indicate the table name and the area dynamically
- To read the exact data contained in Export/Import Tables even if you don't know the exact types (by using the standard magical method `cl_abap_expimp_utilities=>dbuf_import_create_data`). Ever wanted to see easily what is in tables `AQGDB`, `BALDAT`, `EUDB`, `EUFUNC`, `INDX`, `LTDX`, `STXL`, `VARI`, `VRSX`, etc.? It's very easy now with the program `Z_EXPIMP_TABLE_DEMO_VIEW`.
  - Note that `cl_abap_expimp_utilities=>dbuf_import_create_data` is limited and can produce the runtime error `CONNE_IMPORT_ILL_DESCRIPTION` on old data (see [issue #5](https://github.com/sandraros/Export-Import-Tables/issues/5)).

The class `ZCL_EXPIMP_TABLE` has the following methods:
- `export_all`
  - Writes the data objects to the Export/Import table
- `import_all`
  - Reads the data objects from the Export/Import table
- `get_info`
  - Returns information about the export/import table
- `get_keys`
  - Improvement of method `db_get_keys` of class `cl_abap_expimp_utilities=>db_get_keys` (if you experience some issues with it)
- `import_as_xstring`
  - Returns the raw data of Export/Import Tables
- `is_valid_expimp_table`
  - Same as `GET_INFO` but returns a boolean
- `create_keytab_for_get_keys`
  - Utility to help you use the methods `*_GET_KEYS` of class `cl_abap_expimp_utilities`

Demo programs:
- Program `Z_EXPIMP_TABLE_DEMO_VIEW`
  - It displays the decoded contents of any Export/Import Table
- Program `Z_EXPIMP_TABLE_DEMO_INTERNAL`
  - It displays all Test Data from function module DATE_GET_WEEK, its input parameter DATE, and optionally its output parameter WEEK for regression testing.

# Dependencies
None

# Installation
Use [abapGit](https://github.com/abapGit/abapGit)

# Used by
https://github.com/sandraros/FM-Test-Data
