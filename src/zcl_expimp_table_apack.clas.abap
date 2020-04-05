CLASS zcl_expimp_table_apack DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_apack_manifest.

    METHODS: constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_expimp_table_apack IMPLEMENTATION.

  METHOD constructor.
    if_apack_manifest~descriptor = VALUE #(
        group_id     = 'github.com/sandraros'
        artifact_id  = 'Export-Import-Tables'
        version      = '0.2'
        repository_type = 'abapGit'
        git_url      = 'https://github.com/sandraros/Export-Import-Tables.git'
        dependencies = VALUE #( ) ).
  ENDMETHOD.

ENDCLASS.
