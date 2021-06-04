INTERFACE zif_expimp_vx
  PUBLIC .

  "! Version <ul>
  "! <li>06: >= 6.10 UNICODE - Lengths are up to 4 bytes - 2 bytes per character?</li>
  "! <li>05: >= 3.0G and < 6.10 ??? - Lengths are on 2 bytes - 1 byte per character?</li>
  "! <li>04: >= 3.0G and < 6.10 ???</li>
  "! <li>03: <= 3.0F ???</li>
  "! <li>02: <= 3.0F ???</li>
  "! <li>01: <= 3.0F ???</li>
  "! </ul>
  "! Miscellaneous <ul>
  "! <li>ABAP release notes: In Release 3.x, the ABAP types 1 and 2 were still supported in some areas in a very basic manner to retain R/2 compatibility. This is no longer the case in Release 4.0</li>
  "! <li>note 178482: The data types TYP1 and TYP2 are no longer allowed for data specifications. Instead, you must use the type D. Otherwise, a syntax error occurs.</li>
  "! </ul>
  TYPES ty_version TYPE x LENGTH 1.
  CONSTANTS: BEGIN OF c_version,
               v06 TYPE ty_version VALUE '06',
               v05 TYPE ty_version VALUE '05',
               v04 TYPE ty_version VALUE '04',
               v03 TYPE ty_version VALUE '03',
               v02 TYPE ty_version VALUE '02',
               v01 TYPE ty_version VALUE '01',
             END OF c_version.

  "! <ul>
  "! <li>01: BIG ENDIAN</li>
  "! <li>02: LITTLE ENDIAN</li>
  "! </ul>
  TYPES ty_intformat TYPE x LENGTH 1.
  CONSTANTS: BEGIN OF c_intformat,
               big_endian    TYPE ty_intformat VALUE '01',
               little_endian TYPE ty_intformat VALUE '02',
             END OF c_intformat.

  "! <ul>
  "! <li>01: BIG ENDIAN</li>
  "! <li>02: LITTLE ENDIAN</li>
  "! </ul>
  TYPES ty_floatformat TYPE x LENGTH 1.
  CONSTANTS: BEGIN OF c_floatformat,
               big_endian    TYPE ty_floatformat VALUE '01',
               little_endian TYPE ty_floatformat VALUE '02',
             END OF c_floatformat.

  "! <ul>
  "! <li>01: not compressed</li>
  "! <li>02: compressed</li>
  "! </ul>
  TYPES ty_compress TYPE x LENGTH 1.
  CONSTANTS: BEGIN OF c_compress,
               not_compressed TYPE ty_compress VALUE '01',
               compressed     TYPE ty_compress VALUE '02',
             END OF c_compress.

  TYPES: ty_byte TYPE x LENGTH 1.


* (1) Transport header

  "! Header (16 bytes)
  TYPES:
    BEGIN OF ty_transport_header,
      "! Always 'FF'
      id              TYPE ty_byte,
      "! Version
      version         TYPE ty_version,
      "! Integer format
      intformat       TYPE ty_intformat,
      "! Float format
      floatformat     TYPE ty_floatformat,
      "! Compress
      compress        TYPE ty_compress,
      "! Data description
      datadescription TYPE ty_byte,
      unused1         TYPE x LENGTH 2,
      "! SAP Code Page (4 numeric characters in US-ASCII)
      codepage        TYPE x LENGTH 4,
      unused2         TYPE x LENGTH 4,
    END OF ty_transport_header.

ENDINTERFACE.
