INTERFACE zif_expimp_v6
  PUBLIC .

  INTERFACES zif_expimp_vx.

  ALIASES:
  ty_byte FOR zif_expimp_vx~ty_byte.

  "==========================================================
  "
  "             code below taken from RSINDX00
  "
  "==========================================================

***** IMPLEMENTATION OF C-STRUCTURES FROM ABCONNE.C

* (2) Object header

*     release 2.2 to 3.0F

  "! <ul>
  "! <li>00: C (characters)</li>
  "! <li>01: D (date)</li>
  "! <li>02: P (packed number in BCD)</li>
  "! <li>03: T (time)</li>
  "! <li>04: X (bytes)</li>
  "! <li>05: h (internal table)</li>
  "! <li>06: N (numeric characters)</li>
  "! <li>07: F (binary floating number)</li>
  "! <li>08: I (4-bytes integer)</li>
  "! <li>09: s/int2 (2-bytes integer)</li>
  "! <li>0A: b/int1 (1-byte integer)</li>
  "! <li>0B: w (wide character - obsolete)</li>
  "! <li>0C: typ1?</li>
  "! <li>0D: typ2?</li>
  "! <li>0E: u (flat structure)</li>
  "! <li>0F: v (deep structure)</li>
  "! <li>10: ref?</li>
  "! <li>11: obj1?</li>
  "! <li>12: obj2?</li>
  "! <li>13: g/string (string of characters)</li>
  "! <li>14: y/xstring (string of bytes)</li>
  "! <li>15: fref?</li>
  "! <li>16: iref?</li>
  "! <li>17: decfloat16 (decimal floating number with 16-digit mantissa)</li>
  "! <li>18: decfloat34 (decimal floating number with 34-digit mantissa)</li>
  "! <li>1B: 8/int8 (8-bytes integer)</li>
  "! </ul>
  TYPES ty_ityp TYPE ty_byte.
  CONSTANTS: BEGIN OF c_ityp,
               char       TYPE ty_ityp VALUE '00',
               date       TYPE ty_ityp VALUE '01',
               packed     TYPE ty_ityp VALUE '02',
               time       TYPE ty_ityp VALUE '03',
               hex        TYPE ty_ityp VALUE '04',
               table      TYPE ty_ityp VALUE '05',
               num        TYPE ty_ityp VALUE '06',
               float      TYPE ty_ityp VALUE '07',
               int        TYPE ty_ityp VALUE '08',
               int2       TYPE ty_ityp VALUE '09',
               int1       TYPE ty_ityp VALUE '0A',
               w          TYPE ty_ityp VALUE '0B',
               u1         TYPE ty_ityp VALUE '0C', " type 1 ?
               u2         TYPE ty_ityp VALUE '0D', " type 2 ?
               struct1    TYPE ty_ityp VALUE '0E',
               struct2    TYPE ty_ityp VALUE '0F',
               ref        TYPE ty_ityp VALUE '10',
               obj1       TYPE ty_ityp VALUE '11',
               obj2       TYPE ty_ityp VALUE '12',
               string     TYPE ty_ityp VALUE '13',
               xstring    TYPE ty_ityp VALUE '14',
               fref       TYPE ty_ityp VALUE '15',
               iref       TYPE ty_ityp VALUE '16',
               decfloat16 TYPE ty_ityp VALUE '17',
               decfloat34 TYPE ty_ityp VALUE '18',
               unknown1   TYPE ty_ityp VALUE '19',
               unknown2   TYPE ty_ityp VALUE '1A',
               int8       TYPE ty_ityp VALUE '1B',
             END OF c_ityp.

  "! <ul>
  "! <li>01 : primitive</li>
  "! <li>02 : flat structure</li>
  "! <li>03 : flat table</li>
  "! <li>04 : end of data</li>
  "! <li>05 : deep structure</li>
  "! <li>06 : deep table</li>
  "! <li>07 : string</li>
  "! </ul>
  TYPES ty_object_id TYPE ty_byte.
  CONSTANTS: BEGIN OF c_object_id,
               primitive      TYPE ty_object_id VALUE '01',
               flat_structure TYPE ty_object_id VALUE '02',
               flat_table     TYPE ty_object_id VALUE '03',
               end_of_data    TYPE ty_object_id VALUE '04',
               deep_structure TYPE ty_object_id VALUE '05',
               deep_table     TYPE ty_object_id VALUE '06',
               string         TYPE ty_object_id VALUE '07',
             END OF c_object_id.

  TYPES:
    "! Object header
    BEGIN OF ty_object_header,
      "! Field type category
      id    TYPE ty_object_id,
      "! Field type
      ityp  TYPE ty_ityp,
      "! Decimals for type P
      decs  TYPE ty_byte,
      "! Field length in number of bytes
      leng  TYPE x LENGTH 4,
      "! Length of the whole Object block including the header, useful to reach directly the next object; or zero if it's the last object
      next  TYPE x LENGTH 4,
      "! Length of object name, in number of characters
      nlen  TYPE ty_byte,
      "! Hash key for UID
      thash TYPE x LENGTH 4,
      "! Type identifier (check info)
      typid TYPE x LENGTH 16,
    END OF ty_object_header.

  TYPES:
    "! ID of data description
    "! <ul>
    "! <li>A0 : begin of structure include</li>
    "! <li>A1 : end of structure include</li>
    "! <li>A2 : begin of boxed component</li>
    "! <li>A3 : end of boxed component</li>
    "! <li>AA : primitive field</li>
    "! <li>AB : begin of structure</li>
    "! <li>AC : end of structure</li>
    "! <li>AD : begin of table</li>
    "! <li>AE : end of table</li>
    "! <li>AF : filler</li>
    "! </ul>
    ty_dd_id TYPE ty_byte,
    "! data description
    BEGIN OF ty_data_description,
      id   TYPE ty_dd_id,
      type TYPE ty_byte,
      decs TYPE ty_byte,
      flen TYPE x LENGTH 4,
    END OF ty_data_description.

  CONSTANTS:
    BEGIN OF c_dd_id,
      BEGIN OF structure_include,
        start TYPE ty_dd_id VALUE 'A0',
        end   TYPE ty_dd_id VALUE 'A1',
      END OF structure_include,
      BEGIN OF boxed_component,
        start TYPE ty_dd_id VALUE 'A2',
        end   TYPE ty_dd_id VALUE 'A3',
      END OF boxed_component,
      primitive TYPE ty_dd_id VALUE 'AA',
      BEGIN OF structure,
        start TYPE ty_dd_id VALUE 'AB',
        end   TYPE ty_dd_id VALUE 'AC',
      END OF structure,
      BEGIN OF table,
        start TYPE ty_dd_id VALUE 'AD',
        end   TYPE ty_dd_id VALUE 'AE',
      END OF table,
      filler    TYPE ty_dd_id VALUE 'AF',
    END OF c_dd_id.

  TYPES:
    "! ID of data value
    "! <ul>
    "! <li>A0 : begin of structure include</li>
    "! <li>A1 : end of structure include</li>
    "! <li>A2 : begin of boxed component</li>
    "! <li>A3 : end of boxed component</li>
    "! <li>AA : primitive field</li>
    "! <li>AB : begin of structure</li>
    "! <li>AC : end of structure</li>
    "! <li>AD : begin of table</li>
    "! <li>AE : end of table</li>
    "! <li>AF : filler</li>
    "! </ul>
    ty_dv_id TYPE ty_byte,
    "! data area headers
    BEGIN OF ty_dah,

      BEGIN OF interval,
        id  TYPE ty_dv_id,
        len TYPE x LENGTH 4,
      END OF interval,

      BEGIN OF table,
        id    TYPE ty_dv_id,
        len   TYPE x LENGTH 4,
        lines TYPE x LENGTH 4,
      END OF table,

      BEGIN OF string_xstring,
        id  TYPE ty_dv_id,
        len TYPE x LENGTH 4,
      END OF string_xstring,

    END OF ty_dah.

  CONSTANTS:
    BEGIN OF c_dv_id,
      "! Whole length of Data Description
      single TYPE ty_dd_id VALUE 'BB',
      BEGIN OF interval,
        "! Spans several fields
        start TYPE ty_dd_id VALUE 'BC',
        end   TYPE ty_dd_id VALUE 'BD',
      END OF interval,
      BEGIN OF table,
        start TYPE ty_dd_id VALUE 'BE',
        end   TYPE ty_dd_id VALUE 'BF',
      END OF table,
      BEGIN OF string_xstring,
        start TYPE ty_dd_id VALUE 'CA',
        end   TYPE ty_dd_id VALUE 'CB',
      END OF string_xstring,
      BEGIN OF boxed_component,
        start TYPE ty_dd_id VALUE 'CC',
        end   TYPE ty_dd_id VALUE 'CD',
      END OF boxed_component,
    END OF c_dv_id.

ENDINTERFACE.
