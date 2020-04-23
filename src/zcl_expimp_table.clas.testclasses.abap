*"* use this source file for your ABAP unit test classes

class ltc_main definition
      for testing
      duration short
      risk level harmless.
  private section.
    methods test for testing.
*    class-methods class_setup.
*    class-methods class_teardown.
*    methods setup.
*    methods teardown.
endclass.

class ltc_main implementation.
  method test.

    " cl_abap_unit_assert=>assert_equals( ACT = ? EXP = ? MSG = ? ).
  endmethod.
endclass.
