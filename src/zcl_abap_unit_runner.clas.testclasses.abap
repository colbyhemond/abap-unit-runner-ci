*"* use this source file for your ABAP unit test classes

CLASS ltcl_failed_test DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS:
      setup,
      teardown,
      first_test FOR TESTING RAISING cx_static_check.


ENDCLASS.

CLASS ltcl_failed_test IMPLEMENTATION.

  METHOD setup.

  ENDMETHOD.

  METHOD teardown.

  ENDMETHOD.

  METHOD first_test.

    cl_aunit_assert=>assert_equals( exp = abap_true act = abap_false ).

  ENDMETHOD.

ENDCLASS.
