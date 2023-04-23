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

*    data(cut) = zcl_abap_unit_runner=>create( ).
*
*    cut->run_class( iv_class_name = 'test' ).
*
*    cl_aunit_assert=>assert_equals(
*      EXPORTING
*        exp                  = '                                    " Data Object with Expected Type
*        act                  =                                     " Data Object with Current Value
**        msg                  =                                     " Message in Case of Error
**        level                = if_aunit_constants=>severity-medium " Error Severity
**        tol                  =                                     " Tolerance Range for Floating Point Numbers
**        quit                 = if_aunit_constants=>quit-test       " Flow Control in Case of Error
**        ignore_hash_sequence = abap_false                          " Ignore change sequence in hash tables
**      RECEIVING
**        assertion_failed     =                                     " Condition not met
*    ).

  ENDMETHOD.

ENDCLASS.
