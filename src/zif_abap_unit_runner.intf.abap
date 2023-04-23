INTERFACE zif_abap_unit_runner
  PUBLIC .

  TYPES:
    ty_runner_result TYPE c LENGTH 4.

  CONSTANTS:
    BEGIN OF co_runner_result_vals,
      pass TYPE ty_runner_result VALUE 'PASS',
      fail TYPE ty_runner_result VALUE 'FAIL',
    END OF co_runner_result_vals.

  METHODS run_class
    IMPORTING
      !iv_class_name    TYPE seoclsname
      !iv_email_address TYPE ad_smtpadr OPTIONAL
    RETURNING
      VALUE(rv_result)  TYPE ty_runner_result
    RAISING
      zcx_abap_unit_runner.
ENDINTERFACE.
