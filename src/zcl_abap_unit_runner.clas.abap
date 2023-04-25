CLASS zcl_abap_unit_runner DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abap_unit_runner.

    CLASS-METHODS:
      create
        RETURNING
          VALUE(instance) TYPE REF TO zif_abap_unit_runner.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:
      select_email_address
        RETURNING
          VALUE(rv_result) TYPE ad_smtpadr.

    METHODS:
      submit_runner
        IMPORTING
          iv_name          TYPE tbtcjob-jobname
          iv_number        TYPE tbtcjob-jobcount
          iv_class_name    TYPE seoclsname
          iv_email_address TYPE ad_smtpadr.
ENDCLASS.



CLASS ZCL_ABAP_UNIT_RUNNER IMPLEMENTATION.


  METHOD create.
    instance = NEW zcl_abap_unit_runner( ).
  ENDMETHOD.


  METHOD zif_abap_unit_runner~run_class.

    DATA: name TYPE tbtcjob-jobname VALUE '/CI/ABAP_UNIT_RUNNER'.
    DATA email_address TYPE ad_smtpadr.

    TRY.

        IF iv_email_address = space.
          email_address = select_email_address( ).
        ELSE.
          email_address = iv_email_address.
        ENDIF.

        DATA(background_job) = zcl_background_job=>create( name ).

        DATA(number) = background_job->open( ).

        submit_runner(
          EXPORTING
            iv_name          = name
            iv_number        = number
            iv_class_name    = iv_class_name
            iv_email_address = email_address
        ).

        IF sy-subrc = 0.
          background_job->close( ).
        ELSE.
          DATA(msg) = cl_abap_submit_handling=>get_error_message( ).
          RAISE EXCEPTION TYPE zcx_abap_unit_runner.
        ENDIF.

        WAIT UNTIL background_job->is_running( ) = abap_false UP TO 10 SECONDS.

        DATA(joblog_entries) = background_job->get_joblog( ).

        IF lines( joblog_entries ) = 0.
          "try one more time
          WAIT UP TO 1 SECONDS.
          joblog_entries = background_job->get_joblog( ).
        ENDIF.

        LOOP AT joblog_entries ASSIGNING FIELD-SYMBOL(<log_entry>).
          IF <log_entry>-text CS 'Email has been sent'.
            rv_result = zif_abap_unit_runner=>co_runner_result_vals-fail.
            RETURN.
          ENDIF.
        ENDLOOP.

        rv_result = zif_abap_unit_runner=>co_runner_result_vals-pass.

      CATCH zcx_background_job INTO DATA(bg_exception).

        RAISE EXCEPTION TYPE zcx_abap_unit_runner
          EXPORTING
            message_container = bg_exception->get_msg_container( ).

    ENDTRY.

  ENDMETHOD.


  METHOD select_email_address.

    SELECT adr6~smtp_addr
      FROM adr6
     INNER JOIN usr21
        ON usr21~persnumber = adr6~persnumber
       AND usr21~addrnumber = adr6~addrnumber
     WHERE usr21~bname = @sy-uname
      INTO @rv_result.
    ENDSELECT.

  ENDMETHOD.


  METHOD submit_runner.

    SUBMIT rs_aucv_runner
       VIA JOB iv_name NUMBER iv_number
      WITH b_obj = abap_true
      WITH so_class-low = iv_class_name
      WITH so_email-low = iv_email_address
    AND RETURN.

  ENDMETHOD.
ENDCLASS.
