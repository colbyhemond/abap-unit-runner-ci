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
ENDCLASS.



CLASS ZCL_ABAP_UNIT_RUNNER IMPLEMENTATION.


  METHOD create.
    instance = NEW zcl_abap_unit_runner( ).
  ENDMETHOD.


  METHOD zif_abap_unit_runner~run_class.

    DATA: number           TYPE tbtcjob-jobcount,
          name             TYPE tbtcjob-jobname VALUE '/CI/ABAP_UNIT_RUNNER',
          print_parameters TYPE pri_params.

    DATA email_address TYPE ad_smtpadr.

    IF iv_email_address = space.
      SELECT adr6~smtp_addr
        FROM adr6
       INNER JOIN usr21
          ON usr21~persnumber = adr6~persnumber
         AND usr21~addrnumber = adr6~addrnumber
       WHERE usr21~bname = @sy-uname
        INTO @email_address.
      ENDSELECT.
    ELSE.
      email_address = iv_email_address.
    ENDIF.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = name
      IMPORTING
        jobcount         = number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc = 0.

      SUBMIT rs_aucv_runner
        VIA JOB name NUMBER number
        WITH b_obj = abap_true
        WITH so_class-low = iv_class_name
        WITH so_email-low = email_address
      AND RETURN.

      IF sy-subrc = 0.
        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = number
            jobname              = name
            strtimmed            = 'X'
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            OTHERS               = 8.
        IF sy-subrc <> 0.
*          MESSAGE |Error on Job Close. Exception { sy-subrc }.| TYPE 'E'.
          RAISE EXCEPTION TYPE zcx_abap_unit_runner.
        ENDIF.
      ELSE.
        DATA(msg) = cl_abap_submit_handling=>get_error_message( ).
        RAISE EXCEPTION TYPE zcx_abap_unit_runner.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_abap_unit_runner.
    ENDIF.

    DATA status TYPE btcstatus.

    DO.

      """Get Job Status and wait until finished
      CALL FUNCTION 'BP_JOB_STATUS_GET'
        EXPORTING
          jobcount                   = number                 " Job ID
          jobname                    = name                 " Background job name
*         read_only_status           =
        IMPORTING
          status                     = status                 " State of Background Job
*         has_child                  =                  " Flag: Job Has Child Jobs
        EXCEPTIONS
          job_doesnt_exist           = 1
          unknown_error              = 2
          parent_child_inconsistency = 3
          OTHERS                     = 4.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_abap_unit_runner.
      ENDIF.

      " Status':
      "" F - Finished
      "" S - Released
      "" R - Running
      "" Y - Ready
      "" P - Scheduled
      "" A - Aborted
      "" Z - Put Active
      "" X - Unknown State
      IF status <> 'R'.
        EXIT.
      ENDIF.

    ENDDO.

    """ Read Job Log
    DATA joblog_entries TYPE STANDARD TABLE OF tbtc5.

    CALL FUNCTION 'BP_JOBLOG_READ'
      EXPORTING
*       client                = SY-MANDT         " Job Clients
        jobcount              = number            " Job identification no.
*       joblog                = space            " Name of Job Log in TemSe Database
        jobname               = name            " Job Name
*       lines                 =                  " No. of Lines
*       direction             =                  " Read Direction (B = From Beginning, E = From End)
      TABLES
        joblogtbl             = joblog_entries                 " Job Log Entries in List Format
      EXCEPTIONS
        cant_read_joblog      = 1
        jobcount_missing      = 2
        joblog_does_not_exist = 3                " Log Not Found in TemSe Database
        joblog_is_empty       = 4                " Log is Empty
        joblog_name_missing   = 5
        jobname_missing       = 6                " Job Name Not Specified
        job_does_not_exist    = 7                " Job Already Deleted
        OTHERS                = 8.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_unit_runner.
    ENDIF.

    LOOP AT joblog_entries ASSIGNING FIELD-SYMBOL(<log_entry>).
      IF <log_entry>-text CS 'Email has been sent' AND <log_entry>-text CS 'emails were sent'.
        rv_result = zif_abap_unit_runner=>co_runner_result_vals-fail.
        RETURN.
      ENDIF.
    ENDLOOP.

    rv_result = zif_abap_unit_runner=>co_runner_result_vals-pass.

  ENDMETHOD.
ENDCLASS.
