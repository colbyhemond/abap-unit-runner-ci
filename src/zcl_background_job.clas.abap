CLASS zcl_background_job DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_background_job .

    CLASS-METHODS create
      IMPORTING
        iv_name            TYPE btcjob
      RETURNING
        VALUE(ro_instance) TYPE REF TO zif_background_job.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_name TYPE btcjob.
    DATA mv_number TYPE btcjobcnt.

ENDCLASS.



CLASS ZCL_BACKGROUND_JOB IMPLEMENTATION.


  METHOD create.

    DATA(background_job) = NEW zcl_background_job( ).
    background_job->mv_name = iv_name.

    ro_instance = background_job.

  ENDMETHOD.


  METHOD zif_background_job~close.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = mv_number
        jobname              = mv_name
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
      DATA msg_text TYPE bapi_msg.
      CASE sy-subrc.
        WHEN 1. msg_text = |Background Job Close: Cant Start Immediate|.
        WHEN 2. msg_text = |Background Job Close: Invalid Start Date|.
        WHEN 3. msg_text = |Background Job Close: Job Name Missing|.
        WHEN 4. msg_text = |Background Job Close: Job Close Failed|.
        WHEN 5. msg_text = |Background Job Close: Job No Steps|.
        WHEN 6. msg_text = |Background Job Close: Job Not Executed|.
        WHEN 7. msg_text = |Background Job Close: Lock Failed|.
        WHEN OTHERS. msg_text = |Background Job Close: Unknown Exception|.
      ENDCASE.

      DATA(bg_exception) = NEW zcx_background_job( ).

      DATA(message_container) = bg_exception->get_msg_container( ).
      message_container->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E' " Message Type - defined by GCS_MESSAGE_TYPE
          iv_msg_text = msg_text  " Message Text
      ).

      RAISE EXCEPTION bg_exception.

    ENDIF.

  ENDMETHOD.


  METHOD zif_background_job~get_joblog.

    CALL FUNCTION 'BP_JOBLOG_READ'
      EXPORTING
*       client                = SY-MANDT         " Job Clients
        jobcount              = mv_number            " Job identification no.
*       joblog                = space            " Name of Job Log in TemSe Database
        jobname               = mv_name            " Job Name
*       lines                 =                  " No. of Lines
*       direction             =                  " Read Direction (B = From Beginning, E = From End)
      TABLES
        joblogtbl             = rt_result                 " Job Log Entries in List Format
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
      DATA msg_text TYPE bapi_msg.
      CASE sy-subrc.
        WHEN 1. msg_text = |Background Job Log: Cant Read Job Log|.
        WHEN 2. msg_text = |Background Job Log: Job Number Missing|.
        WHEN 3. msg_text = |Background Job Log: Job Log Does Not Exist|.
        WHEN 4. msg_text = |Background Job Log: Job Log Is Empty|.
        WHEN 5. msg_text = |Background Job Log: Job Log Name Missing|.
        WHEN 6. msg_text = |Background Job Log: Job Name Missing|.
        WHEN 7. msg_text = |Background Job Log: Job Does Not Exist|.
        WHEN OTHERS. msg_text = |Background Job Log: Unknown Exception|.
      ENDCASE.

      DATA(bg_exception) = NEW zcx_background_job( ).

      DATA(message_container) = bg_exception->get_msg_container( ).
      message_container->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E' " Message Type - defined by GCS_MESSAGE_TYPE
          iv_msg_text = msg_text  " Message Text
      ).

      RAISE EXCEPTION bg_exception.
    ENDIF.

  ENDMETHOD.


  METHOD zif_background_job~get_status.

    " Status':
    "" F - Finished
    "" S - Released
    "" R - Running
    "" Y - Ready
    "" P - Scheduled
    "" A - Aborted
    "" Z - Put Active
    "" X - Unknown State

    CALL FUNCTION 'BP_JOB_STATUS_GET'
      EXPORTING
        jobcount                   = mv_number                 " Job ID
        jobname                    = mv_name                 " Background job name
*       read_only_status           =
      IMPORTING
        status                     = rv_result                 " State of Background Job
*       has_child                  =                  " Flag: Job Has Child Jobs
      EXCEPTIONS
        job_doesnt_exist           = 1
        unknown_error              = 2
        parent_child_inconsistency = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      DATA msg_text TYPE bapi_msg.
      CASE sy-subrc.
        WHEN 1. msg_text = |Background Job Status: Job Does Not Exist|.
        WHEN 2. msg_text = |Background Job Status: Unknown Error|.
        WHEN 3. msg_text = |Background Job Status: Oarent Child Inconsistency|.
        WHEN OTHERS. msg_text = |Background Job Status: Unknown Exception|.
      ENDCASE.

      DATA(bg_exception) = NEW zcx_background_job( ).

      DATA(message_container) = bg_exception->get_msg_container( ).
      message_container->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E' " Message Type - defined by GCS_MESSAGE_TYPE
          iv_msg_text = msg_text  " Message Text
      ).

      RAISE EXCEPTION bg_exception.
    ENDIF.

  ENDMETHOD.


  METHOD zif_background_job~is_running.

    DATA(status) = me->zif_background_job~get_status( ).

    IF status = 'R'.
      rv_result = abap_true.
      RETURN.
    ENDIF.

    rv_result = abap_false.

  ENDMETHOD.


  METHOD zif_background_job~open.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = mv_name
      IMPORTING
        jobcount         = mv_number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      DATA msg_text TYPE bapi_msg.
      CASE sy-subrc.
        WHEN 1. msg_text = |Background Job Open: Cant Create Job|.
        WHEN 2. msg_text = |Background Job Open: Invalid Job Data|.
        WHEN 3. msg_text = |Background Job Open: Job Name Missing|.
        WHEN OTHERS. msg_text = |Background Job Open: Unknown Exception|.
      ENDCASE.

      DATA(bg_exception) = NEW zcx_background_job( ).

      DATA(message_container) = bg_exception->get_msg_container( ).
      message_container->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E' " Message Type - defined by GCS_MESSAGE_TYPE
          iv_msg_text = msg_text  " Message Text
      ).

      RAISE EXCEPTION bg_exception.
    ENDIF.

    rv_number = mv_number.

  ENDMETHOD.
ENDCLASS.
