INTERFACE zif_background_job
  PUBLIC .

  TYPES ty_joblog_table TYPE STANDARD TABLE OF tbtc5
    WITH DEFAULT KEY.

  METHODS open
    RETURNING
      VALUE(rv_number) TYPE btcjobcnt
    RAISING
      zcx_background_job.

  METHODS close
    RAISING
      zcx_background_job.

  METHODS get_status
    RETURNING
      VALUE(rv_result) TYPE btcstatus
    RAISING
      zcx_background_job.

  METHODS get_joblog
    RETURNING
      VALUE(rt_result) TYPE ty_joblog_table
    RAISING
      zcx_background_job.

  METHODS is_running
    RETURNING
      VALUE(rv_result) TYPE btcstatus
    RAISING
      zcx_background_job.

ENDINTERFACE.
