*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations



CONSTANTS c_selection_screen_changed_on TYPE sydatum VALUE '20100720'.


CLASS test_runner DEFINITION DEFERRED.

TYPES:
  BEGIN OF ty_stat_package,
    name     TYPE string,
    new_line TYPE abap_bool,
    BEGIN OF state,
      executed_classes TYPE i,
      executed_methods TYPE i,
      skipped_methods  TYPE i,
    END OF state,
  END OF ty_stat_package,

  ty_stat_packages TYPE STANDARD TABLE OF ty_stat_package WITH NON-UNIQUE KEY name,
  ty_packages      TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY,

  BEGIN OF ty_context,
    package          TYPE string,
    program          TYPE string,
    obj_type         TYPE string,
    obj_name         TYPE string,
    test_class       TYPE string,
    test_method      TYPE string,
    adt_resource_uri TYPE string,
  END OF ty_context,

  ty_alert_level TYPE c LENGTH 6,

  BEGIN OF ty_alert,
    context     TYPE ty_context,
    kind        TYPE string,
    description TYPE string,
    level       TYPE ty_alert_level,
    apply_zebra TYPE abap_bool,
  END OF ty_alert,

  ty_alerts TYPE STANDARD TABLE OF ty_alert WITH DEFAULT KEY,

  BEGIN OF ty_test_method,
    name  TYPE string,
    alert TYPE ty_alert,
    BEGIN OF state,
      has_been_started TYPE abap_bool,
      has_been_skipped TYPE abap_bool,
    END OF state,
  END OF ty_test_method,
  ty_methods TYPE STANDARD TABLE OF ty_test_method WITH NON-UNIQUE KEY name,

  BEGIN OF ty_icon,
    passed            TYPE string,
    fatal_failure     TYPE string,
    critical_failure  TYPE string,
    tolerable_failure TYPE string,
    skipped           TYPE string,
    syntax_error      TYPE string,
    no_permission     TYPE string,
  END OF ty_icon,

  BEGIN OF ty_test_class,
    name         TYPE string,
    handle       TYPE REF TO if_aunit_test_class_handle,
    test_methods TYPE ty_methods,
    BEGIN OF state,
      has_been_started           TYPE abap_bool,
      BEGIN OF issue,
        has_been_skipped TYPE abap_bool,
        has_rt_failure   TYPE abap_bool,
        has_timeout      TYPE abap_bool,
        has_failure      TYPE abap_bool,
      END OF issue,
      count_exec_methods         TYPE i,
      count_skipped_methods      TYPE i,
      count_skipped_over_methods TYPE i,
      count_no_permission        TYPE i,
    END OF state,
  END OF ty_test_class,

  ty_test_classes TYPE STANDARD TABLE OF ty_test_class WITH NON-UNIQUE KEY name,

  BEGIN OF ty_program,
    name             TYPE progname,
    obj_type         TYPE tadir-object,
    obj_name         TYPE tadir-obj_name,
    package          TYPE tadir-devclass,
    adt_resource_uri TYPE string,
    test_classes     TYPE ty_test_classes,
    is_permitted     TYPE abap_bool,
    BEGIN OF state,
      has_been_started TYPE abap_bool,
      has_issue        TYPE abap_bool,
    END OF state,
  END OF ty_program,

  ty_programs TYPE STANDARD TABLE OF ty_program WITH NON-UNIQUE KEY obj_name obj_type
    WITH UNIQUE SORTED KEY sorted COMPONENTS obj_name obj_type,

  BEGIN OF ty_syntax_error,
    obj_type         TYPE tadir-object,
    obj_name         TYPE tadir-obj_name,
    adt_resource_uri TYPE string,
    message          TYPE string,
    line             TYPE i,
    token            TYPE string,
  END OF ty_syntax_error,

  ty_syntax_errors TYPE STANDARD TABLE OF ty_syntax_error WITH KEY obj_name obj_type,

  BEGIN OF ty_time_interval,
    started_on  TYPE sy-datlo,
    started_at  TYPE t,
    finished_on TYPE sy-datlo,
    finished_at TYPE sy-timlo,
    time_zone   TYPE sy-zonlo,
  END OF ty_time_interval,

  BEGIN OF ty_statistics,
    cnt_packages      TYPE i,
    cnt_programs      TYPE i,
    cnt_test_classes  TYPE i,
    cnt_test_methods  TYPE i,
    cnt_no_permission TYPE i,

    BEGIN OF cnt_method,
      passed         TYPE i,
      with_fatal     TYPE i,
      with_critical  TYPE i,
      with_tolerable TYPE i,
      skipped        TYPE i,
    END OF cnt_method,

    " message
    has_timeout_only  TYPE abap_bool,
    BEGIN OF cnt_failure,
      syntax_error TYPE i,
      fatal        TYPE i,
      critical     TYPE i,
      tolerable    TYPE i,
      total        TYPE i,
    END OF cnt_failure,

    packages          TYPE ty_stat_packages,

  END OF ty_statistics.


CONSTANTS:
  BEGIN OF c_detail,
    no    TYPE c LENGTH 1 VALUE '',
    basic TYPE c LENGTH 1 VALUE 'S',
    full  TYPE c LENGTH 1 VALUE 'X',
  END OF c_detail.

CONSTANTS:
  BEGIN OF c_level,
    skipped   TYPE ty_alert_level VALUE '1-SKIP' ##no_Text,
    tolerable TYPE ty_alert_level VALUE '2-TOLE' ##no_Text,
    critical  TYPE ty_alert_level VALUE '3-CRIT' ##no_Text,
    fatal     TYPE ty_alert_level VALUE '4-FATA' ##no_Text,
  END OF c_level.

CONSTANTS:
  BEGIN OF c_alert_id,
    prerequisite TYPE c LENGTH 4 VALUE 'FM04' ##no_Text,
    assumption   TYPE c LENGTH 4 VALUE 'FM05' ##no_Text,
    timeout      TYPE c LENGTH 4 VALUE 'WM20' ##no_Text,
  END OF c_alert_id.


INTERFACE lif_aunit_test.
ENDINTERFACE.

***********************************************************************

INTERFACE mail_listener.

  INTERFACES: if_aunit_listener.

  TYPES:
    BEGIN OF ty_layout,
      with_syntax_errors     TYPE abap_bool,
      with_no_permissions    TYPE abap_bool,
      with_execution_details TYPE abap_bool,
      with_failure_details   TYPE abap_bool,
      with_adt_uri           TYPE abap_bool,
    END OF ty_layout.

  METHODS:
    send_email
      RAISING cx_cmp_failure,

    has_failure
      RETURNING VALUE(result) TYPE abap_bool,

    has_timeout
      RETURNING VALUE(result) TYPE abap_bool,

    has_warning
      RETURNING VALUE(result) TYPE abap_bool.


ENDINTERFACE.


CLASS globalization_service DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_text,

        BEGIN OF title,
          detailed_log      TYPE string,
          overview          TYPE string,
          failures          TYPE string,
          method_statistics TYPE string,
          packages          TYPE string,
          syntax_errors     TYPE string,
          no_permissions    TYPE string,
        END OF title,

        BEGIN OF label,
          client               TYPE string,
          description          TYPE string,
          failures_critical    TYPE string,
          failures_fatal       TYPE string,
          failures_tolerable   TYPE string,
          finished_on          TYPE string,
          finished_at          TYPE string,
          level                TYPE string,
          object_name          TYPE string,
          object_type          TYPE string,
          package              TYPE string,
          packages             TYPE string,
          passed_tests         TYPE string,
          programs             TYPE string,
          no_permission        TYPE string,
          runtime_error        TYPE string,
          skipped              TYPE string,
          started_by           TYPE string,
          started_on           TYPE string,
          started_at           TYPE string,
          syntax_errors        TYPE string,
          syntax_error_message TYPE string,
          system               TYPE string,
          test_class           TYPE string,
          test_classes         TYPE string,
          test_method          TYPE string,
          test_methods         TYPE string,
          timeout              TYPE string,
          variant              TYPE string,
          with_fatal           TYPE string,
          with_critical        TYPE string,
          with_tolerable       TYPE string,
        END OF label,
        BEGIN OF detail,
          overall_success         TYPE string,
          skipped_methods         TYPE string,
          skipped_further_methods TYPE string,
        END OF detail,
      END OF ty_text.

    CLASS-METHODS:
      get_text
        IMPORTING i_contain_alerts_skipped_tests TYPE abap_bool
        RETURNING VALUE(result)                  TYPE ty_text.

ENDCLASS.

CLASS test_runner DEFINITION.
  PUBLIC SECTION.
    DATA:
      f_programs      TYPE ty_programs READ-ONLY,
      f_syntax_errors TYPE ty_syntax_errors READ-ONLY,
      f_au_factory    TYPE REF TO cl_aunit_factory READ-ONLY.
    METHODS:
      run,
      get_params IMPORTING i_ref_params TYPE REF TO cl_sat_prog_param,
      set_params IMPORTING i_ref_params TYPE REF TO cl_sat_prog_param,
      reset_params.
  PROTECTED SECTION.
    DATA f_programs_without_tests TYPE ty_programs.
    METHODS:
      run_direct,
      run_email,

      select_objects,
      select_objects_by_pkg,
      select_objects_by_type,
      select_objects_by_pkg_and_type
        IMPORTING i_packages TYPE ty_packages,

      execute_unit_tests_4_mail
        EXPORTING e_listener TYPE REF TO mail_listener,

      check_programs_without_tests,
      get_test_class_handles,
      settle_objects_after_selection.
ENDCLASS.


CLASS email_listener DEFINITION CREATE PRIVATE FRIENDS lif_aunit_test.
  PUBLIC SECTION.
    INTERFACES mail_listener.

    CLASS-METHODS:
      create_listener
        IMPORTING i_programs      TYPE ty_programs
                  i_syntax_errors TYPE ty_syntax_errors OPTIONAL
                  i_au_factory    TYPE REF TO cl_aunit_factory
        RETURNING VALUE(result)   TYPE REF TO mail_listener.

  PRIVATE SECTION.
    ALIASES:
      ty_layout FOR mail_listener~ty_layout,
      send_email FOR mail_listener~send_email,
      has_failure FOR mail_listener~has_failure,
      has_timeout FOR mail_listener~has_timeout,
      has_warning FOR mail_listener~has_warning.


    METHODS:
      handle_failure
        IMPORTING i_kind        TYPE string
                  i_ref_failure TYPE REF TO if_aunit_info_failure,

      initialize_program_entry
        IMPORTING i_program_name TYPE csequence,

      finish_statistics,

      add_devc_uri_syntax_errors.


    CONSTANTS:
      BEGIN OF c_on_miss,
        ignore TYPE c LENGTH 6 VALUE 'ignore' ##no_Text,
        assert TYPE c LENGTH 6 VALUE 'assert' ##no_Text,
        create TYPE c LENGTH 6 VALUE 'create' ##no_Text,
      END OF c_on_miss.

    CONSTANTS:
      BEGIN OF c_kind,
        assert_failure TYPE string VALUE 'Assertion Failure' ##no_Text,
        warning        TYPE string VALUE 'Warning' ##no_Text,
        cx_failure     TYPE string VALUE 'Exception' ##no_Text,
        rt_failure     TYPE string VALUE 'Runtime Abortion' ##no_Text,
        skipped        TYPE string VALUE 'Skipped' ##no_Text,
        timeout        TYPE string VALUE 'Timeout' ##no_Text,
      END OF c_kind.

    DATA:
      f_programs      TYPE ty_programs,
      f_syntax_errors TYPE ty_syntax_errors,
      f_statistic     TYPE ty_statistics,
      f_alerts        TYPE ty_alerts,
      f_layout        TYPE ty_layout,
      f_time_interval TYPE ty_time_interval,

      f_test_context  TYPE ty_context,
      f_au_factory    TYPE REF TO cl_aunit_factory,
      f_text_api      TYPE REF TO if_aunit_text_description.

    METHODS:
      init,

      compute_icon_info
        RETURNING VALUE(result) TYPE ty_icon,

      compute_icon
        IMPORTING i_alt         TYPE string
                  i_title       TYPE string
                  i_color       TYPE string
        RETURNING VALUE(result) TYPE string.

ENDCLASS.


CLASS program_events DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      initialization,
      start_of_selection,
      at_selection_screen_output,
      at_selection_screen.

ENDCLASS.



**********************************************************************

CLASS test_runner IMPLEMENTATION.
  METHOD run.

    DATA g_test_runner TYPE REF TO test_runner.
    DATA: r_prog_param TYPE REF TO cl_sat_prog_param.
    DATA: b_email  TYPE s_aucv_d_send_email.
    DATA b_direct TYPE s_aucv_d_show_results.

    IF sy-batch IS INITIAL.
      g_test_runner->set_params( r_prog_param ).
    ENDIF.

    CALL FUNCTION 'DB_COMMIT'.
*
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = 'Select Objects ....'(p01).
    select_objects( ).

    IF me->f_programs IS INITIAL.
      "No objects were found
      RETURN.
    ENDIF.

    get_test_class_handles( ).

    IF ( abap_true EQ b_email ).
      check_programs_without_tests( ).
    ENDIF.

    IF me->f_programs IS INITIAL.
      IF b_email IS NOT INITIAL AND me->f_syntax_errors IS NOT INITIAL.
      ELSE.
        "No unit test class was found
        RETURN.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = 'Executing tests'(pex).

    CASE abap_true.
      WHEN b_direct.
        run_direct( ).
      WHEN b_email.
        run_email( ).
    ENDCASE.
  ENDMETHOD.


  METHOD get_params.
*    define l_mac_get.
*      i_ref_params->get_param_value( exporting i_name  = &1
*                                     changing  p_value = &2 ).
*    end-of-definition.
*
*    l_mac_get:
*      'RS_AUCV_R_DEVC' b_devc, "select by package
*      'RS_AUCV_R_OBJ' b_obj,
*      'RS_AUCV_R_EMAIL' b_email, "send mails
*      'RS_AUCV_R_DIRECT' b_direct, "results in dialog
*
** select by package
*      'RS_AUCV_SO_DEVC' so_devc[],
*      'RS_AUCV_C_PACKR' p_packr,
*      'RS_AUCV_C_EXCL' p_excl, "exclude selected objects
*      'RS_AUCV_SO_NDEVC' so_ndevc[],
*      'RS_AUCV_SO_NCL' so_ncl[],
*      'RS_AUCV_SO_NFG' so_nfg[],
*      'RS_AUCV_SO_NPR' so_npr[],
** select by prog
*      'RS_AUCV_SO_CLASS' so_class[],
*      'RS_AUCV_SO_FUGR' so_fugr[],
*      'RS_AUCV_SO_PROG' so_prog[].
*
** flag as default //see also the definition of b_devc + b_email: default 'X'
*    p_error = abap_true.
*    p_adturi = abap_true.
*    p_aucv = abap_true.
*    l_mac_get:
*     'RS_AUCV_C_ERROR' p_error, "send in error case
*     'RS_AUCV_C_ADTURI' p_adturi,
*     'RS_AUCV_C_AUCV' p_aucv.
** initialized
*    l_mac_get:
*      'RS_AUCV_D_RSKLVL' p_rsklvl, "risk level
*      'RS_AUCV_D_DURLVL' p_durlvl, "duration
*      'RS_AUCV_D_DETAIL' p_detail.
*
*    i_ref_params->get_param_value( exporting i_name  = 'RS_AUCV_SO_EMAIL'
*                                  changing  p_value = so_email[] ).
  ENDMETHOD.


  METHOD set_params.
*    define l_mac_set.
*      i_ref_params->set_param_value(
*       exPORTING
*         i_name = &1
*         i_value  = &2 ).
*    end-of-definition.
*
*    l_mac_set:
*      'RS_AUCV_R_DEVC' b_devc, "select by package
*      'RS_AUCV_R_OBJ' b_obj,
*      'RS_AUCV_R_EMAIL' b_email, "send mails
*      'RS_AUCV_R_DIRECT' b_direct, "results in dialog
*
** select by package
*      'RS_AUCV_SO_DEVC' so_devc[],
*      'RS_AUCV_C_PACKR' p_packr,
*      'RS_AUCV_C_EXCL' p_excl, "exclude selected objects
*      'RS_AUCV_SO_NDEVC' so_ndevc[],
*      'RS_AUCV_SO_NCL' so_ncl[],
*
*      'RS_AUCV_SO_NFG' so_nfg[],
*      'RS_AUCV_SO_NPR' so_npr[],
** select by prog
*      'RS_AUCV_SO_CLASS' so_class[],
*      'RS_AUCV_SO_FUGR' so_fugr[],
*      'RS_AUCV_SO_PROG' so_prog[],
*
*      'RS_AUCV_SO_EMAIL' so_email[],
*
*      'RS_AUCV_C_ERROR' p_error, "send in error case
*      'RS_AUCV_C_ADTURI' p_adturi,
*      'RS_AUCV_C_AUCV' p_aucv,
*
*      'RS_AUCV_D_RSKLVL' p_rsklvl, "risk level
*      'RS_AUCV_D_DURLVL' p_durlvl, "duration
*      'RS_AUCV_D_DETAIL' p_detail.
  ENDMETHOD.


  METHOD reset_params.
*    b_devc = abap_true. b_obj = abap_false. "radio group
*    b_email = abap_true. b_direct = abap_false. "radio group
*
*    clear:
*      p_packr, p_excl, p_error, p_error, p_error, p_detail,
*      so_devc, so_devc[], so_ndevc, so_ndevc[], so_ncl, so_ncl[], so_nfg, so_nfg[], so_npr, so_npr[],
*      so_class, so_class[], so_fugr, so_fugr[], so_prog, so_prog[],
*      so_email, so_email[].
*
*    p_rsklvl = cl_aunit_permission_control=>get_max_risk_level( ).
*    p_durlvl = if_aunit_attribute_enums=>c_duration-long.
*
*    so_email-low =  cl_aucv_job_utilities=>get_user_email( i_uname  = sy-uname ).
*    so_email-sign = 'I'.
*    so_email-option = 'EQ'.
*    insert so_email into table so_email[].
*
*    r_prog_param->delete_param_values( '*' ). "sy-mandt/repid/uname/ '*' all params
  ENDMETHOD.


  METHOD run_direct.
*    data:
*      tadir_keys type sabp_t_tadir_keys,
*      tadir_key  type sabp_s_tadir_key.
*    field-symbols:
*      <program>    type ty_program.
*
*    loop at me->f_programs assigning <program>.
*      tadir_key-obj_type = <program>-obj_type.
*      tadir_key-obj_name = <program>-obj_name.
*      insert tadir_key into table tadir_keys.
*    endloop.
*    call function 'SABP_AU_TEST_ITEMS_FROM_IDE'
*      exporting
*        tadir_keys                 = tadir_keys
*        with_coverage              = p_aucv
*        limit_on_risk_level        = p_rsklvl
*        limit_on_duration_category = p_durlvl.

  ENDMETHOD.


  METHOD run_email.
    DATA:
      mail_listener TYPE REF TO mail_listener,
      failure       TYPE REF TO cx_root.
    TRY.
        execute_unit_tests_4_mail( IMPORTING e_listener = mail_listener ).
        IF ( abap_true EQ mail_listener->has_timeout( ) ).
          " timeout due to byte code generation? retry give it one more chance
          execute_unit_tests_4_mail( IMPORTING e_listener = mail_listener ).
        ENDIF.
        mail_listener->send_email( ).
      CATCH cx_root INTO failure ##catch_All.
        cl_sat_ui_std_dialogue=>display_exception( failure ).
    ENDTRY.
  ENDMETHOD.


  METHOD execute_unit_tests_4_mail.

    DATA p_rsklvl TYPE saunit_d_allowed_risk_level.
    DATA p_durlvl TYPE saunit_d_allowed_rt_duration.

    DATA:
      duration TYPE if_aunit_task=>ty_s_duration_setting,
      au_task  TYPE REF TO if_aunit_task.
    FIELD-SYMBOLS:
      <program>    TYPE ty_program,
      <test_class> TYPE ty_test_class.

    e_listener = email_listener=>create_listener(
      i_programs = me->f_programs
      i_syntax_errors = me->f_syntax_errors
      i_au_factory = me->f_au_factory ).
    au_task =
       me->f_au_factory->create_task( listener = e_listener ).

    au_task->restrict_risk_level( p_rsklvl ).
    au_task->restrict_duration_category( p_durlvl ).

    LOOP AT me->f_programs ASSIGNING <program> WHERE is_permitted IS NOT INITIAL.
      LOOP AT <program>-test_classes ASSIGNING <test_class>.
        au_task->add_test_class_handle( <test_class>-handle ).
      ENDLOOP.
    ENDLOOP.

    au_task->run( mode = if_aunit_task=>c_run_mode-external ).

  ENDMETHOD.


  METHOD get_test_class_handles.
    DATA:
      syntax_error       TYPE ty_syntax_error,
      method_names       TYPE saunit_t_methods,
      method_name        TYPE saunit_d_method,
      method             TYPE ty_test_method,
      test_class         TYPE ty_test_class,
      test_class_handles
        TYPE if_aunit_test_class_handle=>ty_t_testclass_handles.
    FIELD-SYMBOLS:
      <program>         TYPE ty_program.

    CREATE OBJECT me->f_au_factory.

    LOOP AT me->f_programs ASSIGNING <program>.
      test_class_handles =
         me->f_au_factory->get_test_class_handles(
           obj_type = <program>-obj_type
           obj_name = <program>-obj_name  ).

      CLEAR test_class.
      LOOP AT test_class_handles INTO test_class-handle.

        test_class-name = test_class-handle->get_class_name( ).
        method_names = test_class-handle->get_test_methods( ).

        LOOP AT method_names INTO method-name.
          INSERT method INTO TABLE test_class-test_methods.
        ENDLOOP.

        INSERT test_class INTO TABLE <program>-test_classes.
        CLEAR test_class.
      ENDLOOP.

      <program>-is_permitted =
        me->f_au_factory->is_test_execution_permitted(
          object_key = VALUE #( obj_name = <program>-obj_name obj_type = <program>-obj_type )
          package_name = <program>-package ).
    ENDLOOP.

    me->f_programs_without_tests = me->f_programs.
    DELETE me->f_programs_without_tests WHERE test_classes IS NOT INITIAL.
    DELETE me->f_programs WHERE test_classes IS INITIAL.

  ENDMETHOD.


  METHOD check_programs_without_tests.

    DATA:
      syntax_error TYPE ty_syntax_error,
      dummy        TYPE c LENGTH 1.
    FIELD-SYMBOLS:
      <program> TYPE ty_program.

    LOOP AT me->f_programs_without_tests ASSIGNING <program>.
      CLEAR syntax_error.
      SELECT SINGLE r3state FROM reposrc INTO dummy
        WHERE progname = <program>-name AND
              r3state = 'A'.
      IF ( 0 NE sy-subrc ).
        CONTINUE.
      ENDIF.
      SYNTAX-CHECK FOR PROGRAM <program>-name
        MESSAGE syntax_error-message
        LINE syntax_error-line
        WORD syntax_error-token.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      syntax_error-obj_name = <program>-obj_name.
      syntax_error-obj_type = <program>-obj_type.
      INSERT syntax_error INTO TABLE me->f_syntax_errors.
    ENDLOOP.

  ENDMETHOD.


  METHOD select_objects_by_pkg.

    DATA so_devc    TYPE RANGE OF tdevc-devclass.
    DATA  p_packr TYPE s_aucv_d_with_subdevc.
    DATA p_excl  TYPE s_aucv_d_exclude_obj.
    DATA so_ndevc TYPE RANGE OF tdevc-devclass.

    DATA:
      package           TYPE devclass,
      selected_packages TYPE ty_packages,
      expanded_packages TYPE cl_pak_package_queries=>tt_subpackage_info,
      overall_packages  TYPE ty_packages.

    SELECT devclass FROM tdevc
      INTO TABLE selected_packages
      WHERE
        devclass IN so_devc.                            "#EC CI_GENBUFF

    IF abap_false EQ  p_packr. " no sub packages.
      overall_packages = selected_packages.
    ELSE.
      overall_packages = selected_packages.
      LOOP AT selected_packages INTO package.
        CLEAR expanded_packages.
        cl_pak_package_queries=>get_all_subpackages(
          EXPORTING
            im_package                    = package
          IMPORTING
            et_subpackages                = expanded_packages
          EXCEPTIONS
            OTHERS                        = 7 ).
        IF ( abap_true EQ p_excl AND so_ndevc IS NOT INITIAL ).
          DELETE expanded_packages
            WHERE table_line IN so_ndevc.               "#EC CI_SORTSEQ
        ENDIF.
        APPEND LINES OF expanded_packages TO overall_packages.
      ENDLOOP.
    ENDIF.

    SORT overall_packages.
    DELETE ADJACENT DUPLICATES FROM overall_packages.
    IF ( overall_packages IS NOT INITIAL ).
      select_objects_by_pkg_and_type( overall_packages ).
    ENDIF.

  ENDMETHOD.


  METHOD select_objects_by_pkg_and_type.
    DATA p_excl  TYPE s_aucv_d_exclude_obj.
    DATA so_ncl   TYPE RANGE OF seoaliases-clsname.
    DATA so_nfg   TYPE RANGE OF tlibg-area.
    DATA so_npr   TYPE RANGE OF s_aucv_d_prog_other.

    DATA:
      tab_rng_obj_name TYPE RANGE OF tadir-obj_name,
      package          TYPE devclass.

    ASSERT i_packages IS NOT INITIAL.

    IF abap_true EQ p_excl AND so_ncl IS NOT INITIAL.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass AS package
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->f_programs
        FOR ALL ENTRIES IN i_packages
        WHERE
          devclass = i_packages-table_line AND
          pgmid    = 'R3TR' AND
          object   = 'CLAS' AND
          obj_name NOT IN so_ncl[].
    ELSE.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass AS package
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->f_programs
        FOR ALL ENTRIES IN i_packages
        WHERE
          devclass = i_packages-table_line AND
          pgmid    = 'R3TR' AND
          object   = 'CLAS'.
    ENDIF.

    IF abap_true EQ p_excl AND so_nfg IS NOT INITIAL.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass AS package
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->f_programs
        FOR ALL ENTRIES IN i_packages
        WHERE
          devclass = i_packages-table_line AND
          pgmid    = 'R3TR' AND
          object   = 'FUGR' AND
          obj_name NOT IN so_nfg[].
    ELSE.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass AS package
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->f_programs
        FOR ALL ENTRIES IN i_packages
        WHERE
          devclass = i_packages-table_line AND
          pgmid    = 'R3TR' AND
          object   = 'FUGR'.
    ENDIF.

    IF abap_true EQ p_excl AND so_npr IS NOT INITIAL.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass AS package
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->f_programs
        FOR ALL ENTRIES IN i_packages
        WHERE
          devclass = i_packages-table_line AND
          pgmid    = 'R3TR' AND
          object   = 'PROG' AND
          obj_name NOT IN so_npr[].
    ELSE.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass AS package
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->f_programs
        FOR ALL ENTRIES IN i_packages
        WHERE
          devclass = i_packages-table_line AND
          pgmid    = 'R3TR' AND
          object   = 'PROG'.
    ENDIF.

    DELETE me->f_programs WHERE obj_type = 'CLAS' AND obj_name CS '='.
  ENDMETHOD.


  METHOD select_objects_by_type.
    DATA: p_selcl                TYPE abap_bool.
    DATA so_class TYPE RANGE OF seoaliases-clsname.
    DATA p_selprg               TYPE abap_bool.
    DATA so_prog TYPE RANGE OF s_aucv_d_prog_other.
    DATA p_selfg                TYPE abap_bool.
    DATA so_fugr TYPE RANGE OF tlibg-area.

    IF NOT p_selcl IS INITIAL.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass AS package
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->f_programs
        WHERE
          pgmid = 'R3TR' AND
          object = 'CLAS' AND
          obj_name IN so_class.
    ENDIF.

    IF NOT p_selprg IS INITIAL.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass AS package
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->f_programs
        WHERE
          pgmid = 'R3TR' AND
          object = 'PROG' AND
          obj_name IN so_prog.
    ENDIF.

    IF NOT p_selfg IS INITIAL.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass AS package
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->f_programs
        WHERE
          pgmid = 'R3TR' AND
          object = 'FUGR' AND
          obj_name  IN so_fugr.
    ENDIF.

  ENDMETHOD.


  METHOD settle_objects_after_selection.

    DATA: p_selcl                TYPE abap_bool.
    DATA so_class TYPE RANGE OF seoaliases-clsname.
    DATA p_selprg               TYPE abap_bool.
    DATA so_prog TYPE RANGE OF s_aucv_d_prog_other.
    DATA p_selfg                TYPE abap_bool.
    DATA so_fugr TYPE RANGE OF tlibg-area.

    IF ( p_selcl IS INITIAL ).
      DELETE me->f_programs
        WHERE
          obj_type = 'CLAS'.
    ELSE.
      DELETE me->f_programs
        WHERE
          obj_type = 'CLAS' AND
          obj_name NOT IN so_class.
    ENDIF.

    IF ( p_selprg IS INITIAL ).
      DELETE me->f_programs
        WHERE
          obj_type = 'PROG'.
    ELSE.
      DELETE me->f_programs
        WHERE
          obj_type = 'PROG' AND
          obj_name NOT IN so_prog.
    ENDIF.

    IF ( p_selfg IS INITIAL ).
      DELETE me->f_programs
        WHERE
          obj_type = 'FUGR'.
    ELSE.
      DELETE me->f_programs
        WHERE
          obj_type = 'FUGR' AND
          obj_name NOT IN so_fugr.
    ENDIF.

  ENDMETHOD.


  METHOD select_objects.

    DATA b_devc TYPE s_aucv_d_select_by_devc.
    DATA b_obj  TYPE s_aucv_d_select_by_prog.
    DATA p_excl  TYPE s_aucv_d_exclude_obj.


    DATA so_ndevc TYPE RANGE OF tdevc-devclass  .
    DATA so_ncl  TYPE RANGE OF seoaliases-clsname.
    DATA so_nfg  TYPE RANGE OF tlibg-area     .
    DATA so_npr TYPE RANGE OF s_aucv_d_prog_other .


    DATA:
      type_of_include TYPE trdir-subc.
    FIELD-SYMBOLS:
      <program>  TYPE ty_program.
    CONSTANTS:
      BEGIN OF c_include_type,
        stand_alone TYPE trdir-subc  VALUE 'I',
      END OF c_include_type.

    IF b_devc = abap_true.
      select_objects_by_pkg( ).
    ELSEIF b_obj = abap_true.
      select_objects_by_type( ).
    ENDIF.

    " excluded
    IF b_devc = abap_true AND p_excl = abap_true.
      IF NOT so_ndevc[] IS INITIAL.
        DELETE me->f_programs
           WHERE package IN so_ndevc.
      ENDIF.
      IF NOT so_ncl[] IS INITIAL.
        DELETE me->f_programs
           WHERE obj_type = 'CLAS' AND obj_name IN so_ncl.
      ENDIF.
      IF NOT so_nfg[] IS INITIAL.
        DELETE me->f_programs
           WHERE obj_type = 'FUGR' AND obj_name IN so_nfg.
      ENDIF.
      IF NOT so_npr[] IS INITIAL.
        DELETE me->f_programs
           WHERE obj_type = 'PROG' AND obj_name IN so_npr.
      ENDIF.
    ENDIF.

    LOOP AT me->f_programs ASSIGNING <program>.
      IF ( <program>-obj_type = 'PROG' ).
        " no includes
        SELECT SINGLE subc FROM trdir
          INTO type_of_include
          WHERE
            name = <program>-obj_name AND
            subc <> c_include_type-stand_alone.
        IF ( 0 NE sy-subrc ).
          CONTINUE.
        ENDIF.
      ENDIF.
      <program>-name  = cl_aunit_prog_info=>tadir_to_progname(
         obj_type = <program>-obj_type
         obj_name = <program>-obj_name ).
    ENDLOOP.
    DELETE me->f_programs WHERE name IS INITIAL.
    SORT me->f_programs BY package obj_type obj_name.
  ENDMETHOD.

ENDCLASS.


CLASS email_listener IMPLEMENTATION.

  DEFINE mac_assign_program.
    READ TABLE me->f_Programs
      ASSIGNING <program> WITH KEY sorted COMPONENTS
        obj_Type = me->f_Test_Context-obj_Type
        obj_Name = me->f_Test_Context-obj_Name.
    IF ( 0 NE sy-subrc ).
      READ TABLE me->f_Programs
        ASSIGNING <program> WITH KEY
          name = me->f_Test_Context-program.
    ENDIF.
    IF ( 0 NE sy-subrc ).
      CASE &1.
        WHEN c_On_Miss-create.
          INSERT INITIAL LINE INTO TABLE me->f_Programs ASSIGNING <program>.
          <program>-name =     me->f_Test_Context-program.
          <program>-obj_Type = me->f_Test_Context-obj_Type.
          <program>-obj_Name = me->f_Test_Context-obj_Name.
        WHEN c_On_Miss-ignore.
          UNASSIGN <program>.
        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.
    ENDIF.
  end-of-definition.

  DEFINE mac_assign_test_class.
    mac_Assign_Program &1.
    IF ( <Program> IS ASSIGNED ).
      READ TABLE <program>-test_Classes ASSIGNING <test_Class>
        WITH KEY name = me->f_Test_Context-test_Class.
      IF ( 0 NE sy-subrc ).
        CASE &1.
          WHEN c_On_Miss-create.
            INSERT INITIAL LINE INTO TABLE <program>-test_Classes ASSIGNING <test_Class>.
            <test_Class>-name = me->f_Test_Context-test_Class.
          WHEN c_On_Miss-ignore.
            UNASSIGN <Test_Class>.
          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.
      ENDIF.
    ELSE.
      UNASSIGN <Test_Class>.
    ENDIF.
  end-of-definition.

  DEFINE mac_assign_test_method.
    mac_Assign_Test_Class &1.
    IF ( <Test_Class> IS ASSIGNED ).
      READ TABLE <test_Class>-test_Methods ASSIGNING <test_Method>
        WITH KEY name = me->f_Test_Context-test_Method.
      IF ( 0 NE sy-subrc ).
        CASE &1.
          WHEN c_On_Miss-create.
            INSERT INITIAL LINE INTO TABLE <test_Class>-test_Methods ASSIGNING <test_Method>.
            <test_Method>-name = me->f_Test_Context-test_Method.
          WHEN c_On_Miss-ignore.
            UNASSIGN <test_Method>.
          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.
      ENDIF.
    ELSE.
      UNASSIGN <test_Method>.
    ENDIF.
  end-of-definition.


  METHOD create_listener.

    DATA: mail_listener TYPE REF TO email_listener.

    CREATE OBJECT mail_listener.
    mail_listener->f_programs = i_programs.
    mail_listener->f_syntax_errors = i_syntax_errors.
    mail_listener->f_statistic-cnt_failure-syntax_error = lines( i_syntax_errors ).

    IF ( i_au_factory IS BOUND ).
      mail_listener->f_au_factory =  i_au_factory.
    ELSE.
      CREATE OBJECT mail_listener->f_au_factory.
    ENDIF.
    mail_listener->init( ).
    result = mail_listener.

  ENDMETHOD.


  METHOD send_email.

    DATA p_error  TYPE s_aucv_d_error_case_only.
    DATA p_detail TYPE s_aucv_d_list_detail.
    DATA so_email TYPE RANGE OF ad_smtpadr.

    TYPES:
      BEGIN OF ty_no_permission,
        object   TYPE string,
        new_line TYPE abap_bool,
      END OF ty_no_permission,
      ty_no_permissions TYPE STANDARD TABLE OF ty_no_permission WITH DEFAULT KEY.

    DATA: contain_alerts_skipped_tests TYPE abap_bool.
    CONSTANTS:
      c_area     TYPE c LENGTH 4 VALUE 'MAIN',
      c_template TYPE syrepid VALUE 'RS_AUCV_RUNNER_MAIL_TEMPLATE'.
    DATA:
      name           TYPE string,
      xpt_caught     TYPE REF TO cx_root,
      html_composer  TYPE REF TO cl_cmp_composer,
      text_lines     TYPE cl_cmp_composer=>tab_code,
      string         TYPE string,
      title          TYPE string,
      icon_info      TYPE ty_icon,
      email          TYPE ad_smtpadr,
      no_permission  TYPE ty_no_permission,
      no_permissions TYPE ty_no_permissions,
      programs       TYPE ty_programs,
      alert          TYPE ty_alert.
    FIELD-SYMBOLS:
      <text>    LIKE LINE OF text_lines,
      <program> TYPE ty_program.

    " extract programs without sufficient permissions
    LOOP AT me->f_programs ASSIGNING <program> WHERE is_permitted IS INITIAL.
      CLEAR no_permission.
      no_permission-object = |{ <program>-obj_type }&nbsp{ <program>-obj_name }|.
      IF ( 0 EQ ( lines( no_permissions ) MOD 3 ) ).
        " every 3 programs a new line / aid rendering
        no_permission-new_line = abap_true.
      ENDIF.
      INSERT no_permission INTO TABLE no_permissions.
    ENDLOOP.
    IF ( no_permissions IS NOT INITIAL ).
      me->f_layout-with_no_permissions = abap_true.
      WHILE ( 0 <> ( lines( no_permissions ) MOD 3 ) ).
        INSERT INITIAL LINE INTO TABLE no_permissions.
      ENDWHILE.
    ENDIF.

    " reduce programs to permitted ones
    programs = me->f_programs.
    DELETE programs WHERE is_permitted IS INITIAL.

    IF p_error IS NOT INITIAL AND
       me->f_statistic-cnt_failure-fatal    IS INITIAL AND
       me->f_statistic-cnt_failure-critical IS INITIAL AND
       me->f_statistic-cnt_failure-tolerable IS INITIAL AND
       me->f_statistic-cnt_failure-syntax_error IS INITIAL AND
       no_permissions IS INITIAL.
      MESSAGE 'All unit tests passed successfully -> No Email'(k00) TYPE 'S'.
      EXIT.
    ENDIF.

    IF ( me->f_alerts IS NOT INITIAL ).
      me->f_layout-with_failure_details = abap_true.
    ENDIF.
    IF me->f_syntax_errors IS NOT INITIAL.
      me->f_layout-with_syntax_errors = abap_true.
    ENDIF.

    CASE p_detail.
      WHEN c_detail-full.
        IF ( programs IS NOT INITIAL ).
          me->f_layout-with_execution_details = abap_true.
        ENDIF.

      WHEN c_detail-basic.
        LOOP AT programs ASSIGNING <program> WHERE state-has_issue EQ abap_true.
          DELETE <program>-test_classes WHERE state-issue IS INITIAL.
        ENDLOOP.
        DELETE programs WHERE state-has_issue EQ abap_false OR test_classes IS INITIAL.
        IF ( programs IS NOT INITIAL ).
          me->f_layout-with_execution_details = abap_true.
        ENDIF.

      WHEN OTHERS.
        CLEAR programs.
        me->f_layout-with_execution_details = abap_false.
        DELETE me->f_alerts WHERE kind = c_kind-skipped.
    ENDCASE.

    READ TABLE me->f_alerts WITH KEY kind = c_kind-skipped TRANSPORTING NO FIELDS.
    IF ( 0 EQ sy-subrc ).
      contain_alerts_skipped_tests = abap_true.
    ELSE.
      contain_alerts_skipped_tests = abap_false.
    ENDIF.

    html_composer = cl_cmp_composer=>s_create( ).
    icon_info = compute_icon_info( ).

    DATA(globalized_text) = globalization_service=>get_text( contain_alerts_skipped_tests ).

    html_composer->add_var( i_name = :
       'i_Statistic'      i_value = me->f_statistic ),
       'i_Syntax_Errors'  i_value = me->f_syntax_errors ),
       'i_Alerts'         i_value = me->f_alerts ),
       'i_Programs'       i_value = programs ),
       'i_No_Permissions' i_value = no_permissions ),
       'i_Layout'         i_value = me->f_layout ),
       'i_Duration'       i_value = me->f_time_interval ),
       'i_Icon'           i_value = icon_info ),
       'i_Text'           i_value = globalized_text ).
    TRY.
        text_lines =
          html_composer->build_code(
            i_area             = c_area  " 'MAIN'
            i_template_include = c_template ).

      CLEANUP INTO xpt_caught.
        MESSAGE xpt_caught TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

    READ TABLE text_lines INTO title INDEX 1.
    DELETE text_lines INDEX 1.
    LOOP AT text_lines ASSIGNING <text>.
      CONDENSE <text>.
    ENDLOOP.

    cl_aucv_job_utilities=>send_emails(
      i_title         = title
      i_tab_rng_email = so_email[]
      i_tab_lines     = text_lines
      i_flg_html      = abap_true ).

    COMMIT WORK.
  ENDMETHOD.


  METHOD has_failure.
    READ TABLE me->f_alerts
      TRANSPORTING NO FIELDS
      WITH KEY kind = c_kind-assert_failure.
    IF ( 0 EQ sy-subrc ).
      result = abap_true.
      RETURN.
    ENDIF.

    READ TABLE me->f_alerts
      TRANSPORTING NO FIELDS
      WITH KEY kind = c_kind-cx_failure.
    IF ( 0 EQ sy-subrc ).
      result = abap_true.
      RETURN.
    ENDIF.

    READ TABLE me->f_alerts
      TRANSPORTING NO FIELDS
      WITH KEY kind = c_kind-rt_failure.
    IF ( 0 EQ sy-subrc ).
      result = abap_true.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD has_timeout.
    READ TABLE me->f_alerts
      TRANSPORTING NO FIELDS
      WITH KEY kind = c_kind-timeout.
    IF ( 0 EQ sy-subrc ).
      result = abap_true.
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD has_warning.
    READ TABLE me->f_alerts
      TRANSPORTING NO FIELDS
      WITH KEY kind = c_kind-warning.
    IF ( 0 EQ sy-subrc ).
      result = abap_true.
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD add_devc_uri_syntax_errors.
    DATA:
      devclass     TYPE tadir-devclass,
      package      TYPE ty_stat_package,
      escaped_name TYPE string.
    FIELD-SYMBOLS:
      <syntax_error> TYPE ty_syntax_error.

    CHECK me->f_syntax_errors IS NOT INITIAL.
    LOOP AT me->f_syntax_errors ASSIGNING <syntax_error>.
      SELECT SINGLE devclass
        FROM  tadir INTO devclass
         WHERE
           pgmid     = 'R3TR'     AND
           object    = <syntax_error>-obj_type   AND
           obj_name  = <syntax_error>-obj_name.         "#EC CI_GENBUFF
      READ TABLE me->f_statistic-packages
        WITH KEY name = devclass TRANSPORTING NO FIELDS.
      IF ( 0 NE sy-subrc ).
        package-name = devclass.
        INSERT package INTO TABLE me->f_statistic-packages.
        ADD 1 TO me->f_statistic-cnt_packages.
      ENDIF.
      IF abap_true EQ me->f_layout-with_adt_uri.
        escaped_name =
          escape( val = <syntax_error>-obj_name format = cl_abap_format=>e_uri_full ).
        CASE <syntax_error>-obj_type.
          WHEN 'CLAS'.
            <syntax_error>-adt_resource_uri =
             'adt://' && sy-sysid && '/sap/bc/adt/oo/classes/' && escaped_name ##no_Text.
          WHEN 'PROG'.
            <syntax_error>-adt_resource_uri =
             'adt://' && sy-sysid && '/sap/bc/adt/programs/programs/' && escaped_name ##no_Text.
          WHEN 'FUGR'.
            <syntax_error>-adt_resource_uri =
            'adt://' && sy-sysid && '/sap/bc/adt/functions/groups/' && escaped_name ##no_Text.
          WHEN OTHERS.
            CLEAR <syntax_error>-adt_resource_uri.
        ENDCASE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD compute_icon_info.
    DATA: base_url TYPE string.
    CONSTANTS:
      BEGIN OF c_alt,
        passed            TYPE string VALUE `&#x2714;` ##no_Text,
        skipped           TYPE string VALUE `&#x27a5;` ##no_Text,
        tolerable_failure TYPE string VALUE `&#x25ce;` ##no_Text,
        critical_failure  TYPE string VALUE `&#x25b2;` ##no_Text,
        fatal_failure     TYPE string VALUE `&#x2726;` ##no_Text,
        syntax_error      TYPE string VALUE `&#x2716;` ##no_Text, "9889
        no_permission     TYPE string VALUE `&#x26db;` ##no_Text,
      END OF c_alt,
      BEGIN OF c_color,
        passed            TYPE string VALUE `green` ##no_Text,
        skipped           TYPE string VALUE `grey` ##no_Text,
        tolerable_failure TYPE string VALUE `orange` ##no_Text,
        critical_failure  TYPE string VALUE `red` ##no_Text,
        fatal_failure     TYPE string VALUE `red` ##no_Text,
        syntax_error      TYPE string VALUE `red` ##no_Text,
        no_permission     TYPE string VALUE `orange` ##no_Text,
      END OF c_color,
      BEGIN OF c_title,
        passed            TYPE string VALUE `Test Passed` ##no_Text,
        skipped           TYPE string VALUE `Test Skipped` ##no_Text,
        tolerable_failure TYPE string VALUE `Tolerable Failure` ##no_Text,
        critical_failure  TYPE string VALUE `Critical Failure` ##no_Text,
        fatal_failure     TYPE string VALUE `Fatal Failure` ##no_Text,
        syntax_error      TYPE string VALUE `Syntax Error` ##no_Text,
        no_permission     TYPE string VALUE `No Permission` ##no_Text,
      END OF c_title.

    DEFINE mac_compute_icon.
      result-&1 =
        compute_Icon(
          i_alt = c_Alt-&1
          i_title = c_Title-&1
          i_color = c_Color-&1 ).

    end-of-definition.

    mac_compute_icon:
      passed,
      skipped,
      tolerable_failure,
      fatal_failure,
      critical_failure,
      syntax_error,
      no_permission.
  ENDMETHOD.


  METHOD compute_icon.
    result = `<b style="color:$">$</b>` ##no_Text.
    REPLACE FIRST OCCURRENCE OF '$' IN result WITH: i_color, i_alt.
  ENDMETHOD.


  METHOD init.

    DATA g_adt_uri_is_supported TYPE abap_bool.
    DATA p_adturi TYPE abap_bool.

    FIELD-SYMBOLS:
      <test_class>  TYPE ty_test_class,
      <test_method> TYPE ty_test_method,
      <program>     TYPE ty_program.

    me->f_text_api = me->f_au_factory->get_text_converter( language = 'E' ).
    IF ( abap_true EQ g_adt_uri_is_supported AND abap_true EQ p_adturi ).
      me->f_layout-with_adt_uri = abap_true.
    ELSE.
      me->f_layout-with_adt_uri = abap_false.
    ENDIF.

    LOOP AT me->f_programs ASSIGNING <program>.
      CLEAR <program>-state.
      LOOP AT <program>-test_classes ASSIGNING <test_class>.
        CLEAR <test_class>-state.
        LOOP AT <test_class>-test_methods ASSIGNING <test_method>.
          CLEAR <test_method>-state.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    CLEAR me->f_time_interval.
    GET TIME.
    me->f_time_interval-started_on = sy-datlo.
    me->f_time_interval-started_at = sy-timlo.
    me->f_time_interval-time_zone =  sy-zonlo.
  ENDMETHOD.


  METHOD if_aunit_listener~task_start.
    CLEAR:
      me->f_test_context,
      me->f_statistic,
      me->f_alerts.
  ENDMETHOD.


  METHOD if_aunit_listener~program_start.
    DATA:
      descr        TYPE if_aunit_text_description=>ty_s_description,
      package      TYPE ty_stat_package,
      message_text TYPE string.
    FIELD-SYMBOLS:
      <program>      TYPE ty_program.

    CLEAR me->f_test_context-test_method.

    descr = info->get_description( ).
    READ TABLE descr-params INDEX 1 INTO
       me->f_test_context-program.

    initialize_program_entry(  me->f_test_context-program ).
    mac_assign_program c_on_miss-assert.
    <program>-state-has_been_started = abap_true.
    me->f_test_context-obj_name = <program>-obj_name.
    me->f_test_context-obj_type = <program>-obj_type.
    me->f_test_context-package =  <program>-package.


    READ TABLE me->f_statistic-packages
      WITH KEY name = <program>-package TRANSPORTING NO FIELDS.
    IF ( 0 NE sy-subrc ).
      package-name = <program>-package.
      INSERT package INTO TABLE me->f_statistic-packages.
    ENDIF.
  ENDMETHOD.


  METHOD if_aunit_listener~class_start.
    DATA:
      descr         TYPE if_aunit_text_description=>ty_s_description.
    FIELD-SYMBOLS:
      <test_class> TYPE ty_test_class,
      <program>    TYPE ty_program.

    CLEAR me->f_test_context-test_class.
    CLEAR me->f_test_context-test_method.

    descr = info->get_description( ).
    READ TABLE descr-params
      INDEX 1 INTO me->f_test_context-test_class.
    mac_assign_test_class c_on_miss-create.
    <test_class>-state-has_been_started = abap_true.
  ENDMETHOD.


  METHOD if_aunit_listener~method_start.
    DATA:
      descr         TYPE if_aunit_text_description=>ty_s_description.
    FIELD-SYMBOLS:
      <test_method> TYPE ty_test_method,
      <test_class>  TYPE ty_test_class,
      <program>     TYPE ty_program.

    CLEAR me->f_test_context-test_method.

    descr = info->get_description( ).
    READ TABLE descr-params INDEX 1 INTO
       me->f_test_context-test_method.

    mac_assign_test_method c_on_miss-create.
    <test_method>-state-has_been_started = abap_true.
    ADD 1 TO <test_class>-state-count_exec_methods.
  ENDMETHOD.


  METHOD if_aunit_listener~method_end.
    FIELD-SYMBOLS:
      <program>     TYPE ty_program,
      <test_class>  TYPE ty_test_class,
      <test_method> TYPE ty_test_method.

    mac_assign_test_method c_on_miss-assert.
    IF ( <test_method>-alert IS INITIAL ).
      ADD 1 TO me->f_statistic-cnt_method-passed.
    ELSE.
      CASE <test_method>-alert-level.
        WHEN c_level-skipped.
          ADD 1 TO me->f_statistic-cnt_method-skipped.
          ADD 1 TO <test_class>-state-count_skipped_methods.
        WHEN c_level-fatal.
          ADD 1 TO me->f_statistic-cnt_method-with_fatal.
        WHEN c_level-critical.
          ADD 1 TO me->f_statistic-cnt_method-with_critical.
        WHEN c_level-tolerable.
          ADD 1 TO me->f_statistic-cnt_method-with_tolerable.
        WHEN OTHERS.
          CLEAR sy-subrc.
      ENDCASE.
    ENDIF.

    CLEAR:
      me->f_test_context-test_method.
  ENDMETHOD.


  METHOD if_aunit_listener~class_end.
    FIELD-SYMBOLS:
      <program>     TYPE ty_program,
      <test_class>  TYPE ty_test_class,
      <test_method> TYPE ty_test_method.

    mac_assign_test_class c_on_miss-assert.

    CASE abap_true.
      WHEN <test_class>-state-issue-has_been_skipped.
        LOOP AT <test_class>-test_methods ASSIGNING <test_method>
          WHERE
            state-has_been_started = abap_false.
          <test_method>-state-has_been_skipped = abap_true.
          ADD 1 TO me->f_statistic-cnt_method-skipped.
          ADD 1 TO <test_class>-state-count_skipped_methods.
          ADD 1 TO <test_class>-state-count_skipped_over_methods.
        ENDLOOP.
      WHEN <test_class>-state-issue-has_rt_failure OR
           <test_class>-state-issue-has_timeout.
        IF ( <test_class>-state-count_exec_methods IS INITIAL ).
          " a runtime error or timeout cancels to test sand box
          " without further information.
          " Unless the framework is sick at least one test method
          " must have been started. Align statistics accordingly.
          ADD 1 TO <test_class>-state-count_exec_methods.
          ADD 1 TO me->f_statistic-cnt_test_methods.
        ENDIF.
    ENDCASE.

    CLEAR:
      me->f_test_context-test_class,
      me->f_test_context-test_method.
  ENDMETHOD.


  METHOD if_aunit_listener~program_end.
    FIELD-SYMBOLS:
      <program>    TYPE ty_program,
      <package>    TYPE ty_stat_package,
      <test_class> TYPE ty_test_class.

    mac_assign_program c_on_miss-assert.
    CLEAR me->f_test_context.
    READ TABLE me->f_statistic-packages[]
      ASSIGNING <package>
      WITH KEY name = <program>-package.
    ASSERT <package> IS ASSIGNED.

    LOOP AT <program>-test_classes[]
      ASSIGNING <test_class>
      WHERE state-has_been_started = abap_true.
      <package>-state-executed_classes =
        <package>-state-executed_classes + 1.
      <package>-state-executed_methods =
        <package>-state-executed_methods + <test_class>-state-count_exec_methods.
      <package>-state-skipped_methods =
        <package>-state-skipped_methods + <test_class>-state-count_skipped_methods.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_aunit_listener~task_end.
    DELETE ADJACENT DUPLICATES FROM me->f_statistic-packages.
    finish_statistics( ).
  ENDMETHOD.


  METHOD handle_failure.
    DATA:
      adjust_method_totals TYPE abap_bool,
      native_level         TYPE aunit_level,
      alert                TYPE ty_alert,
      descr                TYPE if_aunit_text_description=>ty_s_description,
      count                TYPE i.
    FIELD-SYMBOLS:
      <program>     TYPE ty_program,
      <test_class>  TYPE ty_test_class,
      <test_method> TYPE ty_test_method.

    mac_assign_test_method c_on_miss-ignore.

    IF ( <program> IS ASSIGNED ).
      <program>-state-has_issue = abap_true.
    ENDIF.
    IF ( <test_class> IS ASSIGNED ).
      <test_class>-state-issue-has_failure = abap_true.
    ENDIF.

    descr = i_ref_failure->get_header_description( ).
    alert-kind = i_kind.
    alert-context = me->f_test_context.
    alert-description = me->f_text_api->get_string( descr ).
    alert-description =
      escape( val = alert-description format = cl_abap_format=>e_html_text ).

    IF i_kind = c_kind-skipped.
      alert-level = c_level-skipped.
      IF ( <test_method> IS ASSIGNED ).
        <test_method>-state-has_been_skipped = abap_true.
        <test_class>-state-issue-has_been_skipped = abap_true.
      ELSEIF ( <test_class>  IS ASSIGNED ).
        <test_class>-state-issue-has_been_skipped = abap_true.
      ENDIF.

    ELSE.
      native_level = i_ref_failure->get_level( ).
      CASE native_level.
        WHEN if_aunit_constants=>fatal.
          ADD 1 TO me->f_statistic-cnt_failure-fatal.
          alert-level = c_level-fatal.
        WHEN if_aunit_constants=>critical.
          ADD 1 TO me->f_statistic-cnt_failure-critical.
          alert-level = c_level-critical.
        WHEN OTHERS.
          ADD 1 TO me->f_statistic-cnt_failure-tolerable.
          alert-level = c_level-tolerable.
      ENDCASE.
      IF ( <test_method> IS NOT ASSIGNED AND <test_class> IS ASSIGNED ).
        " a timeout or runtime abortion is reported on class level
        " however it happened within a method. Fix totals in such a case
        " by the assumption that at least one method has been executed
        CASE i_kind.
          WHEN c_kind-rt_failure.
            <test_class>-state-issue-has_rt_failure = abap_true.
            adjust_method_totals = abap_true.
          WHEN c_kind-timeout.
            <test_class>-state-issue-has_timeout = abap_true.
            adjust_method_totals = abap_true.
          WHEN OTHERS.
            adjust_method_totals = abap_false.
        ENDCASE.
        IF ( abap_true =  adjust_method_totals ).
          CASE native_level.
            WHEN if_aunit_constants=>fatal.
              ADD 1 TO me->f_statistic-cnt_method-with_fatal.
            WHEN if_aunit_constants=>critical.
              ADD 1 TO me->f_statistic-cnt_method-with_critical.
            WHEN OTHERS.
              ADD 1 TO me->f_statistic-cnt_method-with_tolerable.
          ENDCASE.
        ENDIF.
      ENDIF.
    ENDIF.

    count = lines( me->f_alerts ).
    count = count MOD 2.
    IF ( 1 = count ).
      alert-apply_zebra = abap_true.
    ENDIF.

    INSERT alert INTO TABLE me->f_alerts[].

    IF <test_method> IS ASSIGNED.
      IF <test_method>-alert-level < alert-level.
        <test_method>-alert = alert.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_aunit_listener~assert_failure.
    DATA:
      header TYPE if_aunit_text_description=>ty_s_description.
    header = failure->get_header_description( ).
    CASE header-id.
      WHEN c_alert_id-prerequisite.
        handle_failure( i_kind = c_kind-skipped i_ref_failure = failure ).

      WHEN c_alert_id-timeout.
        handle_failure( i_kind = c_kind-timeout i_ref_failure = failure ).

      WHEN OTHERS.
        handle_failure( i_kind = c_kind-assert_failure i_ref_failure = failure ).

    ENDCASE.
  ENDMETHOD.


  METHOD if_aunit_listener~warning.
    DATA:
      header TYPE if_aunit_text_description=>ty_s_description.
    header = warning->get_header_description( ).
    CASE header-id.
      WHEN c_alert_id-prerequisite.
        handle_failure( i_kind = c_kind-skipped i_ref_failure = warning ).

      WHEN c_alert_id-timeout.
        handle_failure( i_kind = c_kind-timeout i_ref_failure = warning ).

      WHEN OTHERS.
        handle_failure( i_kind = c_kind-warning i_ref_failure = warning ).

    ENDCASE.
  ENDMETHOD.


  METHOD if_aunit_listener~cx_failure.
    handle_failure( i_kind = c_kind-cx_failure i_ref_failure = failure ).
  ENDMETHOD.


  METHOD if_aunit_listener~rt_failure.
    handle_failure( i_kind = c_kind-rt_failure i_ref_failure = failure ).
  ENDMETHOD.


  METHOD if_aunit_listener~execution_event ##needed.
  ENDMETHOD.


  METHOD initialize_program_entry.
    DATA:
      escaped_name TYPE string,
      program      TYPE ty_program.
    FIELD-SYMBOLS:
      <program> TYPE ty_program.

    READ TABLE me->f_programs WITH KEY
      name = i_program_name
      ASSIGNING <program>.
    IF ( 0 NE sy-subrc ).
      program-name = i_program_name.
      cl_aunit_prog_info=>progname_to_tadir(
        EXPORTING
          progname = program-name
        IMPORTING
          obj_name = program-obj_name
          obj_type = program-obj_type ).
      INSERT program INTO TABLE me->f_programs ASSIGNING <program>.
    ENDIF.

    IF ( abap_true EQ me->f_layout-with_adt_uri AND
         <program>-obj_name IS NOT INITIAL ).
      escaped_name =
        escape( val = <program>-obj_name format = cl_abap_format=>e_uri_full ).

      CASE <program>-obj_type.
        WHEN 'CLAS'.
          me->f_test_context-adt_resource_uri =
           'adt://' && sy-sysid && '/sap/bc/adt/oo/classes/' && escaped_name ##no_Text.

        WHEN 'PROG'.
          me->f_test_context-adt_resource_uri =
           'adt://' && sy-sysid && '/sap/bc/adt/programs/programs/' && escaped_name ##no_Text.

        WHEN 'FUGR'.
          me->f_test_context-adt_resource_uri =
          'adt://' && sy-sysid && '/sap/bc/adt/functions/groups/' && escaped_name ##no_Text.

        WHEN OTHERS.
          CLEAR me->f_test_context-adt_resource_uri.

      ENDCASE.
      <program>-adt_resource_uri = me->f_test_context-adt_resource_uri.
    ENDIF.
  ENDMETHOD.


  METHOD finish_statistics.
    DATA:
      fraction_of_4       TYPE i,
      nbr_of_fill_entries TYPE i.
    FIELD-SYMBOLS:
      <test_class> TYPE ty_test_class,
      <program>    TYPE ty_program,
      <package>    TYPE ty_stat_package.

    me->f_statistic-cnt_failure-syntax_error = lines( me->f_syntax_errors ).
    add_devc_uri_syntax_errors( ).

    LOOP AT me->f_programs TRANSPORTING NO FIELDS WHERE is_permitted IS INITIAL.
      me->f_statistic-cnt_no_permission = me->f_statistic-cnt_no_permission + 1.
    ENDLOOP.

    LOOP AT me->f_programs ASSIGNING <program> WHERE state IS NOT INITIAL.
      ADD 1 TO me->f_statistic-cnt_programs.
      LOOP AT <program>-test_classes ASSIGNING <test_class> WHERE state IS NOT INITIAL.
        ADD 1 TO me->f_statistic-cnt_test_classes.
        LOOP AT <test_class>-test_methods TRANSPORTING NO FIELDS WHERE state IS NOT INITIAL.
          ADD 1 TO me->f_statistic-cnt_test_methods.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    me->f_statistic-cnt_packages = lines( me->f_statistic-packages ).

    me->f_statistic-cnt_failure-total =
      me->f_statistic-cnt_failure-critical +
      me->f_statistic-cnt_failure-fatal +
      me->f_statistic-cnt_failure-tolerable.

    IF ( abap_true EQ me->has_timeout( ) AND
         abap_false EQ me->has_failure( ) AND
         abap_false EQ me->has_warning( ) AND
         me->f_statistic-cnt_failure-syntax_error IS INITIAL ).
      me->f_statistic-has_timeout_only = abap_true.
    ENDIF.

    GET TIME.
    me->f_time_interval-finished_on = sy-datlo.
    me->f_time_interval-finished_at = sy-timlo.
    IF ( me->f_time_interval-finished_on < me->f_time_interval-started_on  OR
         me->f_time_interval-time_zone  <> sy-zonlo ).
      CLEAR me->f_time_interval-finished_on.
      CLEAR me->f_time_interval-finished_at.
    ELSEIF ( me->f_time_interval-finished_on = me->f_time_interval-started_on  ).
      CLEAR me->f_time_interval-finished_on.
    ENDIF.

    " prepare package data to ease rendering
    SORT me->f_statistic-packages BY name ASCENDING.
    IF ( me->f_statistic-packages IS NOT INITIAL ).
      LOOP AT me->f_statistic-packages ASSIGNING <package>.
        fraction_of_4 = sy-tabix  MOD 4.
        IF ( 1 EQ fraction_of_4 AND sy-tabix <> 1 ).
          <package>-new_line = abap_true.
        ENDIF.
      ENDLOOP.
      nbr_of_fill_entries = ( 4 - fraction_of_4 ) MOD 4.
      DO nbr_of_fill_entries TIMES.
        INSERT INITIAL LINE INTO TABLE me->f_statistic-packages.
      ENDDO.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


CLASS globalization_service IMPLEMENTATION.

  METHOD get_text.
    result-title-detailed_log =         'Detailed Log'(tlg).
    IF ( abap_true EQ i_contain_alerts_skipped_tests ).
      result-title-failures =           'Failures and Skipped Tests'(tfs).
    ELSE.
      result-title-failures =           'Failures'(tfl).
    ENDIF.
    result-title-method_statistics =    'Method Statistics'(tms).
    result-title-overview =             'Overview'(tov).
    result-title-packages =             'Packages'(lps).
    result-title-syntax_errors =        'Syntax Errors'(lse).
    result-title-no_permissions =       'Missing Permissions'(lpe).

    result-label-client =               'Client'(lmt).
    result-label-description =          'Description'(ldc).
    result-label-started_by =           'Started by'(lsb).
    result-label-started_on =           'Started on'(lso).
    result-label-started_at =           'Started at'(lsa).
    result-label-finished_on =          'Finished on'(lfo).
    result-label-finished_at =          'Finished at'(lfa).
    result-label-failures_critical =    'Critical Failures'(lcf).
    result-label-failures_fatal =       'Fatal Failures'(lff).
    result-label-failures_tolerable =   'Tolerable Failure'(ltf).
    result-label-level =                'Level'(llv).
    result-label-object_name =          'Object Name'(lon).
    result-label-object_type =          'Type'(lot).
    result-label-package =              'Package'(lpk).
    result-label-packages =             'Packages'(lps).
    result-label-passed_tests =         'Passed'(lpt).
    result-label-programs =             'Programs'(lpg).
    result-label-skipped =              'Skipped'(lws).
    result-label-no_permission =        'No Permission'(lnp).
    result-label-runtime_error =        'Runtime Error'(lre).
    result-label-syntax_errors =        'Syntax Errors'(lse).
    result-label-syntax_error_message = 'Message'(lsm).
    result-label-system =               'System'(lsy).
    result-label-test_class =           'Test Class'(lcl).
    result-label-test_classes =         'Test Classes'(lcs).
    result-label-test_method =          'Test Method'(lmd).
    result-label-test_methods =         'Test Methods'(lms).
    result-label-timeout =              'Timeout'(lto).
    result-label-variant =              'Variant'(lva).
    result-label-with_fatal =           'With Fatal'(lwf).
    result-label-with_critical =        'With Critical'(lwc).
    result-label-with_tolerable =       'With Tolerable'(kwt).

    result-detail-skipped_methods =     'method(s)'(dsm).
    result-detail-skipped_further_methods = 'further method(s)'(dsf).
    result-detail-overall_success =     'all tests passed succesfully'(dok).
  ENDMETHOD.
ENDCLASS.


CLASS program_events IMPLEMENTATION.
  METHOD at_selection_screen.

    DATA g_test_runner TYPE REF TO test_runner.
    DATA: r_prog_param TYPE REF TO cl_sat_prog_param.
    DATA: b_email  TYPE s_aucv_d_send_email.
    DATA b_direct TYPE s_aucv_d_show_results.
    DATA b_devc TYPE s_aucv_d_select_by_devc.
    DATA b_obj  TYPE s_aucv_d_select_by_prog.
    DATA p_excl  TYPE s_aucv_d_exclude_obj.
    DATA so_ndevc TYPE RANGE OF tdevc-devclass  .
    DATA so_ncl  TYPE RANGE OF seoaliases-clsname.
    DATA so_nfg  TYPE RANGE OF tlibg-area     .
    DATA so_npr TYPE RANGE OF s_aucv_d_prog_other .

*    IF sy-ucomm = 'SET_DEFAULT'.
*      g_test_runner->reset_params( ).
*    ENDIF.
*    IF sy-ucomm = 'ONLI' OR sy-ucomm = 'SJOB'.
*      IF b_devc = abap_true.
*        IF so_devc[] IS NOT INITIAL.
*          p_selcl = p_selfg = p_selprg = abap_true.
*        ELSE.
*          MESSAGE e002. "check devc not initial
*        ENDIF.
*      ENDIF.
*      IF b_obj = abap_true.
*        IF so_class[] IS INITIAL AND
*           so_fugr[]  IS INITIAL AND
*           so_prog[]  IS INITIAL.
*          MESSAGE e003. "check devc not initial
*        ENDIF.
*        CLEAR: p_selcl, p_selfg, p_selprg.
*        IF so_class[] IS NOT INITIAL.
*          p_selcl = abap_true.
*        ENDIF.
*        IF so_fugr[]  IS NOT INITIAL.
*          p_selfg = abap_true.
*        ENDIF.
*        IF so_prog[]  IS NOT INITIAL.
*          p_selprg = abap_true.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*    IF sy-ucomm = 'SJOB' AND b_direct = abap_true.
*      MESSAGE e004. "check devc not initial
*    ENDIF.
*    IF ( sy-ucomm = 'ONLI' OR sy-ucomm = 'SJOB' )
*         AND b_email = abap_true AND so_email[] IS INITIAL.
*      MESSAGE e005. "check devc not initial
*    ENDIF.

  ENDMETHOD.

  METHOD at_selection_screen_output.

    DATA tab TYPE string_table ##needed.

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = 'STAT1000'
      TABLES
        p_exclude = tab.

*    PERFORM sub_mode.

  ENDMETHOD.

  METHOD initialization.

*    CREATE OBJECT g_test_runner.
*
*    p_rsklvl = cl_aunit_permission_control=>get_max_risk_level( ).
*    p_durlvl = if_aunit_attribute_enums=>c_duration-long.
*
*    so_email-low =  cl_aucv_job_utilities=>get_user_email( i_uname  = sy-uname ).
*    so_email-sign = 'I'.
*    so_email-option = 'EQ'.
*    INSERT so_email INTO TABLE so_email[].
*
*    IF ( '731' <= sy-saprl ).
*      g_adt_uri_is_supported = abap_true.
*      p_adturi = abap_true.
*    ELSE.
*      g_adt_uri_is_supported = abap_false.
*      p_adturi = abap_false.
*    ENDIF.
*
**!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
** with each change of selection-screen: set this value
** constants c_selection_screen_changed_on type sydatum value '20100720'.
**!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*    CREATE OBJECT r_prog_param
*      EXPORTING
*        i_report                      = sy-repid
*        i_selection_screen_changed_on = c_selection_screen_changed_on. "20.07.2010 '20100720'
*    r_prog_param->delete_if_selscreen_changed( ).
*    r_prog_param->delete_expired_values( ).
*    g_test_runner->get_params( r_prog_param ).

  ENDMETHOD.

  METHOD start_of_selection.
*    g_test_runner->run( ).
  ENDMETHOD.
ENDCLASS.


CLASS tc_statistics DEFINITION FOR TESTING
    INHERITING FROM cl_aunit_assert
    RISK LEVEL HARMLESS DURATION SHORT.

  PUBLIC SECTION.
    INTERFACES:
      lif_aunit_test.

  PRIVATE SECTION.
    METHODS:
      setup,
      critical_failures FOR TESTING,
      tolerable_failures FOR TESTING,
      skipped FOR TESTING.

    DATA:
      f_listener TYPE REF TO email_listener,
      f_driver   TYPE REF TO test_runner,
      f_failure  TYPE REF TO if_aunit_info_failure.
ENDCLASS.


CLASS td_failure DEFINITION CREATE PRIVATE FOR TESTING.

  PUBLIC SECTION.
    INTERFACES:
      if_aunit_info_failure PARTIALLY IMPLEMENTED.
    CLASS-METHODS:
      create
        IMPORTING
          i_level           TYPE aunit_level OPTIONAL
          i_id              TYPE sychar04 OPTIONAL
            PREFERRED PARAMETER i_level
        RETURNING
          VALUE(r_instance) TYPE REF TO td_failure.


  PRIVATE SECTION.
    DATA:
       level TYPE aunit_level.
ENDCLASS.


CLASS td_failure IMPLEMENTATION.
  METHOD create.
    CREATE OBJECT r_instance.
    r_instance->level = i_level.
  ENDMETHOD.

  METHOD if_aunit_info_failure~get_level.
    result = level.
  ENDMETHOD.

  METHOD if_aunit_info_failure~get_stack_description ##needed.
  ENDMETHOD.

  METHOD if_aunit_info_message~get_analysis_description ##needed.
  ENDMETHOD.

  METHOD if_aunit_info_message~get_analysis_documents ##needed.
  ENDMETHOD.

  METHOD if_aunit_info_message~get_complete_description ##needed.
  ENDMETHOD.

  METHOD if_aunit_info_message~get_header_description ##needed.
  ENDMETHOD.

ENDCLASS.


CLASS tc_statistics IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT me->f_driver.
    me->f_listener ?= email_listener=>create_listener(
      i_programs = me->f_driver->f_programs i_au_factory = me->f_driver->f_au_factory ).

  ENDMETHOD.


  METHOD critical_failures.
    me->f_failure = td_failure=>create( if_aunit_constants=>critical ).

    me->f_listener->if_aunit_listener~assert_failure( me->f_failure ).
    me->f_listener->if_aunit_listener~assert_failure( me->f_failure ).

    assert_equals( act = me->f_listener->f_statistic-cnt_failure-critical
                   exp = 2 ).
  ENDMETHOD.


  METHOD tolerable_failures.
    me->f_failure = td_failure=>create( if_aunit_constants=>tolerable ).

    me->f_listener->if_aunit_listener~assert_failure( me->f_failure ).
    me->f_listener->if_aunit_listener~assert_failure( me->f_failure ).
    me->f_listener->if_aunit_listener~assert_failure( me->f_failure ).

    assert_equals(
      act = me->f_listener->f_statistic-cnt_failure-tolerable
      exp = 3 ).
  ENDMETHOD.


  METHOD skipped.
    me->f_failure = td_failure=>create( i_id = c_alert_id-prerequisite ).

    me->f_listener->if_aunit_listener~assert_failure( me->f_failure ).
    me->f_listener->if_aunit_listener~assert_failure( me->f_failure ).
    me->f_listener->if_aunit_listener~assert_failure( me->f_failure ).

    assert_equals(
      act = me->f_listener->f_statistic-cnt_failure-tolerable
      exp = 3 ).
  ENDMETHOD.

ENDCLASS.
