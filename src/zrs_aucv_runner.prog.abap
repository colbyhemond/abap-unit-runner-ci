report zrs_aucv_runner message-id s_aucv_runner.

tables:
  tdevc,
  seoaliases,
  tlibg,
  sscrfields.

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* with each change of selection-screen: set this value
constants c_selection_screen_changed_on type sydatum value '20100720'.
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

data:
  h_prog       type s_aucv_d_prog_other,
  h_mail_addr  type ad_smtpadr,
  r_prog_param type ref to cl_sat_prog_param.

selection-screen begin of block objs with frame title text-000.

selection-screen begin of block header.
parameters:
  b_devc type s_aucv_d_select_by_devc radiobutton group head default 'X' user-command uc_devc,
  b_obj  type s_aucv_d_select_by_prog radiobutton group head.
selection-screen end of block header.

* ----------------- via Package -----------------
selection-screen begin of block devc with frame title text-001.

selection-screen begin of line.
selection-screen comment 1(26)
  text-dvc for field so_devc modif id dvc.
select-options:
   so_devc    for tdevc-devclass no intervals modif id dvc. "default 'willy'
selection-screen end of line.

parameters:
  p_packr type s_aucv_d_with_subdevc as checkbox default 'X' modif id dvc
          user-command uc_packr,
  p_excl  type s_aucv_d_exclude_obj as checkbox default ' ' modif id dvc
          user-command uc_excl.

selection-screen begin of block excl with frame title text-007.
select-options:
  so_ndevc for tdevc-devclass     no intervals modif id xc2,
  so_ncl   for seoaliases-clsname no intervals modif id xcl,
  so_nfg   for tlibg-area         no intervals modif id xcl,
  so_npr   for h_prog             no intervals modif id xcl.
selection-screen end of block excl.

selection-screen end of block devc.

* ----------------- via Object Name -----------------
selection-screen begin of block obj with frame title text-002.

data:
  p_selcl                type abap_bool,
  p_selfg                type abap_bool,
  p_selprg               type abap_bool,
  g_adt_uri_is_supported type abap_bool.

select-options:
  so_class for seoaliases-clsname no intervals modif id obj,
  so_fugr for tlibg-area          no intervals modif id obj,
  so_prog for h_prog              no intervals modif id obj.

selection-screen end of block obj.

selection-screen end of block objs.


selection-screen begin of block rsk with frame title text-004.
parameters:
  p_rsklvl type saunit_d_allowed_risk_level
    as  listbox obligatory visible length 30,
  p_durlvl type saunit_d_allowed_rt_duration
    as listbox obligatory visible length 30.
selection-screen end of block rsk.

selection-screen begin of block options with frame title text-003.

parameters:
  b_email  type s_aucv_d_send_email radiobutton group opt default 'X' user-command uc_email,
  b_direct type s_aucv_d_show_results radiobutton group opt.

selection-screen begin of block send with frame title text-005.
select-options:
  so_email for h_mail_addr no intervals modif id eml. "visible length 60
parameters:
  p_error  type s_aucv_d_error_case_only as checkbox default 'X'
           modif id eml.
selection-screen end of block send.

selection-screen begin of block mail with frame title text-006.
parameters:
  p_detail type s_aucv_d_list_detail as listbox visible length 30 modif id eml,
  p_adturi type abap_bool as checkbox modif id sml.
selection-screen end of block mail.

selection-screen begin of block aucv with frame title text-008.
parameters:
  p_aucv type s_aucv_d_with_coverage as checkbox default 'X' modif id cov.
selection-screen end of block aucv.

selection-screen end of block options.


class test_runner definition deferred.

types:
  begin of ty_stat_package,
    name     type string,
    new_line type abap_bool,
    begin of state,
      executed_classes type i,
      executed_methods type i,
      skipped_methods  type i,
    end of state,
  end of ty_stat_package,

  ty_stat_packages type standard table of ty_stat_package with non-unique key name,
  ty_packages      type standard table of devclass with default key,

  begin of ty_context,
    package          type string,
    program          type string,
    obj_type         type string,
    obj_name         type string,
    test_class       type string,
    test_method      type string,
    adt_resource_uri type string,
  end of ty_context,

  ty_alert_level type c length 6,

  begin of ty_alert,
    context     type ty_context,
    kind        type string,
    description type string,
    level       type ty_alert_level,
    apply_zebra type abap_bool,
  end of ty_alert,

  ty_alerts type standard table of ty_alert with default key,

  begin of ty_test_method,
    name  type string,
    alert type ty_alert,
    begin of state,
      has_been_started type abap_bool,
      has_been_skipped type abap_bool,
    end of state,
  end of ty_test_method,
  ty_methods type standard table of ty_test_method with non-unique key name,

  begin of ty_icon,
    passed            type string,
    fatal_failure     type string,
    critical_failure  type string,
    tolerable_failure type string,
    skipped           type string,
    syntax_error      type string,
    no_permission     type string,
  end of ty_icon,

  begin of ty_test_class,
    name         type string,
    handle       type ref to if_aunit_test_class_handle,
    test_methods type ty_methods,
    begin of state,
      has_been_started           type abap_bool,
      begin of issue,
        has_been_skipped type abap_bool,
        has_rt_failure   type abap_bool,
        has_timeout      type abap_bool,
        has_failure      type abap_bool,
      end of issue,
      count_exec_methods         type i,
      count_skipped_methods      type i,
      count_skipped_over_methods type i,
      count_no_permission        type i,
    end of state,
  end of ty_test_class,

  ty_test_classes type standard table of ty_test_class with non-unique key name,

  begin of ty_program,
    name             type progname,
    obj_type         type tadir-object,
    obj_name         type tadir-obj_name,
    package          type tadir-devclass,
    adt_resource_uri type string,
    test_classes     type ty_test_classes,
    is_permitted     type abap_bool,
    begin of state,
      has_been_started type abap_bool,
      has_issue        type abap_bool,
    end of state,
  end of ty_program,

  ty_programs type standard table of ty_program with non-unique key obj_name obj_type
    with unique sorted key sorted components obj_name obj_type,

  begin of ty_syntax_error,
    obj_type         type tadir-object,
    obj_name         type tadir-obj_name,
    adt_resource_uri type string,
    message          type string,
    line             type i,
    token            type string,
  end of ty_syntax_error,

  ty_syntax_errors type standard table of ty_syntax_error with key obj_name obj_type,

  begin of ty_time_interval,
    started_on  type sy-datlo,
    started_at  type t,
    finished_on type sy-datlo,
    finished_at type sy-timlo,
    time_zone   type sy-zonlo,
  end of ty_time_interval,

  begin of ty_statistics,
    cnt_packages      type i,
    cnt_programs      type i,
    cnt_test_classes  type i,
    cnt_test_methods  type i,
    cnt_no_permission type i,

    begin of cnt_method,
      passed          type i,
      with_fatal      type i,
      with_critical   type i,
      with_tolerable  type i,
      skipped         type i,
    end of cnt_method,

    " message
    has_timeout_only type abap_bool,
    begin of cnt_failure,
      syntax_error   type i,
      fatal          type i,
      critical       type i,
      tolerable      type i,
      total          type i,
    end of cnt_failure,

    packages         type ty_stat_packages,

  end of ty_statistics.



***********************************************************************
data:
 g_test_runner type ref to test_runner.

constants:
  begin of c_detail,
    no    type c length 1 value '',
    basic type c length 1 value 'S',
    full  type c length 1 value 'X',
  end of c_detail.

constants:
  begin of c_level,
    skipped   type ty_alert_level value '1-SKIP' ##no_Text,
    tolerable type ty_alert_level value '2-TOLE' ##no_Text,
    critical  type ty_alert_level value '3-CRIT' ##no_Text,
    fatal     type ty_alert_level value '4-FATA' ##no_Text,
  end of c_level.

constants:
  begin of c_alert_id,
    prerequisite type c length 4 value 'FM04' ##no_Text,
    assumption  type c length 4 value 'FM05' ##no_Text,
    timeout type c length 4 value 'WM20' ##no_Text,
  end of c_alert_id.


interface lif_aunit_test.
endinterface.

***********************************************************************

interface mail_listener.

  interfaces: if_aunit_listener.

  types:
    begin of ty_layout,
      with_syntax_errors     type abap_bool,
      with_no_permissions    type abap_bool,
      with_execution_details type abap_bool,
      with_failure_details   type abap_bool,
      with_adt_uri           type abap_bool,
    end of ty_layout.

  methods:
    send_email
      raising cx_cmp_failure,

    has_failure
      returning value(result) type abap_bool,

    has_timeout
      returning value(result) type abap_bool,

    has_warning
      returning value(result) type abap_bool.


endinterface.


class globalization_service definition.
  public section.
    types:
      begin of ty_text,

        begin of title,
          detailed_log      type string,
          overview          type string,
          failures          type string,
          method_statistics type string,
          packages          type string,
          syntax_errors     type string,
          no_permissions    type string,
        end of title,

        begin of label,
          client               type string,
          description          type string,
          failures_critical    type string,
          failures_fatal       type string,
          failures_tolerable   type string,
          finished_on          type string,
          finished_at          type string,
          level                type string,
          object_name          type string,
          object_type          type string,
          package              type string,
          packages             type string,
          passed_tests         type string,
          programs             type string,
          no_permission        type string,
          runtime_error        type string,
          skipped              type string,
          started_by           type string,
          started_on           type string,
          started_at           type string,
          syntax_errors        type string,
          syntax_error_message type string,
          system               type string,
          test_class           type string,
          test_classes         type string,
          test_method          type string,
          test_methods         type string,
          timeout              type string,
          variant              type string,
          with_fatal           type string,
          with_critical        type string,
          with_tolerable       type string,
        end of label,
        begin of detail,
          overall_success         type string,
          skipped_methods         type string,
          skipped_further_methods type string,
        end of detail,
      end of ty_text.

    class-methods:
      get_text
        importing i_contain_alerts_skipped_tests type abap_bool
        returning value(result)                  type ty_text.

endclass.

class test_runner definition.
  public section.
    data:
      f_programs      type ty_programs read-only,
      f_syntax_errors type ty_syntax_errors read-only,
      f_au_factory    type ref to cl_aunit_factory read-only.
    methods:
      run,
      get_params importing i_ref_params type ref to cl_sat_prog_param,
      set_params importing i_ref_params type ref to cl_sat_prog_param,
      reset_params.
  protected section.
    data f_programs_without_tests type ty_programs.
    methods:
      run_direct,
      run_email,

      select_objects,
      select_objects_by_pkg,
      select_objects_by_type,
      select_objects_by_pkg_and_type
        importing i_packages type ty_packages,

      execute_unit_tests_4_mail
        exporting e_listener type ref to mail_listener,

      check_programs_without_tests,
      get_test_class_handles,
      settle_objects_after_selection.
endclass.


class email_listener definition create private friends lif_aunit_test.
  public section.
    interfaces mail_listener.

    class-methods:
      create_listener
        importing i_programs      type ty_programs
                  i_syntax_errors type ty_syntax_errors optional
                  i_au_factory    type ref to cl_aunit_factory
        returning value(result)   type ref to mail_listener.

  private section.
    aliases:
      ty_layout for mail_listener~ty_layout,
      send_email for mail_listener~send_email,
      has_failure for mail_listener~has_failure,
      has_timeout for mail_listener~has_timeout,
      has_warning for mail_listener~has_warning.


    methods:
      handle_failure
        importing i_kind        type string
                  i_ref_failure type ref to if_aunit_info_failure,

      initialize_program_entry
        importing i_program_name type csequence,

      finish_statistics,

      add_devc_uri_syntax_errors.


    constants:
      begin of c_on_miss,
        ignore type c length 6 value 'ignore' ##no_Text,
        assert type c length 6 value 'assert' ##no_Text,
        create type c length 6 value 'create' ##no_Text,
      end of c_on_miss.

    constants:
      begin of c_kind,
        assert_failure type string value 'Assertion Failure' ##no_Text,
        warning        type string value 'Warning' ##no_Text,
        cx_failure     type string value 'Exception' ##no_Text,
        rt_failure     type string value 'Runtime Abortion' ##no_Text,
        skipped        type string value 'Skipped' ##no_Text,
        timeout        type string value 'Timeout' ##no_Text,
      end of c_kind.

    data:
      f_programs      type ty_programs,
      f_syntax_errors type ty_syntax_errors,
      f_statistic     type ty_statistics,
      f_alerts        type ty_alerts,
      f_layout        type ty_layout,
      f_time_interval type ty_time_interval,

      f_test_context  type ty_context,
      f_au_factory    type ref to cl_aunit_factory,
      f_text_api      type ref to if_aunit_text_description.

    methods:
      init,

      compute_icon_info
        returning value(result) type ty_icon,

      compute_icon
        importing i_alt         type string
                  i_title       type string
                  i_color       type string
        returning value(result) type string.

endclass.



**********************************************************************

initialization.
  perform initialization.

start-of-selection.
  perform start_of_selection.

at selection-screen output.
  perform at_selection_screen_output.

at selection-screen.
  perform at_selection_screen.

**********************************************************************

class test_runner implementation.
  method run.
    if sy-batch is initial.
      g_test_runner->set_params( r_prog_param ).
    endif.

    call function 'DB_COMMIT'.
*
    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        text = 'Select Objects ....'(p01).
    select_objects( ).

    if me->f_programs is initial.
      message s000. "No objects were found
      return.
    endif.

    get_test_class_handles( ).

    if ( abap_true eq b_email ).
      check_programs_without_tests( ).
    endif.

    if me->f_programs is initial.
      if b_email is not initial and me->f_syntax_errors is not initial.
      else.
        message s001. "No unit test class was found
        return.
      endif.
    endif.

    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        text = 'Executing tests'(pex).

    case abap_true.
      when b_direct.
        run_direct( ).
      when b_email.
        run_email( ).
    endcase.
  endmethod.


  method get_params.
    define l_mac_get.
      i_ref_params->get_param_value( exporting i_name  = &1
                                     changing  p_value = &2 ).
    end-of-definition.

    l_mac_get:
      'RS_AUCV_R_DEVC' b_devc, "select by package
      'RS_AUCV_R_OBJ' b_obj,
      'RS_AUCV_R_EMAIL' b_email, "send mails
      'RS_AUCV_R_DIRECT' b_direct, "results in dialog

* select by package
      'RS_AUCV_SO_DEVC' so_devc[],
      'RS_AUCV_C_PACKR' p_packr,
      'RS_AUCV_C_EXCL' p_excl, "exclude selected objects
      'RS_AUCV_SO_NDEVC' so_ndevc[],
      'RS_AUCV_SO_NCL' so_ncl[],
      'RS_AUCV_SO_NFG' so_nfg[],
      'RS_AUCV_SO_NPR' so_npr[],
* select by prog
      'RS_AUCV_SO_CLASS' so_class[],
      'RS_AUCV_SO_FUGR' so_fugr[],
      'RS_AUCV_SO_PROG' so_prog[].

* flag as default //see also the definition of b_devc + b_email: default 'X'
    p_error = abap_true.
    p_adturi = abap_true.
    p_aucv = abap_true.
    l_mac_get:
     'RS_AUCV_C_ERROR' p_error, "send in error case
     'RS_AUCV_C_ADTURI' p_adturi,
     'RS_AUCV_C_AUCV' p_aucv.
* initialized
    l_mac_get:
      'RS_AUCV_D_RSKLVL' p_rsklvl, "risk level
      'RS_AUCV_D_DURLVL' p_durlvl, "duration
      'RS_AUCV_D_DETAIL' p_detail.

    i_ref_params->get_param_value( exporting i_name  = 'RS_AUCV_SO_EMAIL'
                                  changing  p_value = so_email[] ).
  endmethod.


  method set_params.
    define l_mac_set.
      i_ref_params->set_param_value(
       exPORTING
         i_name = &1
         i_value  = &2 ).
    end-of-definition.

    l_mac_set:
      'RS_AUCV_R_DEVC' b_devc, "select by package
      'RS_AUCV_R_OBJ' b_obj,
      'RS_AUCV_R_EMAIL' b_email, "send mails
      'RS_AUCV_R_DIRECT' b_direct, "results in dialog

* select by package
      'RS_AUCV_SO_DEVC' so_devc[],
      'RS_AUCV_C_PACKR' p_packr,
      'RS_AUCV_C_EXCL' p_excl, "exclude selected objects
      'RS_AUCV_SO_NDEVC' so_ndevc[],
      'RS_AUCV_SO_NCL' so_ncl[],

      'RS_AUCV_SO_NFG' so_nfg[],
      'RS_AUCV_SO_NPR' so_npr[],
* select by prog
      'RS_AUCV_SO_CLASS' so_class[],
      'RS_AUCV_SO_FUGR' so_fugr[],
      'RS_AUCV_SO_PROG' so_prog[],

      'RS_AUCV_SO_EMAIL' so_email[],

      'RS_AUCV_C_ERROR' p_error, "send in error case
      'RS_AUCV_C_ADTURI' p_adturi,
      'RS_AUCV_C_AUCV' p_aucv,

      'RS_AUCV_D_RSKLVL' p_rsklvl, "risk level
      'RS_AUCV_D_DURLVL' p_durlvl, "duration
      'RS_AUCV_D_DETAIL' p_detail.
  endmethod.


  method reset_params.
    b_devc = abap_true. b_obj = abap_false. "radio group
    b_email = abap_true. b_direct = abap_false. "radio group

    clear:
      p_packr, p_excl, p_error, p_error, p_error, p_detail,
      so_devc, so_devc[], so_ndevc, so_ndevc[], so_ncl, so_ncl[], so_nfg, so_nfg[], so_npr, so_npr[],
      so_class, so_class[], so_fugr, so_fugr[], so_prog, so_prog[],
      so_email, so_email[].

    p_rsklvl = cl_aunit_permission_control=>get_max_risk_level( ).
    p_durlvl = if_aunit_attribute_enums=>c_duration-long.

    so_email-low =  cl_aucv_job_utilities=>get_user_email( i_uname  = sy-uname ).
    so_email-sign = 'I'.
    so_email-option = 'EQ'.
    insert so_email into table so_email[].

    r_prog_param->delete_param_values( '*' ). "sy-mandt/repid/uname/ '*' all params
  endmethod.


  method run_direct.
    data:
      tadir_keys type sabp_t_tadir_keys,
      tadir_key  type sabp_s_tadir_key.
    field-symbols:
      <program>    type ty_program.

    loop at me->f_programs assigning <program>.
      tadir_key-obj_type = <program>-obj_type.
      tadir_key-obj_name = <program>-obj_name.
      insert tadir_key into table tadir_keys.
    endloop.
    call function 'SABP_AU_TEST_ITEMS_FROM_IDE'
      exporting
        tadir_keys                 = tadir_keys
        with_coverage              = p_aucv
        limit_on_risk_level        = p_rsklvl
        limit_on_duration_category = p_durlvl.

  endmethod.


  method run_email.
    data:
      mail_listener type ref to mail_listener,
      failure       type ref to cx_root.
    try.
        execute_unit_tests_4_mail( importing e_listener = mail_listener ).
        if ( abap_true eq mail_listener->has_timeout( ) ).
          " timeout due to byte code generation? retry give it one more chance
          execute_unit_tests_4_mail( importing e_listener = mail_listener ).
        endif.
        mail_listener->send_email( ).
      catch cx_root into failure ##catch_All.
        cl_sat_ui_std_dialogue=>display_exception( failure ).
    endtry.
  endmethod.


  method execute_unit_tests_4_mail.

    data:
      duration type if_aunit_task=>ty_s_duration_setting,
      au_task  type ref to if_aunit_task.
    field-symbols:
      <program>    type ty_program,
      <test_class> type ty_test_class.

    e_listener = email_listener=>create_listener(
      i_programs = me->f_programs
      i_syntax_errors = me->f_syntax_errors
      i_au_factory = me->f_au_factory ).
    au_task =
       me->f_au_factory->create_task( listener = e_listener ).

    au_task->restrict_risk_level( p_rsklvl ).
    au_task->restrict_duration_category( p_durlvl ).

    loop at me->f_programs assigning <program> where is_permitted is not initial.
      loop at <program>-test_classes assigning <test_class>.
        au_task->add_test_class_handle( <test_class>-handle ).
      endloop.
    endloop.

    au_task->run( mode = if_aunit_task=>c_run_mode-external ).

  endmethod.


  method get_test_class_handles.
    data:
      syntax_error       type ty_syntax_error,
      method_names       type saunit_t_methods,
      method_name        type saunit_d_method,
      method             type ty_test_method,
      test_class         type ty_test_class,
      test_class_handles
        type if_aunit_test_class_handle=>ty_t_testclass_handles.
    field-symbols:
      <program>         type ty_program.

    create object me->f_au_factory.

    loop at me->f_programs assigning <program>.
      test_class_handles =
         me->f_au_factory->get_test_class_handles(
           obj_type = <program>-obj_type
           obj_name = <program>-obj_name  ).

      clear test_class.
      loop at test_class_handles into test_class-handle.

        test_class-name = test_class-handle->get_class_name( ).
        method_names = test_class-handle->get_test_methods( ).

        loop at method_names into method-name.
          insert method into table test_class-test_methods.
        endloop.

        insert test_class into table <program>-test_classes.
        clear test_class.
      endloop.

      <program>-is_permitted =
        me->f_au_factory->is_test_execution_permitted(
          object_key = value #( obj_name = <program>-obj_name obj_type = <program>-obj_type )
          package_name = <program>-package ).
    endloop.

    me->f_programs_without_tests = me->f_programs.
    delete me->f_programs_without_tests where test_classes is not initial.
    delete me->f_programs where test_classes is initial.

  endmethod.


  method check_programs_without_tests.

    data:
      syntax_error type ty_syntax_error,
      dummy        type c length 1.
    field-symbols:
      <program> type ty_program.

    loop at me->f_programs_without_tests assigning <program>.
      clear syntax_error.
      select single r3state from reposrc into dummy
        where progname = <program>-name and
              r3state = 'A'.
      if ( 0 ne sy-subrc ).
        continue.
      endif.
      syntax-check for program <program>-name
        message syntax_error-message
        line syntax_error-line
        word syntax_error-token.
      if sy-subrc = 0.
        continue.
      endif.
      syntax_error-obj_name = <program>-obj_name.
      syntax_error-obj_type = <program>-obj_type.
      insert syntax_error into table me->f_syntax_errors.
    endloop.

  endmethod.


  method select_objects_by_pkg.

    data:
      package           type devclass,
      selected_packages type ty_packages,
      expanded_packages type cl_pak_package_queries=>tt_subpackage_info,
      overall_packages  type ty_packages.

    select devclass from tdevc
      into table selected_packages
      where
        devclass in so_devc.                            "#EC CI_GENBUFF

    if abap_false eq  p_packr. " no sub packages.
      overall_packages = selected_packages.
    else.
      overall_packages = selected_packages.
      loop at selected_packages into package.
        clear expanded_packages.
        cl_pak_package_queries=>get_all_subpackages(
          exporting
            im_package                    = package
          importing
            et_subpackages                = expanded_packages
          exceptions
            others                        = 7 ).
        if ( abap_true eq p_excl and so_ndevc is not initial ).
          delete expanded_packages
            where table_line in so_ndevc.               "#EC CI_SORTSEQ
        endif.
        append lines of expanded_packages to overall_packages.
      endloop.
    endif.

    sort overall_packages.
    delete adjacent duplicates from overall_packages.
    if ( overall_packages is not initial ).
      select_objects_by_pkg_and_type( overall_packages ).
    endif.

  endmethod.


  method select_objects_by_pkg_and_type.

    data:
      tab_rng_obj_name type range of tadir-obj_name,
      package          type devclass.

    assert i_packages is not initial.

    if abap_true eq p_excl and so_ncl is not initial.
      select                                              ##TOO_MANY_ITAB_FIELDS
        object as obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass as package
        from tadir
        appending corresponding fields of table me->f_programs
        for all entries in i_packages
        where
          devclass = i_packages-table_line and
          pgmid    = 'R3TR' and
          object   = 'CLAS' and
          obj_name not in so_ncl[].
    else.
      select                                              ##TOO_MANY_ITAB_FIELDS
        object as obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass as package
        from tadir
        appending corresponding fields of table me->f_programs
        for all entries in i_packages
        where
          devclass = i_packages-table_line and
          pgmid    = 'R3TR' and
          object   = 'CLAS'.
    endif.

    if abap_true eq p_excl and so_nfg is not initial.
      select                                              ##TOO_MANY_ITAB_FIELDS
        object as obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass as package
        from tadir
        appending corresponding fields of table me->f_programs
        for all entries in i_packages
        where
          devclass = i_packages-table_line and
          pgmid    = 'R3TR' and
          object   = 'FUGR' and
          obj_name not in so_nfg[].
    else.
      select                                              ##TOO_MANY_ITAB_FIELDS
        object as obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass as package
        from tadir
        appending corresponding fields of table me->f_programs
        for all entries in i_packages
        where
          devclass = i_packages-table_line and
          pgmid    = 'R3TR' and
          object   = 'FUGR'.
    endif.

    if abap_true eq p_excl and so_npr is not initial.
      select                                              ##TOO_MANY_ITAB_FIELDS
        object as obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass as package
        from tadir
        appending corresponding fields of table me->f_programs
        for all entries in i_packages
        where
          devclass = i_packages-table_line and
          pgmid    = 'R3TR' and
          object   = 'PROG' and
          obj_name not in so_npr[].
    else.
      select                                              ##TOO_MANY_ITAB_FIELDS
        object as obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass as package
        from tadir
        appending corresponding fields of table me->f_programs
        for all entries in i_packages
        where
          devclass = i_packages-table_line and
          pgmid    = 'R3TR' and
          object   = 'PROG'.
    endif.

    delete me->f_programs where obj_type = 'CLAS' and obj_name cs '='.
  endmethod.


  method select_objects_by_type.

    if not p_selcl is initial.
      select                                              ##TOO_MANY_ITAB_FIELDS
        object as obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass as package
        from tadir
        appending corresponding fields of table me->f_programs
        where
          pgmid = 'R3TR' and
          object = 'CLAS' and
          obj_name in so_class.
    endif.

    if not p_selprg is initial.
      select                                              ##TOO_MANY_ITAB_FIELDS
        object as obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass as package
        from tadir
        appending corresponding fields of table me->f_programs
        where
          pgmid = 'R3TR' and
          object = 'PROG' and
          obj_name in so_prog.
    endif.

    if not p_selfg is initial.
      select                                              ##TOO_MANY_ITAB_FIELDS
        object as obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass as package
        from tadir
        appending corresponding fields of table me->f_programs
        where
          pgmid = 'R3TR' and
          object = 'FUGR' and
          obj_name  in so_fugr.
    endif.

  endmethod.


  method settle_objects_after_selection.

    if ( p_selcl is initial ).
      delete me->f_programs
        where
          obj_type = 'CLAS'.
    else.
      delete me->f_programs
        where
          obj_type = 'CLAS' and
          obj_name not in so_class.
    endif.

    if ( p_selprg is initial ).
      delete me->f_programs
        where
          obj_type = 'PROG'.
    else.
      delete me->f_programs
        where
          obj_type = 'PROG' and
          obj_name not in so_prog.
    endif.

    if ( p_selfg is initial ).
      delete me->f_programs
        where
          obj_type = 'FUGR'.
    else.
      delete me->f_programs
        where
          obj_type = 'FUGR' and
          obj_name not in so_fugr.
    endif.

  endmethod.


  method select_objects.

    data:
      type_of_include type trdir-subc.
    field-symbols:
      <program>  type ty_program.
    constants:
      begin of c_include_type,
        stand_alone type trdir-subc  value 'I',
      end of c_include_type.

    if b_devc = abap_true.
      select_objects_by_pkg( ).
    elseif b_obj = abap_true.
      select_objects_by_type( ).
    endif.

    " excluded
    if b_devc = abap_true and p_excl = abap_true.
      if not so_ndevc[] is initial.
        delete me->f_programs
           where package in so_ndevc.
      endif.
      if not so_ncl[] is initial.
        delete me->f_programs
           where obj_type = 'CLAS' and obj_name in so_ncl.
      endif.
      if not so_nfg[] is initial.
        delete me->f_programs
           where obj_type = 'FUGR' and obj_name in so_nfg.
      endif.
      if not so_npr[] is initial.
        delete me->f_programs
           where obj_type = 'PROG' and obj_name in so_npr.
      endif.
    endif.

    loop at me->f_programs assigning <program>.
      if ( <program>-obj_type = 'PROG' ).
        " no includes
        select single subc from trdir
          into type_of_include
          where
            name = <program>-obj_name and
            subc <> c_include_type-stand_alone.
        if ( 0 ne sy-subrc ).
          continue.
        endif.
      endif.
      <program>-name  = cl_aunit_prog_info=>tadir_to_progname(
         obj_type = <program>-obj_type
         obj_name = <program>-obj_name ).
    endloop.
    delete me->f_programs where name is initial.
    sort me->f_programs by package obj_type obj_name.
  endmethod.

endclass.


class email_listener implementation.

  define mac_assign_program.
    read table me->f_Programs
      assigning <program> with key sorted components
        obj_Type = me->f_Test_Context-obj_Type
        obj_Name = me->f_Test_Context-obj_Name.
    if ( 0 ne sy-subrc ).
      read table me->f_Programs
        assigning <program> with key
          name = me->f_Test_Context-program.
    endif.
    if ( 0 ne sy-subrc ).
      case &1.
        when c_On_Miss-create.
          insert initial line into table me->f_Programs assigning <program>.
          <program>-name =     me->f_Test_Context-program.
          <program>-obj_Type = me->f_Test_Context-obj_Type.
          <program>-obj_Name = me->f_Test_Context-obj_Name.
        when c_On_Miss-ignore.
          unassign <program>.
        when others.
          assert 1 = 2.
      endcase.
    endif.
  end-of-definition.

  define mac_assign_test_class.
    mac_Assign_Program &1.
    if ( <Program> is assigned ).
      read table <program>-test_Classes assigning <test_Class>
        with key name = me->f_Test_Context-test_Class.
      if ( 0 ne sy-subrc ).
        case &1.
          when c_On_Miss-create.
            insert initial line into table <program>-test_Classes assigning <test_Class>.
            <test_Class>-name = me->f_Test_Context-test_Class.
          when c_On_Miss-ignore.
            unassign <Test_Class>.
          when others.
            assert 1 = 2.
        endcase.
      endif.
    else.
      unassign <Test_Class>.
    endif.
  end-of-definition.

  define mac_assign_test_method.
    mac_Assign_Test_Class &1.
    if ( <Test_Class> is assigned ).
      read table <test_Class>-test_Methods assigning <test_Method>
        with key name = me->f_Test_Context-test_Method.
      if ( 0 ne sy-subrc ).
        case &1.
          when c_On_Miss-create.
            insert initial line into table <test_Class>-test_Methods assigning <test_Method>.
            <test_Method>-name = me->f_Test_Context-test_Method.
          when c_On_Miss-ignore.
            unassign <test_Method>.
          when others.
            assert 1 = 2.
        endcase.
      endif.
    else.
      unassign <test_Method>.
    endif.
  end-of-definition.


  method create_listener.

    data: mail_listener type ref to email_listener.

    create object mail_listener.
    mail_listener->f_programs = i_programs.
    mail_listener->f_syntax_errors = i_syntax_errors.
    mail_listener->f_statistic-cnt_failure-syntax_error = lines( i_syntax_errors ).

    if ( i_au_factory is bound ).
      mail_listener->f_au_factory =  i_au_factory.
    else.
      create object mail_listener->f_au_factory.
    endif.
    mail_listener->init( ).
    result = mail_listener.

  endmethod.


  method send_email.
    types:
      begin of ty_no_permission,
        object type string,
        new_line type abap_bool,
      end of ty_no_permission,
      ty_no_permissions type standard table of ty_no_permission with default key.

    data: contain_alerts_skipped_tests type abap_bool.
    constants:
      c_area     type c length 4 value 'MAIN',
      c_template type syrepid value 'RS_AUCV_RUNNER_MAIL_TEMPLATE'.
    data:
      name          type string,
      xpt_caught    type ref to cx_root,
      html_composer type ref to cl_cmp_composer,
      text_lines    type cl_cmp_composer=>tab_code,
      string        type string,
      title         type string,
      icon_info     type ty_icon,
      email         like line of so_email[],
      no_permission type ty_no_permission,
      no_permissions type ty_no_permissions,
      programs      type ty_programs,
      alert         type ty_alert.
    field-symbols:
      <text>    like line of text_lines,
      <program> type ty_program.

    " extract programs without sufficient permissions
    loop at me->f_programs assigning <program> where is_permitted is initial.
      clear no_permission.
      no_permission-object = |{ <program>-obj_type }&nbsp{ <program>-obj_name }|.
      if ( 0 eq ( lines( no_permissions ) mod 3 ) ).
        " every 3 programs a new line / aid rendering
        no_permission-new_line = abap_true.
      endif.
      insert no_permission into table no_permissions.
    endloop.
    if ( no_permissions is not initial ).
      me->f_layout-with_no_permissions = abap_true.
      while ( 0 <> ( lines( no_permissions ) mod 3 ) ).
        insert initial line into table no_permissions.
      endwhile.
    endif.

    " reduce programs to permitted ones
    programs = me->f_programs.
    delete programs where is_permitted is initial.

    if p_error is not initial and
       me->f_statistic-cnt_failure-fatal    is initial and
       me->f_statistic-cnt_failure-critical is initial and
       me->f_statistic-cnt_failure-tolerable is initial and
       me->f_statistic-cnt_failure-syntax_error is initial and
       no_permissions is initial.
      message 'All unit tests passed successfully -> No Email'(k00) type 'S'.
      exit.
    endif.

    if ( me->f_alerts is not initial ).
      me->f_layout-with_failure_details = abap_true.
    endif.
    if me->f_syntax_errors is not initial.
      me->f_layout-with_syntax_errors = abap_true.
    endif.

    case p_detail.
      when c_detail-full.
        if ( programs is not initial ).
          me->f_layout-with_execution_details = abap_true.
        endif.

      when c_detail-basic.
        loop at programs assigning <program> where state-has_issue eq abap_true.
          delete <program>-test_classes where state-issue is initial.
        endloop.
        delete programs where state-has_issue eq abap_false or test_classes is initial.
        if ( programs is not initial ).
          me->f_layout-with_execution_details = abap_true.
        endif.

      when others.
        clear programs.
        me->f_layout-with_execution_details = abap_false.
        delete me->f_alerts where kind = c_kind-skipped.
    endcase.

    read table me->f_alerts with key kind = c_kind-skipped transporting no fields.
    if ( 0 eq sy-subrc ).
      contain_alerts_skipped_tests = abap_true.
    else.
      contain_alerts_skipped_tests = abap_false.
    endif.

    html_composer = cl_cmp_composer=>s_create( ).
    icon_info = compute_icon_info( ).

    data(globalized_text) = globalization_service=>get_text( contain_alerts_skipped_tests ).

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
    try.
        text_lines =
          html_composer->build_code(
            i_area             = c_area  " 'MAIN'
            i_template_include = c_template ).

      cleanup into xpt_caught.
        message xpt_caught type 'I' display like 'E'.
    endtry.

    read table text_lines into title index 1.
    delete text_lines index 1.
    loop at text_lines assigning <text>.
      condense <text>.
    endloop.

    cl_aucv_job_utilities=>send_emails(
      i_title         = title
      i_tab_rng_email = so_email[]
      i_tab_lines     = text_lines
      i_flg_html      = abap_true ).

    commit work.
  endmethod.


  method has_failure.
    read table me->f_alerts
      transporting no fields
      with key kind = c_kind-assert_failure.
    if ( 0 eq sy-subrc ).
      result = abap_true.
      return.
    endif.

    read table me->f_alerts
      transporting no fields
      with key kind = c_kind-cx_failure.
    if ( 0 eq sy-subrc ).
      result = abap_true.
      return.
    endif.

    read table me->f_alerts
      transporting no fields
      with key kind = c_kind-rt_failure.
    if ( 0 eq sy-subrc ).
      result = abap_true.
      return.
    endif.
  endmethod.


  method has_timeout.
    read table me->f_alerts
      transporting no fields
      with key kind = c_kind-timeout.
    if ( 0 eq sy-subrc ).
      result = abap_true.
    else.
      result = abap_false.
    endif.
  endmethod.


  method has_warning.
    read table me->f_alerts
      transporting no fields
      with key kind = c_kind-warning.
    if ( 0 eq sy-subrc ).
      result = abap_true.
    else.
      result = abap_false.
    endif.
  endmethod.


  method add_devc_uri_syntax_errors.
    data:
      devclass     type tadir-devclass,
      package      type ty_stat_package,
      escaped_name type string.
    field-symbols:
      <syntax_error> type ty_syntax_error.

    check me->f_syntax_errors is not initial.
    loop at me->f_syntax_errors assigning <syntax_error>.
      select single devclass
        from  tadir into devclass
         where
           pgmid     = 'R3TR'     and
           object    = <syntax_error>-obj_type   and
           obj_name  = <syntax_error>-obj_name. "#EC CI_GENBUFF
      read table me->f_statistic-packages
        with key name = devclass transporting no fields.
      if ( 0 ne sy-subrc ).
        package-name = devclass.
        insert package into table me->f_statistic-packages.
        add 1 to me->f_statistic-cnt_packages.
      endif.
      if abap_true eq me->f_layout-with_adt_uri.
        escaped_name =
          escape( val = <syntax_error>-obj_name format = cl_abap_format=>e_uri_full ).
        case <syntax_error>-obj_type.
          when 'CLAS'.
            <syntax_error>-adt_resource_uri =
             'adt://' && sy-sysid && '/sap/bc/adt/oo/classes/' && escaped_name ##no_Text.
          when 'PROG'.
            <syntax_error>-adt_resource_uri =
             'adt://' && sy-sysid && '/sap/bc/adt/programs/programs/' && escaped_name ##no_Text.
          when 'FUGR'.
            <syntax_error>-adt_resource_uri =
            'adt://' && sy-sysid && '/sap/bc/adt/functions/groups/' && escaped_name ##no_Text.
          when others.
            clear <syntax_error>-adt_resource_uri.
        endcase.
      endif.
    endloop.
  endmethod.


  method compute_icon_info.
    data: base_url type string.
    constants:
      begin of c_alt,
        passed            type string value `&#x2714;` ##no_Text,
        skipped           type string value `&#x27a5;` ##no_Text,
        tolerable_failure type string value `&#x25ce;` ##no_Text,
        critical_failure  type string value `&#x25b2;` ##no_Text,
        fatal_failure     type string value `&#x2726;` ##no_Text,
        syntax_error      type string value `&#x2716;` ##no_Text, "9889
        no_permission     type string value `&#x26db;` ##no_Text,
      end of c_alt,
      begin of c_color,
        passed            type string value `green` ##no_Text,
        skipped           type string value `grey` ##no_Text,
        tolerable_failure type string value `orange` ##no_Text,
        critical_failure  type string value `red` ##no_Text,
        fatal_failure     type string value `red` ##no_Text,
        syntax_error      type string value `red` ##no_Text,
        no_permission     type string value `orange` ##no_Text,
      end of c_color,
      begin of c_title,
        passed            type string value `Test Passed` ##no_Text,
        skipped           type string value `Test Skipped` ##no_Text,
        tolerable_failure type string value `Tolerable Failure` ##no_Text,
        critical_failure  type string value `Critical Failure` ##no_Text,
        fatal_failure     type string value `Fatal Failure` ##no_Text,
        syntax_error      type string value `Syntax Error` ##no_Text,
        no_permission     type string value `No Permission` ##no_Text,
      end of c_title.

    define mac_compute_icon.
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
  endmethod.


  method compute_icon.
    result = `<b style="color:$">$</b>` ##no_Text.
    replace first occurrence of '$' in result with: i_color, i_alt.
  endmethod.


  method init.
    field-symbols:
      <test_class>  type ty_test_class,
      <test_method> type ty_test_method,
      <program>     type ty_program.

    me->f_text_api = me->f_au_factory->get_text_converter( language = 'E' ).
    if ( abap_true eq g_adt_uri_is_supported and abap_true eq p_adturi ).
      me->f_layout-with_adt_uri = abap_true.
    else.
      me->f_layout-with_adt_uri = abap_false.
    endif.

    loop at me->f_programs assigning <program>.
      clear <program>-state.
      loop at <program>-test_classes assigning <test_class>.
        clear <test_class>-state.
        loop at <test_class>-test_methods assigning <test_method>.
          clear <test_method>-state.
        endloop.
      endloop.
    endloop.

    clear me->f_time_interval.
    get time.
    me->f_time_interval-started_on = sy-datlo.
    me->f_time_interval-started_at = sy-timlo.
    me->f_time_interval-time_zone =  sy-zonlo.
  endmethod.


  method if_aunit_listener~task_start.
    clear:
      me->f_test_context,
      me->f_statistic,
      me->f_alerts.
  endmethod.


  method if_aunit_listener~program_start.
    data:
      descr        type if_aunit_text_description=>ty_s_description,
      package      type ty_stat_package,
      message_text type string.
    field-symbols:
      <program>      type ty_program.

    clear me->f_test_context-test_method.

    descr = info->get_description( ).
    read table descr-params index 1 into
       me->f_test_context-program.

    initialize_program_entry(  me->f_test_context-program ).
    mac_assign_program c_on_miss-assert.
    <program>-state-has_been_started = abap_true.
    me->f_test_context-obj_name = <program>-obj_name.
    me->f_test_context-obj_type = <program>-obj_type.
    me->f_test_context-package =  <program>-package.


    read table me->f_statistic-packages
      with key name = <program>-package transporting no fields.
    if ( 0 ne sy-subrc ).
      package-name = <program>-package.
      insert package into table me->f_statistic-packages.
    endif.
  endmethod.


  method if_aunit_listener~class_start.
    data:
      descr         type if_aunit_text_description=>ty_s_description.
    field-symbols:
      <test_class> type ty_test_class,
      <program>    type ty_program.

    clear me->f_test_context-test_class.
    clear me->f_test_context-test_method.

    descr = info->get_description( ).
    read table descr-params
      index 1 into me->f_test_context-test_class.
    mac_assign_test_class c_on_miss-create.
    <test_class>-state-has_been_started = abap_true.
  endmethod.


  method if_aunit_listener~method_start.
    data:
      descr         type if_aunit_text_description=>ty_s_description.
    field-symbols:
      <test_method> type ty_test_method,
      <test_class>  type ty_test_class,
      <program>     type ty_program.

    clear me->f_test_context-test_method.

    descr = info->get_description( ).
    read table descr-params index 1 into
       me->f_test_context-test_method.

    mac_assign_test_method c_on_miss-create.
    <test_method>-state-has_been_started = abap_true.
    add 1 to <test_class>-state-count_exec_methods.
  endmethod.


  method if_aunit_listener~method_end.
    field-symbols:
      <program>     type ty_program,
      <test_class>  type ty_test_class,
      <test_method> type ty_test_method.

    mac_assign_test_method c_on_miss-assert.
    if ( <test_method>-alert is initial ).
      add 1 to me->f_statistic-cnt_method-passed.
    else.
      case <test_method>-alert-level.
        when c_level-skipped.
          add 1 to me->f_statistic-cnt_method-skipped.
          add 1 to <test_class>-state-count_skipped_methods.
        when c_level-fatal.
          add 1 to me->f_statistic-cnt_method-with_fatal.
        when c_level-critical.
          add 1 to me->f_statistic-cnt_method-with_critical.
        when c_level-tolerable.
          add 1 to me->f_statistic-cnt_method-with_tolerable.
        when others.
          clear sy-subrc.
      endcase.
    endif.

    clear:
      me->f_test_context-test_method.
  endmethod.


  method if_aunit_listener~class_end.
    field-symbols:
      <program>     type ty_program,
      <test_class>  type ty_test_class,
      <test_method> type ty_test_method.

    mac_assign_test_class c_on_miss-assert.

    case abap_true.
      when <test_class>-state-issue-has_been_skipped.
        loop at <test_class>-test_methods assigning <test_method>
          where
            state-has_been_started = abap_false.
          <test_method>-state-has_been_skipped = abap_true.
          add 1 to me->f_statistic-cnt_method-skipped.
          add 1 to <test_class>-state-count_skipped_methods.
          add 1 to <test_class>-state-count_skipped_over_methods.
        endloop.
      when <test_class>-state-issue-has_rt_failure or
           <test_class>-state-issue-has_timeout.
        if ( <test_class>-state-count_exec_methods is initial ).
          " a runtime error or timeout cancels to test sand box
          " without further information.
          " Unless the framework is sick at least one test method
          " must have been started. Align statistics accordingly.
          add 1 to <test_class>-state-count_exec_methods.
          add 1 to me->f_statistic-cnt_test_methods.
        endif.
    endcase.

    clear:
      me->f_test_context-test_class,
      me->f_test_context-test_method.
  endmethod.


  method if_aunit_listener~program_end.
    field-symbols:
      <program>    type ty_program,
      <package>    type ty_stat_package,
      <test_class> type ty_test_class.

    mac_assign_program c_on_miss-assert.
    clear me->f_test_context.
    read table me->f_statistic-packages[]
      assigning <package>
      with key name = <program>-package.
    assert <package> is assigned.

    loop at <program>-test_classes[]
      assigning <test_class>
      where state-has_been_started = abap_true.
      <package>-state-executed_classes =
        <package>-state-executed_classes + 1.
      <package>-state-executed_methods =
        <package>-state-executed_methods + <test_class>-state-count_exec_methods.
      <package>-state-skipped_methods =
        <package>-state-skipped_methods + <test_class>-state-count_skipped_methods.
    endloop.
  endmethod.


  method if_aunit_listener~task_end.
    delete adjacent duplicates from me->f_statistic-packages.
    finish_statistics( ).
  endmethod.


  method handle_failure.
    data:
      adjust_method_totals type abap_bool,
      native_level         type aunit_level,
      alert                type ty_alert,
      descr                type if_aunit_text_description=>ty_s_description,
      count                type i.
    field-symbols:
      <program>     type ty_program,
      <test_class>  type ty_test_class,
      <test_method> type ty_test_method.

    mac_assign_test_method c_on_miss-ignore.

    if ( <program> is assigned ).
      <program>-state-has_issue = abap_true.
    endif.
    if ( <test_class> is assigned ).
      <test_class>-state-issue-has_failure = abap_true.
    endif.

    descr = i_ref_failure->get_header_description( ).
    alert-kind = i_kind.
    alert-context = me->f_test_context.
    alert-description = me->f_text_api->get_string( descr ).
    alert-description =
      escape( val = alert-description format = cl_abap_format=>e_html_text ).

    if i_kind = c_kind-skipped.
      alert-level = c_level-skipped.
      if ( <test_method> is assigned ).
        <test_method>-state-has_been_skipped = abap_true.
        <test_class>-state-issue-has_been_skipped = abap_true.
      elseif ( <test_class>  is assigned ).
        <test_class>-state-issue-has_been_skipped = abap_true.
      endif.

    else.
      native_level = i_ref_failure->get_level( ).
      case native_level.
        when if_aunit_constants=>fatal.
          add 1 to me->f_statistic-cnt_failure-fatal.
          alert-level = c_level-fatal.
        when if_aunit_constants=>critical.
          add 1 to me->f_statistic-cnt_failure-critical.
          alert-level = c_level-critical.
        when others.
          add 1 to me->f_statistic-cnt_failure-tolerable.
          alert-level = c_level-tolerable.
      endcase.
      if ( <test_method> is not assigned and <test_class> is assigned ).
        " a timeout or runtime abortion is reported on class level
        " however it happened within a method. Fix totals in such a case
        " by the assumption that at least one method has been executed
        case i_kind.
          when c_kind-rt_failure.
            <test_class>-state-issue-has_rt_failure = abap_true.
            adjust_method_totals = abap_true.
          when c_kind-timeout.
            <test_class>-state-issue-has_timeout = abap_true.
            adjust_method_totals = abap_true.
          when others.
            adjust_method_totals = abap_false.
        endcase.
        if ( abap_true =  adjust_method_totals ).
          case native_level.
            when if_aunit_constants=>fatal.
              add 1 to me->f_statistic-cnt_method-with_fatal.
            when if_aunit_constants=>critical.
              add 1 to me->f_statistic-cnt_method-with_critical.
            when others.
              add 1 to me->f_statistic-cnt_method-with_tolerable.
          endcase.
        endif.
      endif.
    endif.

    count = lines( me->f_alerts ).
    count = count mod 2.
    if ( 1 = count ).
      alert-apply_zebra = abap_true.
    endif.

    insert alert into table me->f_alerts[].

    if <test_method> is assigned.
      if <test_method>-alert-level < alert-level.
        <test_method>-alert = alert.
      endif.
    endif.
  endmethod.


  method if_aunit_listener~assert_failure.
    data:
      header type if_aunit_text_description=>ty_s_description.
    header = failure->get_header_description( ).
    case header-id.
      when c_alert_id-prerequisite.
        handle_failure( i_kind = c_kind-skipped i_ref_failure = failure ).

      when c_alert_id-timeout.
        handle_failure( i_kind = c_kind-timeout i_ref_failure = failure ).

      when others.
        handle_failure( i_kind = c_kind-assert_failure i_ref_failure = failure ).

    endcase.
  endmethod.


  method if_aunit_listener~warning.
    data:
      header type if_aunit_text_description=>ty_s_description.
    header = warning->get_header_description( ).
    case header-id.
      when c_alert_id-prerequisite.
        handle_failure( i_kind = c_kind-skipped i_ref_failure = warning ).

      when c_alert_id-timeout.
        handle_failure( i_kind = c_kind-timeout i_ref_failure = warning ).

      when others.
        handle_failure( i_kind = c_kind-warning i_ref_failure = warning ).

    endcase.
  endmethod.


  method if_aunit_listener~cx_failure.
    handle_failure( i_kind = c_kind-cx_failure i_ref_failure = failure ).
  endmethod.


  method if_aunit_listener~rt_failure.
    handle_failure( i_kind = c_kind-rt_failure i_ref_failure = failure ).
  endmethod.


  method if_aunit_listener~execution_event ##needed.
  endmethod.


  method initialize_program_entry.
    data:
      escaped_name type string,
      program      type ty_program.
    field-symbols:
      <program> type ty_program.

    read table me->f_programs with key
      name = i_program_name
      assigning <program>.
    if ( 0 ne sy-subrc ).
      program-name = i_program_name.
      cl_aunit_prog_info=>progname_to_tadir(
        exporting
          progname = program-name
        importing
          obj_name = program-obj_name
          obj_type = program-obj_type ).
      insert program into table me->f_programs assigning <program>.
    endif.

    if ( abap_true eq me->f_layout-with_adt_uri and
         <program>-obj_name is not initial ).
      escaped_name =
        escape( val = <program>-obj_name format = cl_abap_format=>e_uri_full ).

      case <program>-obj_type.
        when 'CLAS'.
          me->f_test_context-adt_resource_uri =
           'adt://' && sy-sysid && '/sap/bc/adt/oo/classes/' && escaped_name ##no_Text.

        when 'PROG'.
          me->f_test_context-adt_resource_uri =
           'adt://' && sy-sysid && '/sap/bc/adt/programs/programs/' && escaped_name ##no_Text.

        when 'FUGR'.
          me->f_test_context-adt_resource_uri =
          'adt://' && sy-sysid && '/sap/bc/adt/functions/groups/' && escaped_name ##no_Text.

        when others.
          clear me->f_test_context-adt_resource_uri.

      endcase.
      <program>-adt_resource_uri = me->f_test_context-adt_resource_uri.
    endif.
  endmethod.


  method finish_statistics.
    data:
      fraction_of_4       type i,
      nbr_of_fill_entries type i.
    field-symbols:
      <test_class> type ty_test_class,
      <program>    type ty_program,
      <package>    type ty_stat_package.

    me->f_statistic-cnt_failure-syntax_error = lines( me->f_syntax_errors ).
    add_devc_uri_syntax_errors( ).

    loop at me->f_programs transporting no fields where is_permitted is initial.
      me->f_statistic-cnt_no_permission = me->f_statistic-cnt_no_permission + 1.
    endloop.

    loop at me->f_programs assigning <program> where state is not initial.
      add 1 to me->f_statistic-cnt_programs.
      loop at <program>-test_classes assigning <test_class> where state is not initial.
        add 1 to me->f_statistic-cnt_test_classes.
        loop at <test_class>-test_methods transporting no fields where state is not initial.
          add 1 to me->f_statistic-cnt_test_methods.
        endloop.
      endloop.
    endloop.

    me->f_statistic-cnt_packages = lines( me->f_statistic-packages ).

    me->f_statistic-cnt_failure-total =
      me->f_statistic-cnt_failure-critical +
      me->f_statistic-cnt_failure-fatal +
      me->f_statistic-cnt_failure-tolerable.

    if ( abap_true eq me->has_timeout( ) and
         abap_false eq me->has_failure( ) and
         abap_false eq me->has_warning( ) and
         me->f_statistic-cnt_failure-syntax_error is initial ).
      me->f_statistic-has_timeout_only = abap_true.
    endif.

    get time.
    me->f_time_interval-finished_on = sy-datlo.
    me->f_time_interval-finished_at = sy-timlo.
    if ( me->f_time_interval-finished_on < me->f_time_interval-started_on  or
         me->f_time_interval-time_zone  <> sy-zonlo ).
      clear me->f_time_interval-finished_on.
      clear me->f_time_interval-finished_at.
    elseif ( me->f_time_interval-finished_on = me->f_time_interval-started_on  ).
      clear me->f_time_interval-finished_on.
    endif.

    " prepare package data to ease rendering
    sort me->f_statistic-packages by name ascending.
    if ( me->f_statistic-packages is not initial ).
      loop at me->f_statistic-packages assigning <package>.
        fraction_of_4 = sy-tabix  mod 4.
        if ( 1 eq fraction_of_4 and sy-tabix <> 1 ).
          <package>-new_line = abap_true.
        endif.
      endloop.
      nbr_of_fill_entries = ( 4 - fraction_of_4 ) mod 4.
      do nbr_of_fill_entries times.
        insert initial line into table me->f_statistic-packages.
      enddo.
    endif.
  endmethod.

endclass.


class globalization_service implementation.

  method get_text.
    result-title-detailed_log =         'Detailed Log'(tlg).
    if ( abap_true eq i_contain_alerts_skipped_tests ).
      result-title-failures =           'Failures and Skipped Tests'(tfs).
    else.
      result-title-failures =           'Failures'(tfl).
    endif.
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
  endmethod.
endclass.


**********************************************************************
form initialization.
  create object g_test_runner.

  p_rsklvl = cl_aunit_permission_control=>get_max_risk_level( ).
  p_durlvl = if_aunit_attribute_enums=>c_duration-long.

  so_email-low =  cl_aucv_job_utilities=>get_user_email( i_uname  = sy-uname ).
  so_email-sign = 'I'.
  so_email-option = 'EQ'.
  insert so_email into table so_email[].

  if ( '731' <= sy-saprl ).
    g_adt_uri_is_supported = abap_true.
    p_adturi = abap_true.
  else.
    g_adt_uri_is_supported = abap_false.
    p_adturi = abap_false.
  endif.

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* with each change of selection-screen: set this value
* constants c_selection_screen_changed_on type sydatum value '20100720'.
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  create object r_prog_param
    exporting
      i_report                      = sy-repid
      i_selection_screen_changed_on = c_selection_screen_changed_on. "20.07.2010 '20100720'
  r_prog_param->delete_if_selscreen_changed( ).
  r_prog_param->delete_expired_values( ).
  g_test_runner->get_params( r_prog_param ).
endform.


form at_selection_screen_output.
  data tab type string_table ##needed.

  call function 'RS_SET_SELSCREEN_STATUS'
    exporting
      p_status  = 'STAT1000'
    tables
      p_exclude = tab.

  perform sub_mode.
endform.


form at_selection_screen.
  if sy-ucomm = 'SET_DEFAULT'.
    g_test_runner->reset_params( ).
  endif.
  if sy-ucomm = 'ONLI' or sy-ucomm = 'SJOB'.
    if b_devc = abap_true.
      if so_devc[] is not initial.
        p_selcl = p_selfg = p_selprg = abap_true.
      else.
        message e002. "check devc not initial
      endif.
    endif.
    if b_obj = abap_true.
      if so_class[] is initial and
         so_fugr[]  is initial and
         so_prog[]  is initial.
        message e003. "check devc not initial
      endif.
      clear: p_selcl, p_selfg, p_selprg.
      if so_class[] is not initial.
        p_selcl = abap_true.
      endif.
      if so_fugr[]  is not initial.
        p_selfg = abap_true.
      endif.
      if so_prog[]  is not initial.
        p_selprg = abap_true.
      endif.
    endif.
  endif.
  if sy-ucomm = 'SJOB' and b_direct = abap_true.
    message e004. "check devc not initial
  endif.
  if ( sy-ucomm = 'ONLI' or sy-ucomm = 'SJOB' )
       and b_email = abap_true and so_email[] is initial.
    message e005. "check devc not initial
  endif.
endform.


form sub_mode.
  loop at screen.
    case screen-group1.
      when 'DVC'.
        if b_devc = abap_true.
          screen-active = 1.
          screen-invisible = 0.
        else.
          screen-active = 0.
          screen-invisible = 1.
        endif.
      when 'OBJ'.
        if b_obj = abap_true.
          screen-active = 1.
          screen-invisible = 0.
        else.
          screen-active = 0.
          screen-invisible = 1.
        endif.
      when 'XCL' or 'XC2'.
        if p_excl = abap_true and b_devc = abap_true.
          screen-active = 1.
          screen-invisible = 0.
          if p_packr = abap_false and screen-group1 = 'XC2'.
            screen-active = 0.
            screen-invisible = 1.
          endif.
        else.
          screen-active = 0.
          screen-invisible = 1.
        endif.
      when 'EML'.
        if b_email = abap_true.
          screen-active = 1.
          screen-invisible = 0.
        else.
          screen-active = 0.
          screen-invisible = 1.
        endif.
      when 'SML'.
        if b_email = abap_true and g_adt_uri_is_supported = abap_true.
          screen-active = 1.
          screen-invisible = 0.
        else.
          screen-active = 0.
          screen-invisible = 1.
        endif.
      when 'COV'.
        if b_direct = abap_true.
          screen-active = 1.
          screen-invisible = 0.
        else.
          screen-active = 0.
          screen-invisible = 1.
        endif.
      when others. continue.
    endcase.
    modify screen.
  endloop.

endform.


form start_of_selection.
  g_test_runner->run( ).
endform.


class tc_statistics definition for testing
    inheriting from cl_aunit_assert
    risk level harmless duration short.

  public section.
    interfaces:
      lif_aunit_test.

  private section.
    methods:
      setup,
      critical_failures for testing,
      tolerable_failures for testing,
      skipped for testing.

    data:
      f_listener type ref to email_listener,
      f_driver   type ref to test_runner,
      f_failure  type ref to if_aunit_info_failure.
endclass.


class td_failure definition create private for testing.

  public section.
    interfaces:
      if_aunit_info_failure partially implemented.
    class-methods:
      create
        importing
          i_level           type aunit_level optional
          i_id              type sychar04 optional
            preferred parameter i_level
        returning
          value(r_instance) type ref to td_failure.


  private section.
    data:
       level type aunit_level.
endclass.


class td_failure implementation.
  method create.
    create object r_instance.
    r_instance->level = i_level.
  endmethod.

  method if_aunit_info_failure~get_level.
    result = level.
  endmethod.

  method if_aunit_info_failure~get_stack_description ##needed.
  endmethod.

  method if_aunit_info_message~get_analysis_description ##needed.
  endmethod.

  method if_aunit_info_message~get_analysis_documents ##needed.
  endmethod.

  method if_aunit_info_message~get_complete_description ##needed.
  endmethod.

  method if_aunit_info_message~get_header_description ##needed.
  endmethod.

endclass.


class tc_statistics implementation.

  method setup.
    create object me->f_driver.
    me->f_listener ?= email_listener=>create_listener(
      i_programs = me->f_driver->f_programs i_au_factory = me->f_driver->f_au_factory ).

  endmethod.


  method critical_failures.
    me->f_failure = td_failure=>create( if_aunit_constants=>critical ).

    me->f_listener->if_aunit_listener~assert_failure( me->f_failure ).
    me->f_listener->if_aunit_listener~assert_failure( me->f_failure ).

    assert_equals( act = me->f_listener->f_statistic-cnt_failure-critical
                   exp = 2 ).
  endmethod.


  method tolerable_failures.
    me->f_failure = td_failure=>create( if_aunit_constants=>tolerable ).

    me->f_listener->if_aunit_listener~assert_failure( me->f_failure ).
    me->f_listener->if_aunit_listener~assert_failure( me->f_failure ).
    me->f_listener->if_aunit_listener~assert_failure( me->f_failure ).

    assert_equals(
      act = me->f_listener->f_statistic-cnt_failure-tolerable
      exp = 3 ).
  endmethod.


  method skipped.
    me->f_failure = td_failure=>create( i_id = c_alert_id-prerequisite ).

    me->f_listener->if_aunit_listener~assert_failure( me->f_failure ).
    me->f_listener->if_aunit_listener~assert_failure( me->f_failure ).
    me->f_listener->if_aunit_listener~assert_failure( me->f_failure ).

    assert_equals(
      act = me->f_listener->f_statistic-cnt_failure-tolerable
      exp = 3 ).
  endmethod.

endclass.
