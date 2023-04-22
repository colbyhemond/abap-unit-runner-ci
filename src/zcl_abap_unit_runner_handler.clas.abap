CLASS zcl_abap_unit_runner_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAP_UNIT_RUNNER_HANDLER IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

    data(data_xstring) = server->request->get_cdata( ).



    if 1 = 2. endif.

*    DATA(abap_unit_runner) = zcl_abap_unit_runner=>create( ).
*
*    abap_unit_runner->run_class( 'test' ).

    server->response->set_status(
      EXPORTING
        code          = '200'               " HTTP Status Code
        reason        = 'No Coffee'                 " HTTP status description
*        detailed_info =
    ).

  ENDMETHOD.
ENDCLASS.
