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

    TYPES:
      BEGIN OF ty_abapunit_runner_data,
        class TYPE string,
      END OF ty_abapunit_runner_data.

    TYPES:
      BEGIN OF ty_response_data,
        result TYPE zif_abap_unit_runner=>ty_runner_result,
      END OF ty_response_data.

    DATA abapunit_runner_data TYPE ty_abapunit_runner_data.
    DATA response_data TYPE ty_response_data.

    TRY.

        DATA(json_data) = server->request->get_cdata( ).

        /ui2/cl_json=>deserialize(
          EXPORTING
            json             = json_data                 " JSON string
          CHANGING
            data             = abapunit_runner_data                " Data to serialize
        ).

        DATA(abap_unit_runner) = zcl_abap_unit_runner=>create( ).

        response_data-result = abap_unit_runner->run_class( CONV #( abapunit_runner_data-class ) ).

        DATA(response_json) = /ui2/cl_json=>serialize(
          data = response_data
          pretty_name = abap_true ).

        server->response->set_cdata( response_json ).

        server->response->set_status(
          EXPORTING
            code          = '200'               " HTTP Status Code
            reason        = 'Unit Tests Processed'         " HTTP status description
        ).

      CATCH /iwbep/cx_mgw_base_exception INTO DATA(iwbep_exception).

        DATA(messages) = iwbep_exception->get_msg_container( )->get_messages( ).

        server->response->set_status(
          EXPORTING
            code          = '500'               " HTTP Status Code
            reason        = CONV #( messages[ 1 ]-message )         " HTTP status description
        ).

      CATCH cx_static_check INTO DATA(exception).

        server->response->set_status(
          EXPORTING
            code          = '500'               " HTTP Status Code
            reason        = 'Unknown exception occured'         " HTTP status description
        ).

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
