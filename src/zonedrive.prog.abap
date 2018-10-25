*&---------------------------------------------------------------------*
*& Report ZZCONNECT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZONEDRIVE.

  class lcl_receiver definition.
    public section.
    methods:
      handle_finished for event finished of cl_gui_timer.
  ENDCLASS.

  class lcl_receiver IMPLEMENTATION.
    method handle_finished.
      write: | timer ends |.
    ENDMETHOD.
  ENDCLASS.

  data:
    timer    type ref to cl_gui_timer,
    receiver type ref to lcl_receiver.

  constants:
    c_interval type i value 3.

START-OF-SELECTION.


  " cl_http_client=>create(
  "    exporting
  "      host    = |portal.wdf.sap.corp|
  "      service = |443|
  "    importing client = data(x_client) ).
   "cl_http_client=>create_by_destination(
   "     exporting destination = |ldciha5_HA5_01|
   "     importing client = data(x_client) ).

"      exporting
"        host    = |portal.wdf.sap.corp|
"        service = |443|
"      importing client = data(x_client) ).
"ldciha5_HA5_01
   cl_http_client=>create(
      exporting
        host    = |login.live.com|
        service = |443|
      importing client = data(x_client) ).

   x_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
   x_client->request->set_method( if_http_request=>co_request_method_get ).

   cl_http_utility=>set_request_uri(
     request = x_client->request
     uri     = |/oauth20_authorize.srf|
               && |?response_type=token|
               && |?client_id=f36494cc-f94e-4131-8bc2-83e3db7191d8|
               && |&scope=onedrive.readwrite openid offline_access|
               && |&redirect_uri=https://ldciha5.wdf.sap.corp:44301/sap/bc/abap/z_eezz_auth| ).

   "cl_http_utility=>set_request_uri(
   "  request = x_client->request
   "  uri     = |/sap/bc/bsp/sap/z_sapctrl_html/ntmmcalgreen.ico| ).

    call method x_client->send
       exporting  timeout = 1000
       exceptions http_communication_failure  = 1
                  http_invalid_state          = 2
                  http_processing_failed      = 3
                  others                      = 4.
    call method x_client->receive
       exceptions http_communication_failure  = 1
                  http_invalid_state          = 2
                  http_processing_failed      = 3
                  others                      = 4.

    data x_contstr type string.
    data(x_content) = x_client->response->get_data( ).
    data(x_convert) = cl_abap_conv_in_ce=>create( input = x_content encoding = 'UTF-8' ignore_cerr = abap_true ).
    x_convert->read( importing data = x_contstr ).

    x_client->close( ).

   "data(conv_out) = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' endian = 'L' ).
   "data(conv_in)  = cl_abap_conv_in_ce=>create(  encoding = 'UTF-8' endian = 'L' ).
   " conv_in->convert( exporting
   "conv_out->convert( exporting data = x_hex importing buffer = x_str ).
   "conv_out->convert( exporting data = x_str importing buffer = x_hex ).
   " conv->write( data = x_hex n = 1 ).
   " xxx = new xstring( conv->get_buffer( ) ).
   " xxx = conv->get_buffer( ).
   " conv->convert( EXPORTING input = xstr IMPORTING data = x_hex ).
   " data xxx2 type xstring.
   " xxx2 = xxx->*.

   " conv->convert( EXPORTING input = xxx->* IMPORTING data = x_hex ).
   "data xi type int1.
   "xi = x_hex.
   "x_hex = x_xstr.

   write: | finish   |.
