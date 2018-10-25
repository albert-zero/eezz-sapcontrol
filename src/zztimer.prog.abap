*&---------------------------------------------------------------------*
*& Report ZZCONNECT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZZTIMER.
INCLUDE icmdef.
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
  "create object receiver type lcl_receiver.
  "create object timer.
  "set handler receiver->handle_finished for timer.
  "timer->interval = c_interval.
  "timer->run( ).
  "write: | wait for timer |.
  "FIELD-SYMBOLS:
  "  like line of i_data.
  data x_wa type zeezz_systems.
  x_wa = value #( c_system = |YY2| c_host = |ldai1yy2| c_instance = |00| ).
  insert zeezz_systems from x_wa.

  TYPES:
  BEGIN OF tline,
    id     TYPE c LENGTH 1,
    number TYPE i,
  END OF tline.

  DATA: itab TYPE STANDARD TABLE OF tline INITIAL SIZE 0.

  itab = value #(
     ( id = 'a' number = 1 )
     ( id = 'a' number = 10 )
     ( id = 'b' number = 2 )
     ( id = 'c' number = 3 )
     ( id = ''  number = 4 )
  ).

   data(x_icon) = cl_bsp_mimes=>sap_icon( 'ICON_CLOSED_FOLDER' ).

   data x_hex type x.
   SET BIT 8 OF x_hex.
   SET BIT 7 OF x_hex.
   SET BIT 5 OF x_hex.

   data x_xstr type xstring.
   x_xstr = x_hex.
   data x_cstr type string.
   x_cstr = '0B'.
   x_xstr = x_cstr.

  data: param_info type table of THLINES.

  data(x_icm)   = new cl_icm_api( ).
  data(x_param) = x_icm->get_param_info( ).
  data x_found type abap_bool value abap_false.
  data x_port  type string.

  loop at x_param into data(x_icmp_wa).
    if x_icmp_wa cs |icm/server_port| and x_icmp_wa cs |HTTP|.
      write: / |{ x_icmp_wa }  |.

      split x_icmp_wa at '=' into data(xKey) data(xVal).
      write: / |key={ xKey }  val={ xVal }|.

      split xVal at ',' into table data(xValTbl).
      loop at xValTbl into data(x_val_wa).
        split x_val_wa at '=' into xKey xVal.
        if xVal cp |HTTP|.
          write: / |found entry: key={ xKey }  val={ xVal }|.
          x_found = abap_true.
        endif.

        if xKey cs |PORT|.
          x_Port = xVal.
        endif.
      endloop.

      if x_found = abap_true.
        exit.
      endif.
    endif.
  endloop.
  write: / |found entry: port={ x_port }|.

  data(x_protstr) = x_icm->map_protocol( ICM_PLUGIN_PROTOCOL_HTTP ).
  write: / |found entry: port={ x_protstr }|.

   cl_http_client=>create_by_url(
      " exporting url = |https://ldciha5.wdf.sap.corp:44301/sap/bc/bsp/sap/z_sapctrl_html/sapcontrol.htm|
      exporting url    = |http://ldciha5.wdf.sap.corp:50001|
      importing client = data(x_client) ).

   cl_http_utility=>set_request_uri( request = x_client->request
                                     uri     = |http://ldciha5.wdf.sap.corp:50001/sap/bc/bsp/sap/z_sapctrl_html/ntmmcaccesspl.ico| ).

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
   x_hex = x_xstr.

   write: | finish { x_hex }  |.
