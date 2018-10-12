*&---------------------------------------------------------------------*
*& Report ZZCONNECT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZZTIMER.

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
   data xi type int1.
   xi = x_hex.
   x_hex = xi.

   write: | finish { x_xstr }  |.
