*&---------------------------------------------------------------------*
*& Report ZZJSONTEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZZJSONTEST.


START-OF-SELECTION.
  data(xjson1) = new zcl_eezz_json( iv_json = '{"animation":{"name":"e1", "anchor":"a1"}}').
  data(xjson2) = new zcl_eezz_json( iv_json = '{"animation":{"name":"e2", "anchor":"a2"}}').
  data(xjson4) = new zcl_eezz_json( iv_json = '{"animation":["a","b","c"]}').

  data(xwrt)   = new cl_abap_string_c_writer( ).

  data(xjson3) = new zcl_eezz_json( ).

  xjson3->join( it_json = xjson1->get( '/animation' ) iv_key = 'elem1' ).
  xjson3->join( it_json = xjson2->get( 'animation' ) iv_key = 'elem2' ).

  data(xstream) = xjson3->dump( ).
  " xjson2->dump( xwrt ).
  write: xstream->get_result_string( ).
  write 'ok'.
