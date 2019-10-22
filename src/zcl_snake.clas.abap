class ZCL_SNAKE definition
  public
  inheriting from ZCL_EEZZ_TABLE
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods GET_SEGMENTS
    returning
      value(RT_TABLE) type ref to ZCL_EEZZ_TABLE .
protected section.
private section.

  data MT_SEGMENTS type ref to ZTTY_SEGMENTS .
ENDCLASS.



CLASS ZCL_SNAKE IMPLEMENTATION.


  method CONSTRUCTOR.
    super->constructor( ).
    mt_segments = new ztty_segments( ).

    mt_segments->* = value #(
        ( c_posx =  10  c_posy = 10 )
        ( c_posx = 300  c_posy = 50 )
        ( c_posx = 600  c_posy = 100 )
    ).
  endmethod.


  method GET_SEGMENTS.


    data(x_posx) = mt_segments->*[ 1 ]-c_posx + 10.
    data(x_posy) = mt_segments->*[ 1 ]-c_posy.

    modify  mt_segments->* index 1 from value #( c_posx = x_posx c_posy = x_posy ).

    if x_posx > 400.
      clear mt_segments.
    endif.

    try.
        data(x_eezz_table) = new zcl_eezz_table( iv_table = mt_segments ).
        data(x_dictionary) = x_eezz_table->get_dictionary( ).
        "modify table x_dictionary->* from value #( c_key = 'c_posx' c_value = x_path ).
        "modify table x_dictionary->* from value #( c_key = 'table_key'  c_value = x_path ).

        " xtbl_node->c_object   = cast #( x_eezz_table ).
        rt_table             = cast #( x_eezz_table ).
      catch cx_root .
    endtry.

  endmethod.
ENDCLASS.
