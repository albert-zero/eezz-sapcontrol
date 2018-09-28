class ZCL_SAPCONTROL definition
  public
  inheriting from ZCL_EEZZ_TABLE
  final
  create public .

public section.

  methods GET_INSTANCES
    returning
      value(RT_TABLE) type ref to ZCL_EEZZ_TABLE .
  methods CONSTRUCTOR .

  methods ZIF_EEZZ_TABLE~ON_DOWNLOAD
    redefinition .
  methods ZIF_EEZZ_TABLE~PREPARE_DOWNLOAD
    redefinition .
protected section.
private section.

  data M_TBL_INSTANCES type ref to DATA .
ENDCLASS.



CLASS ZCL_SAPCONTROL IMPLEMENTATION.


  method CONSTRUCTOR.
    super->constructor( ).


  endmethod.


  method get_instances.
    types: begin of type_instances,
             name type string,
           end of type_instances.

    types: tbl_instances type table of type_instances with key name initial size 0.
    types: ref_instances type ref to tbl_instances.

    data(ref_instances) = new tbl_instances( ).
    " m_tbl_instances
    append value #( name = |YY2| ) to ref_instances->*.

    rt_table = new zcl_eezz_table( iv_table = ref_instances ).
  endmethod.


  method zif_eezz_table~on_download.
    if iv_message->get_message_type( ) = iv_message->co_message_type_text.
      rv_update = super->zif_eezz_table~on_download( iv_message = iv_message ).
    else.
      " store file data
    endif.
  endmethod.


  method ZIF_EEZZ_TABLE~PREPARE_DOWNLOAD.
*CALL METHOD SUPER->ZIF_EEZZ_TABLE~PREPARE_DOWNLOAD
*  EXPORTING
*    IV_MESSAGE =
*    .
  endmethod.
ENDCLASS.
