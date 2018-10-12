class ZCL_SAPCONTROL definition
  public
  inheriting from ZCL_EEZZ_TABLE
  final
  create public .

public section.

  methods GET_INSTANCES
    importing
      !PATH type STRING
    returning
      value(RT_TABLE) type ref to ZCL_EEZZ_TABLE .
  methods GET_MONITOR_ELEMENTS
    importing
      !PATH type STRING
    returning
      value(RT_TABLE) type ref to ZCL_EEZZ_TABLE .
  methods GET_PROCESS_LIST
    importing
      !PATH type STRING
    returning
      value(RT_TABLE) type ref to ZCL_EEZZ_TABLE .
  methods GET_SYSTEMS
    returning
      value(RT_TABLE) type ref to ZCL_EEZZ_TABLE .
  methods CONSTRUCTOR .

  methods ZIF_EEZZ_TABLE~ON_DOWNLOAD
    redefinition .
  methods ZIF_EEZZ_TABLE~PREPARE_DOWNLOAD
    redefinition .
protected section.
private section.

  data M_TRANSFERRED type I .
  data M_SNAPSHOT_TBL type ref to DATA .
  data M_SNAPSHOT_ZIP type ref to CL_ABAP_STRING_X_WRITER .
  data M_SNAPSHOT_JSN type ref to ZCL_EEZZ_JSON .

  methods PARSE_XML
    importing
      !IV_XMLSTREAM type STRING
    returning
      value(RV_DOCUMENT) type ref to IF_IXML_DOCUMENT .
ENDCLASS.



CLASS ZCL_SAPCONTROL IMPLEMENTATION.


  method CONSTRUCTOR.
    super->constructor( ).


  endmethod.


  method get_instances.
    types: begin of tstr_line,
             dispstatus   type x length 1,
             name         type string,
             instancetype type string,
           end of tstr_line.

    types: tty_table type table of tstr_line with key name initial size 0.

    data x_table       type ref to tty_table.
    data x_instance    type ref to ztty_eezz_json.
    data x_instlist    type ref to zstr_eezz_json.
    data x_wa_instance type tstr_line.

    if m_snapshot_jsn is not bound.
      return.
    endif.

    try.
        " path points to a system name "QD1"
        x_instance = m_snapshot_jsn->get( iv_path = path ).
        x_table    = new tty_table( ).

        loop at x_instance->* into data(x_wa).
          data x_procstat type x length 1 value 0.
          data x_status   type x length 1.

          " get the status of all processes
          " construct path to an instance "QD1/localhost 00"
          data(x_process_list) = get_process_list( path = |{ path }/{ x_wa-c_key }| ).
          data(x_tbl_status)   = x_process_list->get_status( ).

          x_status = x_tbl_status->*[ c_key = 'dispstatus' ]-c_status.

          do 4 times.
            get bit 4 + sy-index of x_status into data(x_val).
            if x_val = 1.
              set bit 4 + sy-index of x_procstat.
              exit.
            endif.
          enddo.

          append value #( name = x_wa-c_key dispstatus = x_procstat instancetype = |D0| ) to x_table->*.
        endloop.

        data(x_eezz_table)   = new zcl_eezz_table( iv_table = x_table ).
        rt_table             = x_eezz_table.

      catch cx_sy_itab_line_not_found.
    endtry.
  endmethod.


  method GET_MONITOR_ELEMENTS.
    types: begin of tstr_line,
             _eezz_row_cell_ type string,
             dispstatus      type x length 1,
             name            type string,
           end of tstr_line.

    types: tty_table type table of tstr_line with key name initial size 0.

    data x_tbl_elements type ref to tty_table.
    data x_instance     type ref to ztty_eezz_json.

    if m_snapshot_jsn is not bound.
      return.
    endif.

    x_instance = m_snapshot_jsn->get( iv_path = path ).
    data(x_elements) = x_instance->*[ c_key = |MonitoringElements| ].

    data(x_proclist) = get_process_list( path = path ).

    x_tbl_elements = new tty_table( ).
    x_tbl_elements->* = value #(
      ( _eezz_row_cell_ = |Processes|      dispstatus = 2 name = |Process List|     )
      ( _eezz_row_cell_ = |CurrentStatus|  dispstatus = 2 name = |Current Status|   )
      ( _eezz_row_cell_ = |OpenAlerts|     dispstatus = 2 name = |Open Alerts|      )
      ( _eezz_row_cell_ = |Syslogs|        dispstatus = 2 name = |Syslog|           )
      ( _eezz_row_cell_ = |QueueStatistic| dispstatus = 2 name = |Queue Statistic|  )
      ( _eezz_row_cell_ = |AccessPoints|   dispstatus = 2 name = |Access Points|    )
      ( _eezz_row_cell_ = |AbapWpTable|    dispstatus = 2 name = |AS ABAP WP Table| )
    ).

    x_elements-c_object = new zcl_eezz_table( iv_table = x_tbl_elements ).
    rt_table = cast #( x_elements-c_object ).
  endmethod.


  method get_process_list.
    types: begin of tstr_line,
             name        type string,
             description type string,
             dispstatus  type string,
             textstatus  type string,
             starttime   type string,
             elapsedtime type string,
             pid         type string,
           end of tstr_line.

    types tty_table type table of tstr_line with key name initial size 0.

    data x_status     type x length 1.
    data x_strstatus  type xstring.
    data x_instance   type ref to ztty_eezz_json.
    data x_proclist   type zstr_eezz_json.
    data x_wa_process type tstr_line.


    if m_snapshot_jsn is not bound.
      return.
    endif.

    x_instance = m_snapshot_jsn->get( iv_path = path ).
    x_proclist = x_instance->*[ c_key = |GetProcessList.xml| ].

    if x_proclist-c_object is bound.
      rt_table = cast #( x_proclist-c_object ).
      return.
    endif.

    data(x_tbl_table)   = new tty_table( ).
    data(x_element)     = me->parse_xml( x_proclist-c_value ).

    data(x_processor)   = new cl_xslt_processor( ).
    x_processor->set_source_node( x_element ).
    x_processor->set_expression( |//item| ).
    x_processor->run( progname = space ).
    data(x_nodelist)    = x_processor->get_nodes( ).
    data(x_iterator)    = x_nodelist->create_iterator( ).

    do 1000 times.
      data(x_next) = x_iterator->get_next( ).
      if x_next is not bound.
        exit.
      endif.

      data(x_filter) = x_next->create_filter_node_type( if_ixml_node=>co_node_element ).
      data(x_sub_it) = x_next->create_iterator_filtered( x_filter ).
      clear x_wa_process.

      do 1000 times.
        data(x_sub_next) = x_sub_it->get_next( ).
        if x_sub_next is not bound.
          exit.
        endif.

        data(x_name)  = x_sub_next->get_name( ).
        data(x_value) = x_sub_next->get_value( ).

        case x_name.
          when 'name'.        x_wa_process-name        = x_value.
          when 'description'. x_wa_process-description = x_value.
          when 'textstatus'.  x_wa_process-textstatus  = x_value.
          when 'starttime'.   x_wa_process-starttime   = x_value.
          when 'elapsedtime'. x_wa_process-elapsedtime = x_value.
          when 'pid'.         x_wa_process-pid         = x_value.
          when 'dispstatus'.  x_wa_process-dispstatus  = x_value.
            case x_value.
              when 'SAPControl-GRAY'.   set bit 8 of x_status.
              when 'SAPControl-GREEN'.  set bit 7 of x_status.
              when 'SAPControl-YELLOW'. set bit 6 of x_status.
              when 'SAPControl-RED'.    set bit 5 of x_status.
            endcase.
        endcase.
      enddo.
      append x_wa_process to x_tbl_table->*.
    enddo.

    x_strstatus         = x_status.
    data(x_eezz_table)  = new zcl_eezz_table( iv_table = x_tbl_table ).
    x_eezz_table->set_status( iv_key = |dispstatus| iv_status = x_strstatus ).

    x_proclist-c_object = cast #( x_eezz_table ).
    rt_table            = cast #( x_eezz_table ).
  endmethod.


  method get_systems.
    types: begin of type_systems,
             dispstatus type string,
             name       type string,
           end of type_systems.

    types: tbl_systems type table of type_systems with key name initial size 0.
    data x_status      type x length 1.
    data x_content     type xstring.
    data x_ref_x       type ref to xstring.
    data x_filedata    type string.

    data(x_ref_systems) = new tbl_systems( ).

    if m_snapshot_zip is bound.
      m_snapshot_jsn = new zcl_eezz_json( ).
      data(x_zip)    = new cl_abap_zip( ).

      x_zip->load( zip = m_snapshot_zip->get_result_string( ) ).

      loop at x_zip->files into data(x_file).
        x_zip->get( exporting index = sy-tabix importing content = x_content ).
        data(x_convert) = cl_abap_conv_in_ce=>create( input = x_content encoding = 'UTF-8' ignore_cerr = abap_true ).
        x_convert->read( importing data = x_filedata ).
        find first occurrence of |{ cl_abap_char_utilities=>cr_lf }{ cl_abap_char_utilities=>cr_lf }| in x_filedata results data(x_offset).
        x_filedata = x_filedata+x_offset-offset.
        x_filedata = x_filedata+x_offset-length.
        m_snapshot_jsn->join( iv_key = x_file-name iv_value = x_filedata iv_create = abap_true ).
      endloop.

      clear m_snapshot_zip.
    endif.

    if m_snapshot_jsn is bound.
      data(x_jsn_tbl) = m_snapshot_jsn->get( ).

      loop at x_jsn_tbl->* into data(x_wa).
        clear x_status.
        data(x_system_list) = cast ztty_eezz_json( x_wa-c_ref ).

        loop at x_system_list->* into data(x_wa_instance).
          data(x_path) = |{ x_wa-c_key }/{ x_wa_instance-c_key }|.
          data(x_process_list) = get_process_list( path = x_path ).

          data(xtbl_stat) = x_process_list->get_status( ).
          x_status        = xtbl_stat->*[ c_key = |dispstatus| ]-c_status.
          m_snapshot_jsn->join( iv_key = |{ x_path }/MonitoringElements| iv_create = abap_true ).
        endloop.

        insert value #( dispstatus = |{ x_status }| name = x_wa-c_key ) into table x_ref_systems->*.
      endloop.
    endif.

    rt_table = new zcl_eezz_table( iv_table = x_ref_systems ).
  endmethod.


  method PARSE_XML.
      data(x_xml_cl)         = cl_ixml=>create( ).
      data(x_stream_factory) = x_xml_cl->create_stream_factory( ).
      data(x_in_stream)      = x_stream_factory->create_istream_string( IV_XMLSTREAM ).
      data(x_document)       = x_xml_cl->create_document( ).
      data(x_xml_parser)     = x_xml_cl->create_parser( document = x_document stream_factory = x_stream_factory istream = x_in_stream ).

      if x_xml_parser->parse( ) = 0.
        rv_document = x_document.
      endif.
  endmethod.


  method zif_eezz_table~on_download.
    types: begin of tstr_line,
             c_inx  type string,
             c_data type xstring,
           end of tstr_line.
    types: tty_table type table of tstr_line with key c_inx initial size 0.
    data x_ref_tbl   type ref to tty_table .
    data x_transfer  type i.
    data x_update    type ztty_update.

    try.
        if m_snapshot_tbl is initial.
          m_snapshot_tbl = new tty_table( ).
          m_transferred  = 0.
        endif.

        x_ref_tbl = cast #( m_snapshot_tbl ).

        if iv_message->get_message_type( ) = iv_message->co_message_type_text.
          data(x_json)       = new zcl_eezz_json( iv_json = iv_message->get_text( ) ).
          data(x_progress)   = x_json->get_value( |progress| ).
          data(x_filesize)   = x_json->get_value( |file/size| ).
          data(x_transfered) = x_json->get_value( |file/chunkSize| ).
          data(x_chunksize)  = x_json->get_value( |chunkSize| ).
          data(x_sequence)   = x_json->get_value( |file/sequence| ).

          append value #( c_inx = x_sequence ) to x_ref_tbl->*.
          data(x_segments)  = lines( x_ref_tbl->* ).
          data(x_prog_seq)  = ( x_segments + 1 ) * ( x_chunksize / x_filesize ) * 100.
          x_prog_seq        = nmin( val1 = x_prog_seq  val2 = 100 ).
          x_update = value #(
            ( c_key = |{ x_progress }.innerHTML|   c_value = |{ x_prog_seq }%| )
            ( c_key = |{ x_progress }.style.width| c_value = |{ x_prog_seq }%| )
          ).

          if m_transferred = 0.
            m_transferred  =  x_filesize.
          endif.
          m_transferred    = m_transferred - x_transfered.
          rv_update        = zcl_eezz_json=>gen_response( ref #( x_update ) ).
        else.
          x_segments  = lines( x_ref_tbl->* ).
          modify x_ref_tbl->* index x_segments from value #( c_data = iv_message->get_binary( ) ) transporting c_data.

          if m_transferred = 0.
            sort x_ref_tbl->* by c_inx.
            loop at x_ref_tbl->* into data(x_wa).
              m_snapshot_zip->write( x_wa-c_data ).
            endloop.
            clear x_ref_tbl->*.
            clear m_snapshot_tbl.
          endif.
        endif.
      catch cx_apc_error.
    endtry.
  endmethod.


  method zif_eezz_table~prepare_download.
*CALL METHOD SUPER->ZIF_EEZZ_TABLE~PREPARE_DOWNLOAD
*  EXPORTING
*    IV_MESSAGE =
*    .
    m_snapshot_zip = new cl_abap_string_x_writer( ).
    clear m_snapshot_tbl.
  endmethod.
ENDCLASS.
