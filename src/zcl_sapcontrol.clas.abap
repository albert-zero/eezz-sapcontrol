class ZCL_SAPCONTROL definition
  public
  inheriting from ZCL_EEZZ_TABLE
  final
  create public .

public section.

  aliases DO_SELECT
    for ZIF_EEZZ_TABLE~DO_SELECT .

  methods LOGIN
    importing
      !PATH type STRING
      !USER type STRING
      !PASSWORD type STRING .
  methods SAVE_LOGFILES
    importing
      !PATH type STRING optional
      !FILES type STRING optional
      !UPDATE type STRING optional
      !PROGRESS type STRING optional
      !MANAGER type ref to IF_APC_WSP_MESSAGE_MANAGER optional
      !MESSAGE type ref to IF_APC_WSP_MESSAGE optional .
  methods GET_DEVTRACE
    importing
      !PATH type STRING
    returning
      value(RT_TABLE) type ref to ZCL_EEZZ_TABLE .
  methods GET_INSTANCES
    importing
      !PATH type STRING
    returning
      value(RT_TABLE) type ref to ZCL_EEZZ_TABLE .
  methods GET_LOGFILES
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
  methods GET_VERSION_INFO
    importing
      !PATH type STRING
    returning
      value(RT_TABLE) type ref to ZCL_EEZZ_TABLE .
  methods CONSTRUCTOR
    importing
      !IV_TABLE type ref to DATA optional
      !TABLE_NAME type STRING optional .

  methods ZIF_EEZZ_TABLE~DO_SELECT
    redefinition .
  methods ZIF_EEZZ_TABLE~ON_DOWNLOAD
    redefinition .
  methods ZIF_EEZZ_TABLE~PREPARE_DOWNLOAD
    redefinition .
  protected section.
  private section.

    data m_snapshot_name type string .
    data m_transferred type i .
    data m_snapshot_tbl type ref to data .
    data m_snapshot_zip type ref to cl_abap_string_x_writer .
    data m_snapshot_jsn type ref to zcl_eezz_json .

    methods parse_xml
      importing
        !iv_xmlstream      type string
      returning
        value(rv_document) type ref to if_ixml_document .
ENDCLASS.



CLASS ZCL_SAPCONTROL IMPLEMENTATION.


  method constructor.
    super->constructor( iv_table = iv_table table_name = table_name ).

    if iv_table is initial.
      m_selected     = |{ 1 }|.
      m_snapshot_jsn = new zcl_eezz_json( ).
      data x_system_name type string.
      data(rc) = cl_spfl_profile_parameter=>get_value( exporting name = 'SAPSYSTEMNAME' importing value = x_system_name  ).
      m_snapshot_jsn->join( iv_key = |Systems/{ x_system_name }| iv_type = 'object' iv_create = abap_true iv_object = new zcl_eezz_table( ) ).
    endif.
  endmethod.


  method get_devtrace.
    types: begin of tstr_line,
             inx  type i,
             line type string,
           end of tstr_line.

    types ttbl_table  type table of tstr_line with key inx initial size 0.
    data  xtbl_node   type ref to ttbl_table.

    data x_relpath    type string.
    data x_filename   type string.
    data x_tracepath  type string.
    data x_tracedir   type ref to ztty_eezz_json.
    data x_sapcontrol type ref to co_sapcontrol_factory.

    try.
        " Find the controlling structures for the given path
        data(x_sys_root)  = m_snapshot_jsn->get( iv_path = |Systems| ).
        data(x_sys_json)  = new zcl_eezz_json( it_json = x_sys_root ).

        x_sys_root        = x_sys_json->get( iv_path = path iv_match = 1 ).
        data(x_sys_table) = x_sys_json->m_parent-c_object.

        data(x_instance)  = x_sys_json->get( iv_path = path iv_match = 2 ).
        data(x_inst_path) = x_sys_json->m_path.

        if path cs 'disp+work'.
          x_filename = 'dev_disp'.
        elseif path cs 'jstart'.
          x_filename = 'dev_jstart'.
        else.
          return.
        endif.

        data(x_dictionary) = cast zcl_eezz_table( x_sys_table )->get_dictionary( ).
        if line_exists( x_dictionary->*[ c_key = 'sapcontrol' ] ).
          x_sapcontrol ?= x_dictionary->*[ c_key = 'sapcontrol' ]-c_object.
        else.
          x_tracedir    = x_sys_json->get( iv_path = |{ path }/ReadLogFile/work| iv_match = 5 ).
          x_tracepath   = x_sys_json->m_path.
        endif.
      catch cx_root into data(x_exception).
        zcl_eezz_message=>add( iv_status = 500 iv_key = 'GetDevTrace' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.

    try.
        xtbl_node = new ttbl_table( ).

        if x_sapcontrol is bound.
          return.
        endif.

        if x_tracedir is not initial.
          data(x_trace)     = x_tracedir->*[ c_key = |{ x_filename }.xml| ].
          data(x_element)   = me->parse_xml( x_trace-c_value ).
          data(x_processor) = new cl_xslt_processor( ).
          x_processor->set_source_node( x_element ).
          x_processor->set_expression( |//fields/item| ).
          x_processor->run( progname = space ).

          data(x_nodelist)    = x_processor->get_nodes( ).
          data(x_iterator)    = x_nodelist->create_iterator( ).

          do 1000 times.
            data(x_next) = x_iterator->get_next( ).
            if x_next is not bound.
              exit.
            endif.

            data(x_value) = x_next->get_value( ).
            append value #( inx = sy-index line = x_value ) to xtbl_node->*.
          enddo.

        endif.
      catch cx_sy_itab_line_not_found.
        return.
    endtry.

    try.
        data(x_eezz_table) = new zcl_eezz_table( iv_table =  xtbl_node ).
        x_dictionary       = x_eezz_table->get_dictionary( ).
        modify table x_dictionary->* from value #( c_key = 'table_key'          c_value  = |{ x_inst_path }/work/{ x_filename }| ).
        modify table x_tracedir->*   from value #( c_key = |{ x_filename }.xml| c_object = x_eezz_table ) transporting c_object.
        rt_table = cast #( x_eezz_table ).
      catch cx_root into x_exception.
        zcl_eezz_message=>add( iv_status = 500 iv_key = 'GetDevTrace' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.
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
        data(x_systems)      = m_snapshot_jsn->get( iv_path = |Systems| ).
        data(x_jsn_instance) = new zcl_eezz_json( it_json = x_systems ).

        x_instance     = x_jsn_instance->get( iv_path = path iv_match = 1 ).
        data(x_path)   = x_jsn_instance->m_path.
        x_table        = new tty_table( ).
        x_jsn_instance = new zcl_eezz_json( it_json = x_instance ).

        loop at x_instance->* into data(x_wa).
          data(x_statval) = x_jsn_instance->get_status( iv_path = |{ x_wa-c_key }/MonitoringElements| ).
          if x_statval is not initial.
            append value #( name = x_wa-c_key dispstatus = x_statval instancetype = |D0| ) to x_table->*.
          endif.
        endloop.

        data(x_eezz_table)   = new zcl_eezz_table( iv_table = x_table ).
        data(x_dictionary)   = x_eezz_table->get_dictionary( ).

        modify table x_dictionary->* from value #( c_key = 'table_key' c_value = x_path ).
        rt_table             = x_eezz_table.

      catch cx_sy_itab_line_not_found.
    endtry.
  endmethod.


  method get_logfiles.
    types: begin of tstr_struct,
             filename type string,
             size     type i,
             modtime  type string,
             format   type string,
           end of tstr_struct.

    types ttbl_table  type table of tstr_struct with key filename initial size 0.
    data  xtbl_node   type ref to ttbl_table.
    data  x_soap_data type zstr_eezz_json.
    data  xtbl_wa     type tstr_struct.
    data x_sapcontrol type ref to co_sapcontrol_factory.
    data x_request    type ssilist_log_files_request.
    data x_response   type ssilist_log_files_response.

    try.
        " Find the controlling structures for the given path
        data(x_sys_root)   = m_snapshot_jsn->get( iv_path = |Systems| ).
        data(x_sys_json)   = new zcl_eezz_json( it_json = x_sys_root ).

        x_sys_root         = x_sys_json->get( iv_path = path iv_match = 1 ).
        data(x_sys_table)  = x_sys_json->m_parent-c_object.

        data(x_instance)   = x_sys_json->get( iv_path = path iv_match = 2 ).
        data(x_path)       = x_sys_json->m_path.

        if x_sys_table is not bound.
          return.
        endif.

        data(x_dictionary) = cast zcl_eezz_table( x_sys_table )->get_dictionary( ).
        if line_exists( x_dictionary->*[ c_key = 'sapcontrol' ] ).
          x_sapcontrol ?= x_dictionary->*[ c_key = 'sapcontrol' ]-c_object.
        else.
          x_soap_data   = x_instance->*[ c_key = |ListLogFiles.xml| ].
        endif.

      catch cx_root into data(x_exception).
        zcl_eezz_message=>add( iv_status = 500 iv_key = 'GetLogFiles' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.

    try.
        xtbl_node = new ttbl_table( ).
        if x_sapcontrol is bound.
          x_sapcontrol->mo_public_proxy->list_log_files( exporting input = x_request importing output = x_response ).
          loop at x_response-file-item into data(x_wa).
            xtbl_wa-filename = x_wa-filename.
            xtbl_wa-size     = x_wa-size.
            xtbl_wa-modtime  = x_wa-modtime.
            xtbl_wa-format   = x_wa-format.
            append xtbl_wa to xtbl_node->*.
          endloop.
        endif.

        if x_soap_data is not initial.
          data(x_element)   = me->parse_xml( x_soap_data-c_value ).

          data(x_processor) = new cl_xslt_processor( ).
          x_processor->set_source_node( x_element ).
          x_processor->set_expression( |//item| ).
          x_processor->run( progname = space ).
          data(x_nodelist)  = x_processor->get_nodes( ).
          data(x_iterator)  = x_nodelist->create_iterator( ).

          do 1000 times.
            data(x_next) = x_iterator->get_next( ).
            if x_next is not bound.
              exit.
            endif.

            data(x_filter) = x_next->create_filter_node_type( if_ixml_node=>co_node_element ).
            data(x_sub_it) = x_next->create_iterator_filtered( x_filter ).
            clear xtbl_wa.

            do 1000 times.
              data(x_sub_next) = x_sub_it->get_next( ).
              if x_sub_next is not bound.
                exit.
              endif.

              data(x_name)  = x_sub_next->get_name( ).
              data(x_value) = x_sub_next->get_value( ).

              case x_name.
                when 'filename'. xtbl_wa-filename     = replace( val = x_value sub = '\' with = '/' occ = 0 ).
                when 'size'.     xtbl_wa-size         = x_value.
                when 'modtime'.  xtbl_wa-modtime      = x_value.
                when 'format'.   xtbl_wa-format       = x_value.
              endcase.
            enddo.
            append xtbl_wa to xtbl_node->*.

          enddo.

        endif.

      catch cx_root into x_exception.
        zcl_eezz_message=>add( iv_status = 500 iv_key = 'GetLogFiles' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.


    try.
        data(x_eezz_table) = new zcl_eezz_table( iv_table = xtbl_node ).
        x_dictionary       = x_eezz_table->get_dictionary( ).
        modify table x_dictionary->* from value #( c_key = 'table_path' c_value = x_path ).

        " xtbl_node->c_object   = cast #( x_eezz_table ).
        rt_table             = cast #( x_eezz_table ).
      catch cx_root into x_exception.
        zcl_eezz_message=>add( iv_status = 500 iv_key = 'GetLogFiles' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.

  endmethod.


  method get_monitor_elements.
    types: begin of tstr_line,
             _eezz_row_cell_ type string,
             dispstatus      type x length 1,
             name            type string,
           end of tstr_line.

    types: tty_table type table of tstr_line with key name initial size 0.

    data x_tbl_elements type ref to tty_table.
    data x_elements     type ref to ztty_eezz_json.

    if m_snapshot_jsn is not bound.
      return.
    endif.

    x_elements      = m_snapshot_jsn->get( iv_path = |{ path }/MonitoringElements| iv_match = 3 ).
    data(x_path)    = m_snapshot_jsn->m_path.
    data(x_statstr) = m_snapshot_jsn->m_status.
    "---- data(x_proclist) = get_process_list( path = path ).

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

    data(x_eezz_table) = new zcl_eezz_table( iv_table = x_tbl_elements ).
    " x_eezz_table->set_status( iv_key = |dispstatus| iv_value = x_statstr ).

    data(x_dictionary) = x_eezz_table->get_dictionary( ).
    modify table x_dictionary->* from value #( c_key = |table_key| c_value = x_path ).
    "-----x_elements->*-c_object = x_eezz_table.

    rt_table = cast #( x_eezz_table ).
  endmethod.


  method get_process_list.
    types: begin of tstr_line,
             name        type string,
             fullname    type string,
             description type string,
             dispstatus  type string,
             textstatus  type string,
             starttime   type string,
             elapsedtime type string,
             pid         type string,
           end of tstr_line.

    types ttbl_table    type table of tstr_line with key name initial size 0.
    data  xtbl_node     type ref to ttbl_table.

    data x_inst_status  type xstring.
    data x_status       type x length 1.
    data x_instance     type ref to ztty_eezz_json.
    data x_proclist     type zstr_eezz_json.
    data x_wa_process   type tstr_line.
    data x_sapcontrol   type ref to co_sapcontrol_factory.
    data x_request      type ssiget_process_list_request.
    data x_response     type ssiget_process_list_response.

    try.
        " Find the controlling structures for the given path
        data(x_sys_root)   = m_snapshot_jsn->get( iv_path = |Systems| ).
        data(x_sys_json)   = new zcl_eezz_json( it_json = x_sys_root ).

        x_sys_root         = x_sys_json->get( iv_path = path iv_match = 1 ).
        data(x_sys_table)  = x_sys_json->m_parent-c_object.

        x_instance         = x_sys_json->get( iv_path = path iv_match = 2 ).
        data(x_proc_root)  = x_sys_json->m_parent.
        data(x_path)       = x_sys_json->m_path.

        if x_sys_table is not bound.
          return.
        endif.

        data(x_dictionary) = cast zcl_eezz_table( x_sys_table )->get_dictionary( ).
        if line_exists( x_dictionary->*[ c_key = 'sapcontrol' ] ).
          x_sapcontrol ?= x_dictionary->*[ c_key = 'sapcontrol' ]-c_object.
        else.
          x_proclist    = x_instance->*[ c_key = |GetProcessList.xml| ].
        endif.

      catch cx_root into data(x_exception).
        zcl_eezz_message=>add( iv_status = 500 iv_key = 'GetProcessList' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.

    try.
        " If sapcontrol is bound, a life update is requested
        if x_sapcontrol is bound.
          xtbl_node = new ttbl_table( ).
          x_sapcontrol->mo_public_proxy->get_process_list( exporting input = x_request importing output = x_response ).

          loop at x_response-process-item into data(x_item).
            case x_item-dispstatus.
              when 'SAPControl-GRAY'.   x_status = nmax( val1 = x_status val2 = 1 ).
              when 'SAPControl-GREEN'.  x_status = nmax( val1 = x_status val2 = 2 ).
              when 'SAPControl-YELLOW'. x_status = nmax( val1 = x_status val2 = 4 ).
              when 'SAPControl-RED'.    x_status = nmax( val1 = x_status val2 = 8 ).
            endcase.

            append value #(
              fullname    = x_item-name
              description = x_item-description
              dispstatus  = x_item-dispstatus
              textstatus  = x_item-textstatus
              starttime   = x_item-starttime
              elapsedtime = x_item-elapsedtime
              pid         = x_item-pid ) to xtbl_node->*.
          endloop.
        endif.
      catch cx_root into x_exception.
        zcl_eezz_message=>add( iv_status = 500 iv_key = 'GetProcessList' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.

    try.
        " If x_proclist is bound, a snapshot contains data
        if x_proclist is not initial.
          xtbl_node       = new ttbl_table( ).
          data(x_element) = me->parse_xml( x_proclist-c_value ).

          data(x_processor) = new cl_xslt_processor( ).
          x_processor->set_source_node( x_element ).
          x_processor->set_expression( |//item| ).
          x_processor->run( progname = space ).
          data(x_nodelist) = x_processor->get_nodes( ).
          data(x_iterator) = x_nodelist->create_iterator( ).

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
                when 'name'.
                  split x_value at '.' into table data(x_part).
                  x_wa_process-name        = x_part[ 1 ].
                  x_wa_process-fullname    = x_value.
                when 'description'. x_wa_process-description = x_value.
                when 'textstatus'.  x_wa_process-textstatus  = x_value.
                when 'starttime'.   x_wa_process-starttime   = x_value.
                when 'elapsedtime'. x_wa_process-elapsedtime = x_value.
                when 'pid'.         x_wa_process-pid         = x_value.
                when 'dispstatus'.  x_wa_process-dispstatus  = x_value.
                  case x_value.
                    when 'SAPControl-GRAY'.   x_status = nmax( val1 = x_status val2 = 1 ).
                    when 'SAPControl-GREEN'.  x_status = nmax( val1 = x_status val2 = 2 ).
                    when 'SAPControl-YELLOW'. x_status = nmax( val1 = x_status val2 = 4 ).
                    when 'SAPControl-RED'.    x_status = nmax( val1 = x_status val2 = 8 ).
                  endcase.
              endcase.
            enddo.
            append x_wa_process to xtbl_node->*.
          enddo.
        endif.
      catch cx_root into x_exception.
        zcl_eezz_message=>add( iv_status = 500 iv_key = 'GetProcessList' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.

    try.
        " Finally store accumulated state and create table for output
        if lines( xtbl_node->* ) > 0.
          " Convert x_status for the status management
          data x_statval type xstring.
          data x_statstr type string.
          x_statval = x_status.
          x_statstr = x_statval.

          data(x_eezz_table) = new zcl_eezz_table( iv_table = xtbl_node ).
          x_dictionary       = x_eezz_table->get_dictionary( ).
          modify table x_dictionary->* from value #( c_key = 'table_key' c_value = x_path ).

          x_sys_json->join( iv_key = |{ x_path }/MonitoringElements/| iv_status = |{ x_status }| iv_create = abap_true ).
          modify table x_instance->* from value #( c_key = |GetProcessList.xml| c_object = x_eezz_table ) transporting c_object.
          rt_table = cast #( x_eezz_table ).
        endif.
      catch cx_root into x_exception.
        zcl_eezz_message=>add( iv_status = 500 iv_key = 'GetProcessList' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.

  endmethod.


  method get_systems.
    types: begin of tstr_line,
             dispstatus type string,
             name       type string,
             filename   type string,
           end of tstr_line.

    types ttbl_table type table of tstr_line with key name initial size 0.
    data  xtbl_node  type ref to ttbl_table.

    data x_selected    type string.
    data x_content     type xstring.
    data x_ref_x       type ref to xstring.
    data x_filedata    type string.

    try.
        xtbl_node        = new ttbl_table( ).
        data(x_systems)  = m_snapshot_jsn->get( ).
        data(x_sys_root) = m_snapshot_jsn->get( iv_path = |Systems| ).
        data x_statval type x length 1.
        data x_statcal type x length 1.

        data(x_tmp_json)   = new zcl_eezz_json( it_json = x_sys_root ).

        field-symbols <fs_wa> type zstr_eezz_json.
        loop at x_sys_root->* assigning <fs_wa>.
          clear x_statval.
          clear x_statcal.

          data(x_dictionary)  = cast zcl_eezz_table( <fs_wa>-c_object )->get_dictionary( ).
          data(x_system_list) = cast ztty_eezz_json( <fs_wa>-c_ref ).
          modify table x_dictionary->* from value #( c_key = 'table_key' c_value = <fs_wa>-c_key ).

          if x_system_list is not initial.
            loop at x_system_list->* into data(x_wa_instance).
              data(x_path) = |{ <fs_wa>-c_key }/{ x_wa_instance-c_key }|.

              data(x_process_list) = get_process_list( path = x_path ).
              x_statval            = x_tmp_json->get_status( iv_path = |{ x_path }/MonitoringElements/|  ).
              x_statcal            = x_statcal bit-or x_statval.
            endloop.
          else.
            x_statcal = '01'.
          endif.

          data(x_filename) = x_dictionary->*[ c_key = 'table_type' ]-c_value.
          insert value #( dispstatus = |{ x_statcal }| name = <fs_wa>-c_key filename = x_filename ) into table xtbl_node->*.
          if sy-tabix eq m_selected.
            x_selected = <fs_wa>-c_key.
          endif.
        endloop.

        rt_table = new zcl_sapcontrol( iv_table = xtbl_node ).
        modify table x_systems->* from value #( c_key = |Systems| c_object = rt_table ) transporting c_object.
        x_dictionary = rt_table->get_dictionary( ).
        modify table x_dictionary->* from value #( c_key = 'table_key' c_value = x_selected ).

      catch cx_root into data(x_exception).
        zcl_eezz_message=>add( iv_status = 500 iv_key = 'GetSystems' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.
  endmethod.


  method get_version_info.
    types: begin of tstr_line,
             filename    type string,
             versioninfo type string,
             time        type string,
           end of tstr_line.

    types ttbl_table   type table of tstr_line with key filename initial size 0.
    data  xtbl_node    type ref to ttbl_table.
    data  xtbl_line    type tstr_line.
    data  x_sapcontrol type ref to co_sapcontrol_factory.
    data  x_request    type ssiget_version_info_request.
    data  x_response   type ssiget_version_info_response.
    data  x_system     type ref to ztty_eezz_json.
    data  x_soap_data  type zstr_eezz_json.

    try.
        data(x_sys_root)   = m_snapshot_jsn->get( iv_path = |Systems| ).
        data(x_sys_json)   = new zcl_eezz_json( it_json = x_sys_root ).

        x_sys_root         = x_sys_json->get( iv_path = path iv_match = 1 ).
        data(x_sys_table)  = x_sys_json->m_parent-c_object.

        data(x_instance)   = x_sys_json->get( iv_path = path iv_match = 2 ).
        data(x_path)       = x_sys_json->m_path.

        data(x_dictionary) = cast zcl_eezz_table( x_sys_table )->get_dictionary( ).
        if line_exists( x_dictionary->*[ c_key = 'sapcontrol' ] ).
          x_sapcontrol ?= x_dictionary->*[ c_key = 'sapcontrol' ]-c_object.
        else.
          x_soap_data   = x_instance->*[ c_key = |GetVersionInfo.xml| ].
        endif.
      catch cx_root into data(x_exception).
        zcl_eezz_message=>add( iv_status = 500 iv_key = 'GetVersionInfo' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.

    try.
        xtbl_node = new ttbl_table( ).

        if x_sapcontrol is bound.
          x_sapcontrol->mo_public_proxy->get_version_info( exporting input = x_request importing output = x_response ).
          loop at x_response-version-item into data(x_wa).
            xtbl_line-filename     = x_wa-filename.
            xtbl_line-versioninfo  = x_wa-version_info.
            xtbl_line-time         = x_wa-time.
            append xtbl_line to xtbl_node->*.
          endloop.
        endif.

        if x_soap_data is not initial.
          data(x_element)   = me->parse_xml( x_soap_data-c_value ).
          data(x_processor) = new cl_xslt_processor( ).
          x_processor->set_source_node( x_element ).
          x_processor->set_expression( |//item| ).
          x_processor->run( progname = space ).
          data(x_nodelist)  = x_processor->get_nodes( ).
          data(x_iterator)  = x_nodelist->create_iterator( ).

          do 1000 times.
            data(x_next) = x_iterator->get_next( ).
            if x_next is not bound.
              exit.
            endif.

            data(x_filter) = x_next->create_filter_node_type( if_ixml_node=>co_node_element ).
            data(x_sub_it) = x_next->create_iterator_filtered( x_filter ).
            clear xtbl_line.

            do 100 times.
              data(x_sub_next) = x_sub_it->get_next( ).
              if x_sub_next is not bound.
                exit.
              endif.

              data(x_name)  = x_sub_next->get_name( ).
              data(x_value) = x_sub_next->get_value( ).

              case x_name.
                when 'Filename'.    xtbl_line-filename     = x_value.
                when 'VersionInfo'. xtbl_line-versioninfo  = x_value.
                when 'Time'.        xtbl_line-time         = x_value.
              endcase.
            enddo.
            append xtbl_line to xtbl_node->*.
          enddo.
        endif.
      catch cx_ai_system_fault cx_root into x_exception.
        zcl_eezz_message=>add( iv_status = 500 iv_key = 'GetVersionInfo' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.

    try.
        data(x_eezz_table) = new zcl_eezz_table( iv_table = xtbl_node ).
        x_dictionary = x_eezz_table->get_dictionary( ).
        modify table x_dictionary->* from value #( c_key = 'table_path' c_value = x_path ).
        modify table x_dictionary->* from value #( c_key = 'table_key'  c_value = x_path ).
        rt_table     = cast #( x_eezz_table ).
      catch cx_root.
        zcl_eezz_message=>add( iv_status = 500 iv_key = 'GetVersionInfo' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.

  endmethod.


  method login.
    data rc type i.
    data x_system       type ref to ztty_eezz_json.
    data x_status       type xstring.
    data x_inst_status  type xstring.
    data x_sapcontrol   type ref to co_sapcontrol_factory.
    data x_system_name  type spfl_parameter_value.
    data x_system_host  type spfl_parameter_value.
    data x_system_nr    type spfl_parameter_value.
    data xinp_inst_list type ssiget_system_instance_list_r1.
    data xout_inst_list type ssiget_system_instance_list_re.
    data x_hostentry    type ref to ztty_eezz_json.

    data(x_sys_root) = m_snapshot_jsn->get( iv_path = |Systems| ).
    data(x_sys_json) = new zcl_eezz_json( it_json = x_sys_root ).

    if path cp |localhost|.
      rc  = cl_spfl_profile_parameter=>get_value( exporting name = 'SAPSYSTEMNAME' importing value = x_system_name  ).
      rc  = cl_spfl_profile_parameter=>get_value( exporting name = 'SAPLOCALHOST'  importing value = x_system_host  ).
      rc  = cl_spfl_profile_parameter=>get_value( exporting name = 'SAPSYSTEM'     importing value = x_system_nr  ).

      x_system = x_sys_json->get( iv_path = x_system_name ).
    else.
      x_system = x_sys_json->get( iv_path = path iv_match = 1 ).
    endif.

    try.
        data(x_eezz_table) = x_sys_json->m_parent-c_object.
        if x_eezz_table is not bound.
          return.
        endif.
        data(x_dictionary) = cast zcl_eezz_table( x_eezz_table )->get_dictionary( ).

        if line_exists( x_dictionary->*[ c_key = 'sapcontrol' ] ).
          x_sapcontrol ?= x_dictionary->*[ c_key = 'sapcontrol' ]-c_object.
        else.
          create object x_sapcontrol
            exporting
              iv_host        = x_system_host
              iv_nr          = x_system_nr
              iv_user        = user
              iv_password    = password
              iv_protocol    = 'http'
              iv_sapinternal = ''.
          insert value #( c_key = 'sapcontrol' c_object = x_sapcontrol ) into table x_dictionary->*.
        endif.

        x_sapcontrol->mo_public_proxy->get_system_instance_list( exporting input = xinp_inst_list  importing output = xout_inst_list ).

        loop at xout_inst_list-instance-item into data(x_wainst).
          case x_wainst-dispstatus.
            when 'SAPControl-GRAY'.   x_status = nmax( val1 = x_status val2 = '01' ).
            when 'SAPControl-GREEN'.  x_status = nmax( val1 = x_status val2 = '02' ).
            when 'SAPControl-YELLOW'. x_status = nmax( val1 = x_status val2 = '04' ).
            when 'SAPControl-RED'.    x_status = nmax( val1 = x_status val2 = '08' ).
          endcase.

          data(x_path) = |{ x_system_name }/{ x_wainst-hostname } { x_wainst-instance_nr }|.
          x_sys_json->join( iv_key = |{ x_path }/MonitoringElements/| iv_status = |{ x_status }| iv_create = abap_true ).
        endloop.
      catch cx_root into data(x_exception).
        zcl_eezz_message=>add( iv_status = 500 iv_key = 'login' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.

  endmethod.


  method parse_xml.
    data(x_xml_cl)         = cl_ixml=>create( ).
    data(x_stream_factory) = x_xml_cl->create_stream_factory( ).
    data(x_in_stream)      = x_stream_factory->create_istream_string( iv_xmlstream ).
    data(x_document)       = x_xml_cl->create_document( ).
    data(x_xml_parser)     = x_xml_cl->create_parser( document = x_document stream_factory = x_stream_factory istream = x_in_stream ).

    if x_xml_parser->parse( ) = 0.
      rv_document = x_document.
    endif.
  endmethod.


  method SAVE_LOGFILES.
    types: begin of tstr_struct,
             filename type string,
             size     type i,
             modtime  type string,
             format   type string,
           end of tstr_struct.

    types ttbl_table  type table of tstr_struct with key filename initial size 0.
    data  xtbl_node   type ref to ttbl_table.
    data  x_soap_data type zstr_eezz_json.
    data  xtbl_wa     type tstr_struct.
    data x_sapcontrol type ref to co_sapcontrol_factory.
    data x_request    type ssilist_log_files_request.
    data x_response   type ssilist_log_files_response.

    try.
        " Find the controlling structures for the given path
        data(x_sys_root)   = m_snapshot_jsn->get( iv_path = |Systems| ).
        data(x_sys_json)   = new zcl_eezz_json( it_json = x_sys_root ).

        x_sys_root         = x_sys_json->get( iv_path = path iv_match = 1 ).
        data(x_sys_table)  = x_sys_json->m_parent-c_object.

        data(x_instance)   = x_sys_json->get( iv_path = path iv_match = 2 ).
        data(x_path)       = x_sys_json->m_path.

        if x_sys_table is not bound.
          return.
        endif.

        data(x_dictionary) = cast zcl_eezz_table( x_sys_table )->get_dictionary( ).
        if line_exists( x_dictionary->*[ c_key = 'sapcontrol' ] ).
          x_sapcontrol ?= x_dictionary->*[ c_key = 'sapcontrol' ]-c_object.
        else.
          x_soap_data   = x_instance->*[ c_key = |ListLogFiles.xml| ].
        endif.

      catch cx_root into data(x_exception).
        zcl_eezz_message=>add( iv_status = 500 iv_key = 'GetLogFiles' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.

    try.
        data xprogressa type f.
        data xpercent   type f.
        data xcurrent   type f.
        data xtbl_updt  type ztty_update.

        field-symbols <fs> type any table.

        xtbl_node = new ttbl_table( ).

        if x_sapcontrol is bound.
          x_sapcontrol->mo_public_proxy->list_log_files( exporting input = x_request importing output = x_response ).

          xprogressa = lines( x_response-file-item ).
          xpercent   = 100 / xprogressa.
          xcurrent   = xpercent.

          loop at x_response-file-item into data(x_wa).
            xcurrent = sy-tabix * xpercent.
          endloop.

        endif.

        if x_soap_data is not initial.
          data(x_eezz_table) = me->get_logfiles( path = path ).
          assign x_eezz_table->mt_table->* to <fs>.

          xprogressa = lines( <fs> ).
          xpercent   = 100 / xprogressa.
          xcurrent   = xpercent.

          loop at <fs> assigning field-symbol(<fs_wa>).
            xcurrent = sy-tabix * xpercent.

            xtbl_updt = value #(
              ( c_key =  |{ progress }.innerHTML|    c_value = |{ xcurrent }%| )
              ( c_key =  |{ progress }.style.width|  c_value = |{ xcurrent }%| )
            ).
            data(x_json_resp) = zcl_eezz_json=>gen_response( it_update = ref #( xtbl_updt ) ).
            message->set_text( x_json_resp ).
            manager->send( message ).
          endloop.
        endif.

      catch cx_root into x_exception.
        zcl_eezz_message=>add( iv_status = 500 iv_key = 'GetLogFiles' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.


    try.
        x_eezz_table  = new zcl_eezz_table( iv_table = xtbl_node ).
        x_dictionary  = x_eezz_table->get_dictionary( ).
        modify table x_dictionary->* from value #( c_key = 'table_path' c_value = x_path ).

        " xtbl_node->c_object   = cast #( x_eezz_table ).
        " rt_table             = cast #( x_eezz_table ).
      catch cx_root into x_exception.
        zcl_eezz_message=>add( iv_status = 500 iv_key = 'GetLogFiles' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.

  endmethod.


  method zif_eezz_table~do_select.
    me->m_selected = |{ index }|.

    FIELD-SYMBOLS <fs_table> type ANY TABLE.
    field-SYMBOLS <fs_line> type any.

    assign me->mt_table->* to <fs_table>.

    loop at <fs_table> ASSIGNING <fs_line>.
      if sy-tabix eq index.
         data(x_hash) = me->get_hash( iv_line = <fs_line> iv_clear = abap_true ).
         exit.
      endif.
    endloop.

    modify table mt_dictionary from value #( c_key = 'table_key' c_value = x_hash ).
    rt_eezz_table = me.
  endmethod.


  method zif_eezz_table~on_download.
    types: begin of tstr_line,
             c_inx  type string,
             c_data type xstring,
           end of tstr_line.
    types: tty_table type table of tstr_line with key c_inx initial size 0.

    data x_ref_tbl   type ref to tty_table .
    data x_transfer  type i.
    data x_content   type xstring.
    data x_filedata  type string.

    if m_snapshot_tbl is initial.
      m_snapshot_tbl = new tty_table( ).
      m_transferred  = 0.
    endif.

    x_ref_tbl  = cast #( m_snapshot_tbl ).
    rv_message = new zcl_eezz_message( ).

    try.
        if iv_message->get_message_type( ) = iv_message->co_message_type_text.
          data(x_json)       = new zcl_eezz_json( iv_json = iv_message->get_text( ) ).
          data(x_progress)   = x_json->get_value( |progress| ).
          data(x_filesize)   = x_json->get_value( |file/size| ).
          data(x_transfered) = x_json->get_value( |file/chunkSize| ).
          data(x_filename)   = x_json->get_value( |file/name| ).
          data(x_chunksize)  = x_json->get_value( |chunkSize| ).
          data(x_sequence)   = x_json->get_value( |file/sequence| ).

          append value #( c_inx = x_sequence ) to x_ref_tbl->*.
          data(x_segments)  = lines( x_ref_tbl->* ).
          data(x_prog_seq)  = ( x_segments + 1 ) * ( x_chunksize / x_filesize ) * 100.
          x_prog_seq        = nmin( val1 = x_prog_seq  val2 = 100 ).

          if m_transferred = 0.
            m_transferred   = x_filesize.
            m_snapshot_name = x_filename.
          endif.
          m_transferred    = m_transferred - x_transfered.

          rv_message->add( iv_key = |{ x_progress }.innerHTML|   iv_value = |{ x_prog_seq }%| ).
          rv_message->add( iv_key = |{ x_progress }.style.width| iv_value = |{ x_prog_seq }%| ).

          if x_prog_seq = 100.
            rv_message->add( iv_status = 200 iv_key = 'OnLoad' iv_message = value #( c_msgtext = |File successfully loaded| c_msgcls = |zcl_eezz_sapctrl| c_msgnum = 0 ) ).
          endif.
          return.
        endif.
      catch cx_root into data(x_exception).
        rv_message->add( iv_status = 500 iv_key = 'OnLoad' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.

    try.
        x_segments  = lines( x_ref_tbl->* ).
        modify x_ref_tbl->* index x_segments from value #( c_data = iv_message->get_binary( ) ) transporting c_data.

        if m_transferred > 0.
          return.
        endif.

        sort x_ref_tbl->* by c_inx.
        loop at x_ref_tbl->* into data(x_wa).
          m_snapshot_zip->write( x_wa-c_data ).
        endloop.

        clear x_ref_tbl->*.
        clear m_snapshot_tbl.

        field-symbols <fswa> type zstr_eezz_json.

        " Replace snapshot entry and remove old entries
        data(x_sysroot) = m_snapshot_jsn->get( iv_path = |Systems| ).
        data x_entry type string.
        loop at x_sysroot->* assigning <fswa>.
          if <fswa>-c_object is bound.
            data(x_dictionary) = cast zcl_eezz_table( <fswa>-c_object )->get_dictionary( ).
            if x_dictionary->*[ c_key = 'table_type' ]-c_value cs 'snapshot'.
              x_entry = <fswa>-c_key.
              exit.
            endif.
          endif.
        endloop.

        if x_entry is not initial.
          delete x_sysroot->* where c_key = x_entry.
        endif.

        data(x_zip)      = new cl_abap_zip( ).
        data(x_res_str)  = m_snapshot_zip->get_result_string( ).
        x_zip->load( zip = x_res_str ).
      catch cx_root into x_exception.
        rv_message->add( iv_status = 500 iv_key = 'OnLoad' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.

    " store in shared memory segment
    try.
        data x_root type ref to zcl_eezz_drive.
        data(x_handle) = zcl_eezz_shm=>attach_for_write( ).
        create object x_root area handle x_handle.
        x_handle->set_root( x_root ).
        x_root->set( x_res_str ).
        x_handle->detach_commit( ).
      catch cx_root into x_exception.
        rv_message->add( iv_key = 'OnLoad' iv_exception = x_exception ).
    endtry.

    try.
        data(x_tmpjsn) = new zcl_eezz_json( ).

        loop at x_zip->files into data(x_file).
          x_zip->get( exporting index = sy-tabix importing content = x_content ).
          data(x_convert) = cl_abap_conv_in_ce=>create( input = x_content encoding = 'UTF-8' ignore_cerr = abap_true ).
          x_convert->read( importing data = x_filedata ).
          find first occurrence of |{ cl_abap_char_utilities=>cr_lf }{ cl_abap_char_utilities=>cr_lf }| in x_filedata results data(x_offset).
          x_filedata = x_filedata+x_offset-offset.
          x_filedata = x_filedata+x_offset-length.
          x_tmpjsn->join( iv_key = x_file-name iv_value = x_filedata iv_create = abap_true ).
        endloop.

        data(x_tmptbl)  = x_tmpjsn->get( ).

        loop at x_tmptbl->* into data(xwa_sys).
          data(x_sys_table) = new zcl_eezz_table( ).
          x_dictionary      = x_sys_table->get_dictionary( ).
          modify table x_dictionary->* from value #( c_key = 'table_type' c_value = m_snapshot_name ).
          m_snapshot_jsn->join( iv_key = |Systems/{ xwa_sys-c_key }| it_json = cast #( xwa_sys-c_ref ) iv_object = x_sys_table ).
        endloop.

        clear m_snapshot_zip.
        rv_message->set_status( 201 ).
      catch cx_apc_error.
        rv_message->add( iv_status = 500 iv_key = 'OnLoad' iv_exception = x_exception ).
        raise exception x_exception.
    endtry.
  endmethod.


  method zif_eezz_table~prepare_download.

    try.
        super->zif_eezz_table~prepare_download( iv_message ).

        m_snapshot_zip = new cl_abap_string_x_writer( ).
        clear m_snapshot_tbl.

      catch cx_apc_error cx_root into data(x_exception).
        raise exception x_exception.
    endtry.

  endmethod.
ENDCLASS.
