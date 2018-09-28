*&---------------------------------------------------------------------*
*& Report ZZDB
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZZDB.

START-OF-SELECTION.
   "data xitab type zzdemo.
   data(c_out) = cl_demo_output=>new( ).

   types: BEGIN OF itab_line,
     node_id(2)    type C,
     parent_id(2)  type c,
     type(1)       type c,
     ord           type i,
     amount        type i,
   END OF itab_line.

   types: BEGIN OF xxresstr,
     node_id(2)    type C,
     parent_id(2)  type c,
   END OF xxresstr.

   " types ttable  type standard TABLE OF itab_line.
   " type xxtable table of zzdbtbl.
   " data  ttable  type table of zzdbtbl.
   " types xxtable type standard TABLE OF zzdbtbl.
   data xxline  type table of zzdbtbl.
   data xxres   type table of zzdbtbl.
   data xhier   type table of hierarchy.

   xxline = value #(
   ( parent_id = 'A1'    node_id = 'B1'  type = 'b'  ord = 1  amount =  120 )
   ( parent_id = 'A1'    node_id = 'B2'  type = 'b'  ord = 2  amount =   90 )
   ( parent_id = 'B1'    node_id = 'C1'  type = 'c'  ord = 1  amount =   40 )
   ( parent_id = 'B1'    node_id = 'C2'  type = 'a'  ord = 2  amount =   60 )
   ( parent_id = 'B2'    node_id = 'C3'  type = 'b'  ord = 3  amount =   75 )
   ( parent_id = 'B2'    node_id = 'C4'  type = 'a'  ord = 4  amount =   30 )
   ( parent_id = 'C3'    node_id = 'D1'  type = 'b'  ord = 1  amount =   10 )
   ( parent_id = 'C3'    node_id = 'D2'  type = 'c'  ord = 2  amount =   25 )
   ( parent_id = 'C4'    node_id = 'D3'  type = 'a'  ord = 3  amount =   30 )
   ( parent_id = ''      node_id = 'A2'  type = 'b'  ord = 2  amount =   80 )
   ( parent_id = 'A2'    node_id = 'B3'  type = 'c'  ord = 3  amount =   45 )
   ( parent_id = 'A2'    node_id = 'C4'  type = 'a'  ord = 4  amount =   30 )

   ).
   commit work.

   loop at xxline into data(waline).
      insert into zzdbtbl values waline.
   endloop.

   data(x_sqlstm_stream) = new cl_abap_string_c_writer(  ).
   data(lr_connection) = cl_sql_connection=>get_connection( 'DEFAULT' ).
   data(lr_statement)  = lr_connection->create_statement( ).
   data(l_client)      = sy-mandt.
   "data xxres type hierarchy.

   " x_sqlstm_stream->write( | select node_id, parent_id from zzdbtbl;  | ).
   "
   x_sqlstm_stream->write( |select node_id, parent_id from HIERARCHY ( source ( select node_id, parent_id from zzdbtbl ) )  | ).
   "     | SELECT hierarchy_rank AS rank FROM HIERARCHY ( | &&
   "     | SOURCE ( SELECT node_id, parent_id, type FROM ZZDBTBL ORDER by ord ) | &&
   "     | cache force )  order by hierarchy_rank |
   "  ).

   field-symbols <fs_table> type any table.
   data xxresval type table of xxresstr.
   assign xxresval to <fs_table>.

try.
   " select node_id, parent_id from zzdbtbl into table @xxresval.

   data(x_sqlstm)  = x_sqlstm_stream->get_result_string( ).
   data(lr_result) = lr_statement->execute_query( statement = x_sqlstm ).
   lr_result->set_param_table( ref #( xxresval ) ).
   lr_result->next_package( ).

   lr_connection->close( ).
   c_out->display( xxresval ).
catch cx_root into data(xx_ex).
  data(xx_err) = xx_ex->get_text( ).
endtry.

   " select * from zzdbtbl into table @data(xresult).
   " c_out->display( xxres ).
   "insert into zzdemo values  itab.
