-module(advanced_table_demo).

-export([start/0]).
-export([init/1, update/2, render/1]).

-include_lib("cellium.hrl").

start() ->
   cellium:start(#{module => ?MODULE}).

init(_Ignored) ->
    {ok, #{}}.

update(Model, Msg) ->

 case Msg of
        {tb_event, key, _ ,{keydata, _ ,$q}} ->
            init:stop(),
            Model;
         _Else -> Model
 end.


render(_Model) ->
    Headers = ["File", "Size", "Date Modified", "Permissions"],
    Rows = [
        ["src/table.erl", "3.4 KB", "2025-11-01 10:30", "-rw-r--r--"],
        ["src/table_row.erl", "1.8 KB", "2025-11-01 10:32", "-rw-r--r--"],
        ["src/table_cell.erl", "1.2 KB", "2025-11-01 10:31", "-rw-r--r--"],
        ["examples/advanced_table_demo.erl", "1.5 KB", "2025-11-01 10:35", "-rw-r--r--"],
        ["Makefile", "512 B", "2025-10-28 14:00", "-rw-r--r--"]
    ],

    TableWidget = #{id => adv_table,
           type => widget,
           widget_type => table,
	   size => 20, 
           width => 84, height => 12,
           column_widths => [30, 10, 20, 15],
           headers => Headers,
           rows => Rows
    },

    #{type => container,
      id => main_container,
      orientation => vertical,
      children => [ TableWidget ] }.
