-module(debug_layout).
-export([debug_broken_layout/0, print_layout/1]).

debug_broken_layout() ->
    LayoutData = (container:new(horizontal, container1))#{
        id => something,
        x => 0,
        y => 0,
        width => 80,
        height => 10,
        children => [
            (button:new(<<"HELLO">>, foo1))#{id => button1,
                                           size => 20},
            (button:new(<<"THERE">>, foo2))#{id => button2,
                                            expand => true}
        ]
    },
    
    io:format("Original layout data:~n"),
    print_layout(LayoutData),
    
    CalculatedLayout = layout_engine:calculate_layout(LayoutData),
    io:format("~nCalculated layout:~n"),
    print_layout(CalculatedLayout),
    
    Children = maps:get(children, CalculatedLayout),
    [Button1, Button2] = Children,
    
    io:format("~nButton1 details:~n"),
    print_button_info(Button1),
    
    io:format("~nButton2 details:~n"),
    print_button_info(Button2),
    
    % Calculate expected values
    ContainerWidth = maps:get(width, CalculatedLayout),
    Button1Size = maps:get(size, Button1, 0),
    Gap = 1,
    ExpectedButton2Width = ContainerWidth - Button1Size - Gap,
    ExpectedButton2EndX = maps:get(x, Button2, 0) + maps:get(width, Button2, 0),
    
    io:format("~nExpected calculations:~n"),
    io:format("Container width: ~p~n", [ContainerWidth]),
    io:format("Button1 size: ~p~n", [Button1Size]),
    io:format("Gap: ~p~n", [Gap]),
    io:format("Expected Button2 width: ~p~n", [ExpectedButton2Width]),
    io:format("Actual Button2 width: ~p~n", [maps:get(width, Button2, 0)]),
    io:format("Expected Button2 end X: ~p~n", [ExpectedButton2EndX]),
    
    CalculatedLayout.

print_layout(Layout) ->
    Type = maps:get(type, Layout, unknown),
    io:format("Type: ~p~n", [Type]),
    io:format("ID: ~p~n", [maps:get(id, Layout, no_id)]),
    io:format("X: ~p, Y: ~p~n", [maps:get(x, Layout, 0), maps:get(y, Layout, 0)]),
    io:format("Width: ~p, Height: ~p~n", [maps:get(width, Layout, 0), maps:get(height, Layout, 0)]),
    
    case maps:get(children, Layout, []) of
        [] -> ok;
        Children ->
            io:format("Children: ~p~n", [length(Children)])
    end.

print_button_info(Button) ->
    io:format("ID: ~p~n", [maps:get(id, Button, no_id)]),
    io:format("X: ~p, Y: ~p~n", [maps:get(x, Button, 0), maps:get(y, Button, 0)]),
    io:format("Width: ~p, Height: ~p~n", [maps:get(width, Button, 0), maps:get(height, Button, 0)]),
    io:format("Size: ~p~n", [maps:get(size, Button, no_size)]),
    io:format("Expand: ~p~n", [maps:get(expand, Button, no_expand)]).
