-ifndef(TERMINAL).
-define(TERMINAL, native_terminal).
-endif. 

%%/* Colors (numeric) and attributes (bitwise) (`term_cell.fg`, `term_cell.bg`) */
-define(TERM_DEFAULT,16#0000).
-define(TERM_BLACK,  16#0001).
-define(TERM_RED,    16#0002).
-define(TERM_GREEN,  16#0003).
-define(TERM_YELLOW, 16#0004).
-define(TERM_BLUE,   16#0005).
-define(TERM_MAGENTA,16#0006).
-define(TERM_CYAN,   16#0007).
-define(TERM_WHITE,  16#0008).

-define(TERM_OUTPUT_CURRENT,   0).
-define(TERM_OUTPUT_NORMAL,    1).
-define(TERM_OUTPUT_256,       2).
-define(TERM_OUTPUT_216,       3).
-define(TERM_OUTPUT_GRAYSCALE, 4).
-define(TERM_OUTPUT_TRUECOLOR, 5).

%% -define(TERM_BOLD      ,0x0100
%% -define(TERM_UNDERLINE ,0x0200
%% -define(TERM_REVERSE   ,0x0400
%% -define(TERM_ITALIC    ,0x0800
%% -define(TERM_BLINK     ,0x1000
%% -define(TERM_HI_BLACK  ,0x2000).
%% -define(TERM_BRIGHT,   ,16#4000).
%% -define(TERM_DIM       ,0x8000
%% -define(TERM_256_BLACK TERM_HI_BLACK // `TERM_256_BLACK` is deprecated

%% %% fixme, maybe i need to call to see the TERM_OPT_ATTR
-define(TERM_BOLD                ,16#01000000).
-define(TERM_UNDERLINE           ,16#02000000).
-define(TERM_REVERSE             ,16#04000000).
-define(TERM_ITALIC              ,16#08000000).
-define(TERM_BLINK               ,16#10000000).
-define(TERM_HI_BLACK            ,16#20000000).
-define(TERM_BRIGHT              ,16#40000000).
-define(TERM_DIM                 ,16#80000000).
%% -define(TERM_TRUECOLOR_BOLD      TERM_BOLD // `TERM_TRUECOLOR_*` is deprecated
%% -define(TERM_TRUECOLOR_UNDERLINE TERM_UNDERLINE
%% -define(TERM_TRUECOLOR_REVERSE   TERM_REVERSE
%% -define(TERM_TRUECOLOR_ITALIC    TERM_ITALIC
%% -define(TERM_TRUECOLOR_BLINK     TERM_BLINK
%% -define(TERM_TRUECOLOR_BLACK     TERM_HI_BLACK


%% -define(TERM_STRIKEOUT   ,0x0000000100000000
%% -define(TERM_UNDERLINE_2 ,0x0000000200000000
%% -define(TERM_OVERLINE    ,0x0000000400000000
%% -define(TERM_INVISIBLE   ,0x0000000800000000


-define(TERM_INPUT_CURRENT,    0).
-define(TERM_INPUT_ESC,        1).
-define(TERM_INPUT_ALT,        2).
-define(TERM_INPUT_MOUSE,      4).

-define(DEFAULT_BG_COLOR, black).
-define(DEFAULT_FG_COLOR, white).

-record(box, {
    top_left, top, top_divider, top_right,
    head_left, head_vertical, head_right,
    head_row_left, head_row_horizontal, head_row_cross, head_row_right,
    mid_left, mid_vertical, mid_right,
    row_left, row_horizontal, row_cross, row_right,
    foot_row_left, foot_row_horizontal, foot_row_cross, foot_row_right,
    foot_left, foot_vertical, foot_right,
    bottom_left, bottom, bottom_divider, bottom_right,
    is_ascii = false
}).

