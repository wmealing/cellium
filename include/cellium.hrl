% -define(TERMBOX, termbox_dummy).
-define(TERMBOX, termbox_wrapper).

%%/* Colors (numeric) and attributes (bitwise) (`tb_cell.fg`, `tb_cell.bg`) */
-define(TB_DEFAULT,16#0000).
-define(TB_BLACK,  16#0001).
-define(TB_RED,    16#0002).
-define(TB_GREEN,  16#0003).
-define(TB_YELLOW, 16#0004).
-define(TB_BLUE,   16#0005).
-define(TB_MAGENTA,16#0006).
-define(TB_CYAN,   16#0007).
-define(TB_WHITE,  16#0008).

%%  0008ca://github.com/termbox/termbox2/blob/ffd159c2a6106dd5eef338a6702ad15d4d4aa809/termbox2.h#L261
%% -define(TB_BOLD      ,0x0100
%% -define(TB_UNDERLINE ,0x0200
%% -define(TB_REVERSE   ,0x0400
%% -define(TB_ITALIC    ,0x0800
%% -define(TB_BLINK     ,0x1000
%% -define(TB_HI_BLACK  ,0x2000
%% -define(TB_BRIGHT    ,0x4000
%% -define(TB_DIM       ,0x8000
%% -define(TB_256_BLACK TB_HI_BLACK // `TB_256_BLACK` is deprecated

%% %% fixme, maybe i need to call to see the TB_OPT_ATTR
-define(TB_BOLD                ,16#01000000).
-define(TB_UNDERLINE           ,16#02000000).
-define(TB_REVERSE             ,16#04000000).
-define(TB_ITALIC              ,16#08000000).
-define(TB_BLINK               ,16#10000000).
-define(TB_HI_BLACK            ,16#20000000).
-define(TB_BRIGHT              ,16#40000000).
-define(TB_DIM                 ,16#80000000).
%% -define(TB_TRUECOLOR_BOLD      TB_BOLD // `TB_TRUECOLOR_*` is deprecated
%% -define(TB_TRUECOLOR_UNDERLINE TB_UNDERLINE
%% -define(TB_TRUECOLOR_REVERSE   TB_REVERSE
%% -define(TB_TRUECOLOR_ITALIC    TB_ITALIC
%% -define(TB_TRUECOLOR_BLINK     TB_BLINK
%% -define(TB_TRUECOLOR_BLACK     TB_HI_BLACK



%% -define(TB_STRIKEOUT   ,0x0000000100000000
%% -define(TB_UNDERLINE_2 ,0x0000000200000000
%% -define(TB_OVERLINE    ,0x0000000400000000
%% -define(TB_INVISIBLE   ,0x0000000800000000

-define(DEFAULT_BG_COLOR, 16#0001).
-define(DEFAULT_FG_COLOR, 16#0008).
