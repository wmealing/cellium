-module(termbox_wrapper).
-moduledoc """
  A wrapper for the terminal implementation that allows switching between
  the native terminal, dummy terminal, and mock terminal.
""".

-export([
    tb_init/0,
    tb_shutdown/0,
    tb_width/0,
    tb_height/0,
    tb_clear/0,
    tb_present/0,
    tb_set_cell/5,
    tb_print/5,
    tb_poll_event/0,
    tb_set_input_mode/1,
    tb_set_output_mode/1
]).

%% Internal management
-export([set_backend/1, get_backend/0]).

%% %% Warning: This record state is unused
%% -record(state, {
%%     backend = native_terminal
%% }).

%% This module maintains its own state for the backend.
%% To keep it simple, we'll use a persistent term or just an environment variable.
%% For now, we'll use application:get_env/2.

tb_init() ->
    (get_backend()):tb_init().

tb_shutdown() ->
    (get_backend()):tb_shutdown().

tb_width() ->
    (get_backend()):tb_width().

tb_height() ->
    (get_backend()):tb_height().

tb_clear() ->
    (get_backend()):tb_clear().

tb_present() ->
    (get_backend()):tb_present().

tb_set_cell(X, Y, Char, Fg, Bg) ->
    (get_backend()):tb_set_cell(X, Y, Char, Fg, Bg).

tb_print(X, Y, Fg, Bg, Str) ->
    (get_backend()):tb_print(X, Y, Fg, Bg, Str).

tb_poll_event() ->
    (get_backend()):tb_poll_event().

tb_set_input_mode(Mode) ->
    (get_backend()):tb_set_input_mode(Mode).

tb_set_output_mode(Mode) ->
    (get_backend()):tb_set_output_mode(Mode).

set_backend(Backend) ->
    application:set_env(cellium, terminal_backend, Backend).

get_backend() ->
    case application:get_env(cellium, terminal_backend) of
        {ok, Backend} -> Backend;
        undefined -> native_terminal
    end.
