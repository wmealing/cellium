-module(terminal_wrapper).
-moduledoc """
  A wrapper for the terminal implementation that allows switching between
  the native terminal, dummy terminal, and mock terminal.
""".

-export([
    term_init/0,
    term_shutdown/0,
    term_width/0,
    term_height/0,
    term_clear/0,
    term_present/0,
    term_set_cell/5,
    term_print/5,
    get_cell/2,
    term_get_row/2,
    term_get_row/3,
    term_poll_event/0,
    term_set_input_mode/1,
    term_set_output_mode/1
]).

%% Internal management
-export([set_backend/1, get_backend/0]).

%% This module maintains its own state for the backend.
%% To keep it simple, we'll use a persistent term or just an environment variable.
%% For now, we'll use application:get_env/2.

term_init() ->
    (get_backend()):term_init().

term_shutdown() ->
    (get_backend()):term_shutdown().

term_width() ->
    (get_backend()):term_width().

term_height() ->
    (get_backend()):term_height().

term_clear() ->
    (get_backend()):term_clear().

term_present() ->
    (get_backend()):term_present().

term_set_cell(X, Y, Char, Fg, Bg) ->
    (get_backend()):term_set_cell(X, Y, Char, Fg, Bg).

term_print(X, Y, Fg, Bg, Str) ->
    (get_backend()):term_print(X, Y, Fg, Bg, Str).

get_cell(X, Y) ->
    (get_backend()):get_cell(X, Y).

term_get_row(Y, Width) ->
    (get_backend()):term_get_row(Y, Width).

term_get_row(X, Y, Length) ->
    (get_backend()):term_get_row(X, Y, Length).

term_poll_event() ->
    (get_backend()):term_poll_event().

term_set_input_mode(Mode) ->
    (get_backend()):term_set_input_mode(Mode).

term_set_output_mode(Mode) ->
    (get_backend()):term_set_output_mode(Mode).

set_backend(Backend) ->
    application:set_env(cellium, terminal_backend, Backend).

get_backend() ->
    case application:get_env(cellium, terminal_backend) of
        {ok, Backend} -> Backend;
        undefined -> native_terminal
    end.
