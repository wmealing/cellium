-module(logging).

-export([setup/0]).

setup() ->
    %% Remove the default console handler to prevent logs from appearing in the TUI
    logger:remove_handler(default),

    logger:set_primary_config(level, debug),
    Config = #{config => #{file => "./logs/cellium-debug"}, level => debug},
    case logger:add_handler(to_file_handler, logger_std_h, Config) of
        ok -> ok;
        {error, {already_exist, _}} -> ok;
        Error -> Error
    end,
    logger:info("logging started.").
