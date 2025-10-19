-module(logging).

-export([setup/0, teardown/0]).

setup() ->
    logger:remove_handler(default),
    logger:set_primary_config(level, debug),
    Config = #{config => #{file => "./logs/debug"}, level => debug},
    logger:add_handler(to_file_handler, logger_std_h, Config),
    logger:info("logging started.").


teardown() ->
    logger:remove_handler(to_file_handler).
