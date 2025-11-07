-module(greedy_wrap).

-export([word_wrap/2]).

word_wrap(Text, Width) ->
    Lines = binary:split(Text, <<"\n">>, [global]),
    lists:flatmap(fun(Line) -> wrap_single_line(Line, Width) end, Lines).

wrap_single_line(Line, Width) ->
    Words = binary:split(Line, [<<" ">>], [global, trim]),
    FilteredWords = [W || W <- Words, W =/= <<>>],
    if
        FilteredWords == [] ->
            [<<>>]; % This was an empty line, preserve it.
        true ->
            lists:reverse(do_wrap(FilteredWords, Width, <<>>, []))
    end.

% The main recursive function for wrapping a single line's words
do_wrap([], _Width, CurrentLine, Lines) ->
    % Base case: No more words. Add the final line.
    [CurrentLine | Lines];

do_wrap([Word | Rest], Width, <<>>, Lines) ->
    % Case 1: Starting a new line. (No preceding space needed)
    do_wrap(Rest, Width, Word, Lines);

do_wrap([Word | Rest], Width, CurrentLine, Lines) ->
    % Case 2: Continuing an existing line
    WordSize = size(Word),
    CurrentSize = size(CurrentLine),

    % The total length if we add the word, including one space
    PotentialSize = CurrentSize + 1 + WordSize,

    if PotentialSize =< Width ->
        % Subcase A: Word fits. Append space and word.
        NewLine = <<CurrentLine/binary, " ", Word/binary>>,
        do_wrap(Rest, Width, NewLine, Lines);

    true ->
        % Subcase B: Word does not fit. Complete the current line
        % and start a new line with just the current Word.
        do_wrap(Rest, Width, Word, [CurrentLine | Lines])
    end.



