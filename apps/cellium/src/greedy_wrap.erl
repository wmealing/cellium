-module(greedy_wrap).

-export([word_wrap/2]).

word_wrap(Text, Width) ->
    Words = binary:split(Text, [<<" ">>], [global, trim]),
    % Filter out empty binaries that might result from extra spaces
    FilteredWords = [W || W <- Words, W =/= <<>>],
    lists:reverse(do_wrap(FilteredWords, Width, <<>>, [])).

% The main recursive function
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



