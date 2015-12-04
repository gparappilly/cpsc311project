
-module(stats).
-export([parse/1, statistics/1, map_merge/1, unzip4/1, chapter_stats/1]).

% Input: Character Ch
% Output: true if Ch is a letter; Ch if Ch is a special character; false otherwise
isalpha(Ch) -> case lists:member(Ch, ",;\"!-.") of
				   true -> Ch;
				   false -> case lists:member(Ch, lists:seq(97, 97+25) ++ lists:seq(65, 65+25)) of
								true -> true;
								false -> false
							end
				   end.

% Extracts words and special characters.
parse(Text) -> parse_(Text, "", false, []).

parse_("", Word, Alpha, Result) -> Result;
parse_([Ch|Rest], Word, Alpha, Result) -> case isalpha(Ch) of
											 true -> parse_(Rest, [Ch|Word], true, Result);
											 false -> if Alpha -> parse_(Rest, "", false, [{word, lists:reverse(Word)}|Result]);
														 true -> parse_(Rest, "", false, Result) end;
                                             N -> if Alpha -> parse_(Rest, "", false, [{char, N}|[{word, lists:reverse(Word)}|Result]]);
                                                     true -> parse_(Rest, "", false, [{char, N}|Result]) end
                                         end.

% Computes basic statistics.
stats([], CountMap, WordLengthSum, WordCount, SentenceLength) -> {CountMap, WordLengthSum, WordCount, SentenceLength};
stats([T|Rest], CountMap, WordLengthSum, WordCount, SentenceLength) -> NewCountMap = maps:put(T, maps:get(T, CountMap, 0) + 1, CountMap),
                                                                       [CurrentLength|RestSL] = SentenceLength,
                                                                       case T of {word, Word} -> stats(Rest, NewCountMap, WordLengthSum + length(Word), WordCount + 1, [CurrentLength+1|RestSL]);
                                                                           {char, $.} -> stats(Rest, NewCountMap, WordLengthSum, WordCount, [0|SentenceLength]);
                                                                           _Else -> stats(Rest, NewCountMap, WordLengthSum, WordCount, SentenceLength) end.

mean(Xs) -> lists:foldr(fun (X, Acc) -> X + Acc end, 0, Xs) / length(Xs).
standard_deviation(Xs, XMean) -> math:sqrt(lists:foldr(fun (X, Acc) -> (X-XMean)*(X-XMean) + Acc end, 0, Xs) / length(Xs)).

% Counts the number of unique words (entries with key {word, _}).
unique_words(CountMap) -> lists:foldl(fun (X, Acc) -> case X of {word, _} -> Acc + 1;
                                                                _Else -> Acc end end, 0, maps:keys(CountMap)).

% OBSOLETE !!
% Computes all statistics.
% To be called with the result of parse().
statistics(List) -> {CountMap, WordLengthSum, WordCount, SentenceLength} = stats(List, maps:new(), 0, 0, [0]),
                    MeanSentenceLength = mean(SentenceLength),
                    {unique_words(CountMap) / WordCount, % type-token-ratio
                     WordLengthSum / WordCount, % mean word length
                     MeanSentenceLength, % mean sentence length
                     standard_deviation(SentenceLength, MeanSentenceLength), % sd of sentence length
                     0, % TODO: mean paragraph length
                     WordCount, % chapter length
                     maps:get({char, $,}, CountMap, 0) / WordCount, % comma density
                     maps:get({char, $;}, CountMap, 0) / WordCount, % semicolon density
                     maps:get({char, $\"}, CountMap, 0) / WordCount, % quotation mark density
                     maps:get({char, $!}, CountMap, 0) / WordCount, % exclamation mark density
                     maps:get({char, $-}, CountMap, 0) / WordCount, % hyphen density
                     maps:get({word, "and"}, CountMap, 0) / WordCount, % "and" density
                     maps:get({word, "but"}, CountMap, 0) / WordCount, % "but" density
                     maps:get({word, "however"}, CountMap, 0) / WordCount, % "however" density
                     maps:get({word, "if"}, CountMap, 0) / WordCount, % "if" density
                     maps:get({word, "that"}, CountMap, 0) / WordCount, % "that" density
                     maps:get({word, "more"}, CountMap, 0) / WordCount, % "more" density
                     maps:get({word, "must"}, CountMap, 0) / WordCount,
                     maps:get({word, "might"}, CountMap, 0) / WordCount,
                     maps:get({word, "this"}, CountMap, 0) / WordCount,
                     maps:get({word, "very"}, CountMap, 0) / WordCount}.

% Merge all maps, adding the values of common keys.
map_merge([]) -> maps:new();
map_merge([X|[]]) -> X;
map_merge([X|[Y|Rest]]) -> map_merge([maps:fold(fun (K, V, AccIn) -> maps:put(K, maps:get(K, AccIn, 0) + V, AccIn) end, X, Y)|Rest]).

% Parse text and compute stats.
stats_(Text) -> stats(lists:reverse(parse(Text)), maps:new(), 0, 0, []).

unzip4(List) -> lists:foldr(fun ({A, B, C, D}, {As, Bs, Cs, Ds}) -> {[A|As], [B|Bs], [C|Cs], [D|Ds]} end, {[],[],[],[]}, List).

% THE MAIN FUNCTION
% Calculate the statistics of a chapter.
% Paragraphs must be separated by \r\n\r\n.
chapter_stats(Text) -> Paragraphs = re:split(Text, "\r\n\r\n",[{return,list}]),
                       ParagraphStats = lists:map(fun stats_/1, Paragraphs),
                       %CountMaps = lists:map(fun (X) -> element(1, X) end, ParagraphStats),
                       %WordLengthSums = lists:map(fun (X) -> element(2, X) end, ParagraphStats),
                       %WordCounts = lists:map(fun (X) -> element(3, X) end, ParagraphStats),
                       %SentenceLengths = lists:map(fun (X) -> element(4, X) end, ParagraphStats), % contains 0's
                       {CountMaps, WordLengthSums, WordCounts, SentenceLengths} = unzip4(ParagraphStats),
                       CountMap = map_merge(CountMaps),
                       WordLengthSum = lists:sum(WordLengthSums),
                       WordCount = lists:sum(WordCounts),
                       SentenceLength = lists:append(SentenceLengths),
                       MeanSentenceLength = mean(SentenceLength),
                       {unique_words(CountMap) / WordCount, % type-token-ratio
                       WordLengthSum / WordCount, % mean word length
                       MeanSentenceLength, % mean sentence length
                       standard_deviation(SentenceLength, MeanSentenceLength), % sd of sentence length
                       mean(WordCounts), % mean paragraph length
                       WordCount, % chapter length
                       maps:get({char, $,}, CountMap, 0) / WordCount, % comma density
                       maps:get({char, $;}, CountMap, 0) / WordCount, % semicolon density
                       maps:get({char, $\"}, CountMap, 0) / WordCount, % quotation mark density
                       maps:get({char, $!}, CountMap, 0) / WordCount, % exclamation mark density
                       maps:get({char, $-}, CountMap, 0) / WordCount, % hyphen density
                       maps:get({word, "and"}, CountMap, 0) / WordCount, % "and" density
                       maps:get({word, "but"}, CountMap, 0) / WordCount, % "but" density
                       maps:get({word, "however"}, CountMap, 0) / WordCount, % "however" density
                       maps:get({word, "if"}, CountMap, 0) / WordCount, % "if" density
                       maps:get({word, "that"}, CountMap, 0) / WordCount, % "that" density
                       maps:get({word, "more"}, CountMap, 0) / WordCount, % "more" density
                       maps:get({word, "must"}, CountMap, 0) / WordCount,
                       maps:get({word, "might"}, CountMap, 0) / WordCount,
                       maps:get({word, "this"}, CountMap, 0) / WordCount,
                       maps:get({word, "very"}, CountMap, 0) / WordCount}.
