-module(ann).

-export([myzip/2, testann/0, feed_forward/2]).

-record(network, {layers, layersize, weights}).

testann() -> #network{layers=3, layersize=[2,1,2], weights=[6,7,8,9]}.

feed_forward({network, Layers, Layersize, Weights}, Input) -> if Layers == 1 -> [lists:map(fun sigmoid/1, Input)];
                                          true -> InputSize = length(Input),
                                                  Output = lists:map(fun sigmoid/1, Input),
                                                  NextLayerSize = lists:nth(2, Layersize),
                                                  NextInput = compute_next_input(Output, NextLayerSize, Weights),
                                                  NewWeights = lists:nthtail(length(Output) * NextLayerSize, Weights),
                                                  [Output|feed_forward(#network{layers = Layers-1, layersize = lists:nthtail(1, Layersize), weights=NewWeights}, NextInput)]
                                end.

compute_next_input(Output, 0, Weights) -> [];
compute_next_input(Output, NextLayerSize, Weights) -> NextInput = lists:foldl(fun ({Out, Wei}, Sum) -> Sum + Out * Wei end, 0, myzip(Output, Weights)),
                                                      [NextInput|compute_next_input(Output, NextLayerSize-1, lists:nthtail(length(Output), Weights))].

% Like the regular zip, but the lists can have different lengths.
myzip([], Bs) -> [];
myzip(As, []) -> [];
myzip([A|As], [B|Bs]) -> [{A, B}|myzip(As,Bs)].

sigmoid(X) -> 1 / (1 + math:exp(-X)).
