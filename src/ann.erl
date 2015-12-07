%% @author Fabian Volz

-module(ann).

-export([myzip/2, testann/0, feed_forward/2, partitionList/2, compute_deltas_init/3, compute_deltas/3, train_network/3, train_repeatedly/4, repeat/2, repeat_each/2,training_session_parallelism/3]).

-record(network, {layers, layersize, weights}).

testann() -> #network{layers=3, layersize=[2,1,2], weights=[[1,1],[1,1]]}.

% Weights is a list of lists
feed_forward_({network, Layers, Layersize, Weights}, Input) -> feed_forward(#network{layers=Layers, layersize=Layersize, weights=lists:append(Weights)}, Input).
% Weights is a list
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


% Computes the delta values. The layers of the input network must be reversed.
compute_deltas_init(Weights, [Output|ORest], Target) -> Deltas = map(fun (X, Y) -> (X-Y)*X*(1-X) end, Output, Target),
                                                        compute_deltas([Deltas], Weights, ORest).

% Recursively computes the delta values. The layers of the input network must be reversed.
% Requires the delta values of the next layer.
% Called by compute_deltas_init.
compute_deltas(Deltas, [], []) -> Deltas;
compute_deltas([Deltas|DRest], [Weights|WRest], [Output|ORest]) -> ThisLayerSize = length(Weights),
                                                                   PreviousLayerSize = length(Deltas),
                                                                   AAWeights = partitionList(Weights, ThisLayerSize),
                                                                   NewDeltas = map(fun (O, NodeWeights) -> O*(1-O) * lists:foldl(fun ({X, Y}, Acc) -> Acc + X*Y end, 0, myzip(Deltas, NodeWeights)) end, Output, AAWeights),
                                                                   compute_deltas([NewDeltas|[Deltas|DRest]], WRest, ORest).

update_weights(Alpha, Output, Delta, Weights) -> map3(fun (Os, Ds, Ws) -> map3(fun (O, D, W) -> W - Alpha * D * O end, repeat(Os, length(Ds)), repeat_each(Ds, length(Os)), Ws) end, Output, lists:nthtail(1, Delta), Weights).

% Returns the network with updated weights.
train_network({network, Layers, Layersize, Weights}, X, Y) -> Output = feed_forward(#network{layers=Layers, layersize=Layersize, weights=lists:append(Weights)}, X),
                                                              Deltas = compute_deltas_init(lists:reverse(Weights), lists:reverse(Output), Y),
                                                              NewWeights = update_weights(0.01, Output, Deltas, Weights),
                                                              #network{layers=Layers, layersize=Layersize, weights=NewWeights}.

train_repeatedly(Network, X, Y, 0) -> {Network, feed_forward_(Network, X)};
train_repeatedly(Network, X, Y, N) -> train_repeatedly(train_network(Network, X, Y), X, Y, N-1).

training_session_parallelism(Xs, Ys, N) -> training_session_parallelism_(Xs, Ys, N, N).
training_session_parallelism_(Xs, Ys, N, 0) -> receive_results(Xs, Ys, N);
training_session_parallelism_(Xs, Ys, N, K) -> InputLayerSize = length(lists:nth(1, Xs)),
                                               OutputLayerSize = length(lists:nth(1, Ys)),
                                               HiddenLayerSize = round(1.5*InputLayerSize),
                                               Network = #network{layers=3, layersize=[InputLayerSize,HiddenLayerSize,OutputLayerSize], weights=[random_list(InputLayerSize*HiddenLayerSize), random_list(HiddenLayerSize*OutputLayerSize)]},
                                               spawn(ann, train_concurrently, [Network, Xs, Ys]),
                                               training_session_parallelism_(Xs, Ys, N, K-1).

receive_results(Xs, Ys, N) -> 0.

% ----------------
% Helper Functions
% ----------------


% N-fold concatenation of Xs
repeat(Xs, 0) -> [];
repeat(Xs, N) -> Xs ++ repeat(Xs, N-1).

% [x1,...,xK] -> [x1,...,x1,x2,...,x2,...,xK,...,xK]
repeat_each([], N) -> [];
repeat_each([X|Xs], N) -> repeat([X], N) ++ repeat_each(Xs, N).

% Partition the list into N smaller lists by assigning the elements cyclically.
% partitionList([1,2,3,4,5,6,7,8,9], 3) = [[1,4,7], [2,5,8], [3,6,9]]
partitionList(List, N) -> partitionList_(List, N, N).
partitionList_(List, N, 0) -> [];
partitionList_(List, N, K) -> [partListHelp(List, N, 0)|partitionList_(lists:nthtail(1, List), N, K-1)].
partListHelp([], N, M) -> [];
partListHelp([X|Xs], N, 0) -> [X|partListHelp(Xs, N, N-1)];
partListHelp([X|Xs], N, M) -> partListHelp(Xs, N, M-1).

% Like the regular map, but with two lists.
map(Function, [], Ys) -> [];
map(Function, Xs, []) -> [];
map(Function, [X|Xs], [Y|Ys]) -> [Function(X, Y)|map(Function,Xs,Ys)].

% Like the regular map, but with three lists.
map3(Function, [], Ys, Zs) -> [];
map3(Function, Xs, [], Zs) -> [];
map3(Function, Xs, Ys, []) -> [];
map3(Function, [X|Xs], [Y|Ys], [Z|Zs]) -> [Function(X, Y, Z)|map3(Function,Xs,Ys, Zs)].

% Like the regular zip, but the lists can have different lengths.
myzip([], Bs) -> [];
myzip(As, []) -> [];
myzip([A|As], [B|Bs]) -> [{A, B}|myzip(As,Bs)].

random_list(0) -> [];
random_list(N) -> [random:uniform()|random_list(N-1)].

sigmoid(X) -> 1 / (1 + math:exp(-X)).
