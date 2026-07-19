-module(main).
-export([x/0, fact/1, main/0]).

x() ->
    5.000000.

fact(N) ->
    (case ((N =:= 0.000000)) of
        true -> 1.000000;
        false -> (N * fact((N - 1.000000)))
    end).

main() ->
    x(),
    stdlib:println_num(fact(x())).
