-module(nemes_test).
-include_lib("eunit/include/eunit.hrl").
-export([test/0]).

test() ->
    gamma_one_half_test(),
    gamma_factorial_test(),
    gamma_factorial_test2(),
    {ok, "Tests passed"}.

close(V, EV, Margin) ->
    abs(V - EV) < Margin.
    
gamma_one_half_test() ->
    ?assert( close( math:pow(nemes:gamma(0.5), 2), math:pi(), 1.0E-10) ).

gamma_factorial_test() ->
    ?assertEqual( nemes:gamma(5), 24.0).

gamma_factorial_test2() ->
    ?assert( close( nemes:gamma(30.0), 8.84176199373963e+30, 1.0E20)).