%% @author Hirotsugu Asari
%% @doc Provides the Gamma function for real number R
%% @reference <a href="http://www.ebyte.it/library/downloads/2008_MTH_Nemes_GammaApproximationUpdate.pdf">New
%%      asymptotic expansion for the Gamma(x) function</a>
%% @copyright 2011 Hirotsugu Asari
%% Distributed under MIT license. See README.

-module(nemes).
-include_lib("eunit/include/eunit.hrl").
-export([gamma/1, test/0]).

-define (f, {
    1.0, % 0! = Gamma(1)
    1.0,
    2.0,
    6.0,
    24.0,
    120.0,
    720.0,
    5040.0,
    40320.0,
    362880.0,
    3628800.0, % 10!
    39916800.0,
    479001600.0,
    6227020800.0,
    87178291200.0,
    1307674368000.0,
    20922789888000.0,
    355687428096000.0,
    6402373705728000.0,
    121645100408832000.0,
    2432902008176640000.0, % 20!
    51090942171709440000.0,
    1124000727777607680000.0
}).

%% @doc returns Gamma(r) for a real number r.
%% @spec gamma(R::float()) -> {ok, float()} | {error, atom()}

-spec(gamma(R::float()) -> {ok, float()} | {error, atom()}).

gamma(X) ->
    Len = tuple_size(?f),
    if 
        % for integers up to 23, look up factorials
        ((X > 0) and (X == trunc(X)) and (X =< Len)) ->
            element(X, ?f);
        % otherwise, use logarithmic version of the approximation formula
        % to get the gamma-function value
        true ->
            sign(X) * math:exp(log_gamma(X))
    end.

%% helper functions
sign(R) ->
    I = trunc(R),
    if
        ((I rem 2 == 0) and (R == I) and (R < 0)) -> -1;
        true -> 1
    end.

log_gamma(R) ->
    Len = tuple_size(?f),
    N = {
        1.0,
        0,
        0.08333333333333333333333333333333333,
        0,
        0.00069444444444444444444444444444444,
        0,
        0.00065861992945326278659611992945326,
        0,
       -0.00053287817827748383303938859494415,
        0,
        0.00079278588700608376534302460228386,
        0,
       -0.00184758189322033028400606295961969,
        0,
        0.00625067824784941846328836824623616,
        0,
       -0.02901710246301150993444701506844402,
        0,
        0.17718457242491308890302832366796470,
        0,
       -1.37747681703993534399676348903067470
    },
    
    I = trunc(R),

    if
        ((I == R) and (I > 0) and (I =< Len)) ->
            math:log(element(I, ?f));
        % if R is too small, use identity to get good approximation
        (R < 10) ->
            Ints = lists:seq(0, trunc(abs(R))-I+10-1),
            RF   = lists:foldl( fun(X, Prod) -> Prod * (R+X) end, 1, Ints ),
            L    = log_gamma(R + trunc(abs(R))-I+10),
            L - math:log(abs(RF));
        is_integer(R) ->
            log_gamma(R+0.0);
        true ->
            Ints = lists:seq(0, tuple_size(N)-1),
            V    = lists:foldl( fun(X, Sum) -> Sum + element(X+1, N) * 1.0 / math:pow(R, X) end, 0, Ints ),
            R *(math:log(R) - 1 + math:log(V)) + (math:log(2.0) + math:log(math:pi()) - math:log(R)) / 2.0
    end.
