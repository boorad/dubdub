%%%----------------------------------------------------------------------
%%% File    : randoms.erl
%%% Author  : Brad Anderson <brad@sankatygroup.com>
%%% Purpose :
%%% Created : 16 Jan 2009 by Brad Anderson <brad@sankatygroup.com>
%%%----------------------------------------------------------------------

-module(randoms).

-export([getRandomId/0]).

%% @doc generates a random id
%% @spec getRandomId() -> list()
getRandomId() ->
    integer_to_list(crypto:rand_uniform(1, 65536 * 65536)).
