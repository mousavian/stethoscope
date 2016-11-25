%%%-------------------------------------------------------------------
%% @doc sniffer top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sniffer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    UDP_Server = #{
        id => udp_listener,
        start => {sniffer_listener, start, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [sniffer]
    },

    Children = [UDP_Server],

    RestartStrategy = {one_for_one, 10, 10},

    {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
