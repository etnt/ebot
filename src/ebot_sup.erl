
-module(ebot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Data), 
        {Name, {ebot_worker, start_link, []}, 
         permanent, 5000, worker, [ebot_worker]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% FIXME
    L = file:consult("/home/tobbe/Kreditor/git/ebot/priv/example.conf"),

    Workers = [?CHILD(proplists:get_value(name, Data), Data)
               || Data <- L],

    {ok, { {one_for_one, 5, 10}, Workers} }.
