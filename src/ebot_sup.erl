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
    L = file:consult(config_file()),
    Workers = [?CHILD(proplists:get_value(workername, Data), Data)
               || Data <- L],
    {ok, { {one_for_one, 5, 10}, Workers} }.


%% First look in /etc then in our own priv dir.
config_file() ->
    case file:consult("/etc/ebot.conf") of
        L when is_list(L) -> L;
        _ ->
            [_Beam,"ebin"|RevTokPath] = 
                lists:reverse(string:tokens(code:which(?MODULE),"/")),
            PrivPath = string:join(
                         lists:reverse(["priv"|RevTokPath]),"/"),
            [_|_] = L = file:consult(PrivPath++"/ebot.conf"),
            L
    end.
