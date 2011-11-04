-module(ebot_example).

-export([  welcome/3
         , time/3
         , pastebin/3
         , status/3
        ]).


%% ---------------------------------------------------------------------
%% STATUS (of particular Jenkins job)
%% ---------------------------------------------------------------------
-define(JENKINS_HOST, "master-hudson.internal.machines").
-define(PROJECT, "KRED.staging").

status(Sock, Line, Match) ->
    Project = get_project(Line, Match),
    Msg = status_msg(Project),
    case who_and_channel(Line) of
        {Who, Channel} ->
            ebot:say(Sock, Msg, "#"++Channel, Who);
        _ ->
            error_logger:error_msg("~p(~p): status, Line = ~p~n",
                                  [?MODULE,?LINE,Line])
    end.

status_msg(Project) ->
    try
        LatestJob = latest_job(Project),
        case job_status(Project, LatestJob) of
            "false" -> 
                "no job is running, "++
                    last_job_ok_msg(Project);
            _ -> 
                estimated_finish_msg(Project, LatestJob)
        end
    catch
        throw:Emsg -> Emsg;
          _:_        -> "Error, failed to get job status"

    end.

last_job_ok_msg(Project) ->
    JobStatus = case is_last_job_ok(Project) of
                    true  -> "Ok!";
                    false -> "Broken!!"
                end,
    "last job was: "++JobStatus.

get_project(Line, {match,[_,{Start0,Len}]}) when Len > 0 ->
    string:substr(Line, Start0+1, Len);
get_project(_Line, _Match) ->
    ?PROJECT.

estimated_finish_msg(Project, LatestJob) ->
    JobStart = job_start(Project, LatestJob),
    Duration = job_duration(Project, second_latest_job(Project)),
    DurSec = trunc(Duration / 60000)*60,
    MinLeft = trunc((JobStart + DurSec - gnow()) / 60),
    assert_positive(MinLeft, "Error, got negative estimate"),
    "estimated finish in "++i2l(MinLeft)++ " minutes ("++
        gtostr(JobStart + DurSec)++") "++
        last_job_ok_msg(Project).

assert_positive(I,_) when I >= 0 -> true;
assert_positive(_,E)             -> throw(E).

job_start(Project, Job) ->
    Cmd = "curl -s \"http://"++?JENKINS_HOST++"/view/Klarna/job/"++
        Project++"/"++Job++"/api/xml\" | xpath -q -e /freeStyleBuild/id "
        "| sed -e 's/<[/]*id>//g' | sed -e 's/\([^_]*\)_\([0-9]*\)-\([0-9]*\)"
        "-\([0-9]\)/\1 \2:\3:\4/g'",
    %% Will return something like: 2011-11-02_16-42-40
    Ds = hd(string:tokens(os:cmd(Cmd),"\n")),
    [YYYY,MM,DD,Ho,Mi,Se] = string:tokens(Ds,"-_"),
    datetime2gsecs({{l2i(YYYY),
                     l2i(MM),
                     l2i(DD)},
                    {l2i(Ho),
                     l2i(Mi),
                     l2i(Se)}}).

latest_job(Project) ->
    Cmd = "curl -s \"http://"++?JENKINS_HOST++"/view/Klarna/job/"++
        Project++"/api/xml?wrapper=foo&xpath=/freeStyleProject/build/"
        "number\" | xpath -q -e '/foo/number[1]/text()'",
    hd(string:tokens(os:cmd(Cmd),"\n")).

second_latest_job(Project) ->
    Cmd = "curl -s \"http://"++?JENKINS_HOST++"/view/Klarna/job/"++
        Project++"/api/xml?wrapper=foo&xpath=/freeStyleProject/build/"
        "number\" | xpath -q -e '/foo/number[2]/text()'",
    hd(string:tokens(os:cmd(Cmd),"\n")).

job_duration(Project, Job) ->
    Cmd = "curl -s \"http://"++?JENKINS_HOST++"/view/Klarna/job/"++
        Project++"/"++Job++"/api/xml\" | xpath -q -e "
        "/freeStyleBuild/duration | sed -e 's/<[/]*duration>//g'",
    l2i(hd(string:tokens(os:cmd(Cmd),"\n"))).

job_status(Project, Job) ->
    Cmd = "curl -s \"http://"++?JENKINS_HOST++"/view/Klarna/job/"++
        Project++"/"++Job++"/api/xml?xpath=//building\""
        " | xpath -q -e '/building/text()'",
    hd(string:tokens(os:cmd(Cmd),"\n")).

is_last_job_ok(Project) ->
    Cmd = "curl -s \"http://"++?JENKINS_HOST++"/view/Klarna/job/"++
        Project++"/rssAll\" | xpath -q -e /feed/entry/title | "
        "sort -rn -k2 | head -1 | grep -e 'stable' -e 'back to normal'",
    case os:cmd(Cmd) of
        [] -> false;
        _  -> true
    end.

%% ---------------------------------------------------------------------
%% WELCOME
%%
%% ":tobbe!~tobbe@foo.bar.com PRIVMSG #staging :ebot: welcome"
%% ---------------------------------------------------------------------
welcome(Sock, Line, _Match) ->
    case who_and_channel(Line) of
        {Who, Channel} ->
            ebot:say(Sock, "Thank you", "#"++Channel, Who);
        _ ->
            error_logger:error_msg("~p(~p): welcome, Line = ~p~n",
                                  [?MODULE,?LINE,Line])
    end.

%% ---------------------------------------------------------------------
%% PASTEBIN
%%
%% ":tobbe!~tobbe@foo.bar.com PRIVMSG #staging :ebot: welcome"
%% ---------------------------------------------------------------------
pastebin(Sock, Line, _Match) ->
    case who_and_channel(Line) of
        {Who, Channel} ->
            ebot:say(Sock, "http://kgist.internal.machines", "#"++Channel, Who);
        _ ->
            error_logger:error_msg("~p(~p): pastebin, Line = ~p~n",
                                  [?MODULE,?LINE,Line])
    end.

%% ---------------------------------------------------------------------
%% TIME
%% ---------------------------------------------------------------------
time(Sock, Line, _Match) ->
    case who_and_channel(Line) of
        {Who, Channel} ->
            ebot:say(Sock, time2str(), "#"++Channel, Who);
        _ ->
            error_logger:error_msg("~p(~p): time, Line = ~p~n",
                                  [?MODULE,?LINE,Line])
    end.

time2str() ->
    {H,M,S} = time(),
    lists:concat([H,":",M,":",S]).


%% ---------------------------------------------------------------------
%% UTILS
%% ---------------------------------------------------------------------

who_and_channel(Line) ->
    case re:run(Line, ":(.*)!.*PRIVMSG #([^ ]*).*") of
        {match,[_,{Who0,WhoLen},{Chan0,ChanLen}]} ->
            Who  = string:substr(Line, Who0+1,  WhoLen),
            Chan = string:substr(Line, Chan0+1, ChanLen),
            {Who, Chan};
        _ ->
            false
    end.

datetime2gsecs({YMD, Time}) ->
    calendar:datetime_to_gregorian_seconds({YMD, Time}).

gtostr(Secs) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
	calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(
      io_lib:format(
        "~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
        [Year, Month, Day, Hour, Minute, Second])).

gnow() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).

l2i(L) when is_list(L) ->
    list_to_integer(L).

i2l(I) when is_integer(I) ->
    integer_to_list(I).
