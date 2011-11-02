%%% --------------------------------------------------------------------
%%% Created : 2 Nov 2011 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.   : A major rework of the manderlbot code, aiming for a lean
%%%           code base, easily included into any application.
%%% --------------------------------------------------------------------
-module(ebot_worker).

-export([start_link/1, init/3, loop/1]).

-import(proplists, [get_value/2]).

-record(state, {
          socket,
          data,
          buffer=[],
          logfun={error_logger,info_msg}
         }).


start_link(Data) ->
    proc_lib:start_link(?MODULE, init, [self(), [], Data]).

init(Parent, _Options, Data) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    Sock = sock_connect(Data),
    State = #state{socket = Sock,
                   data   = Data},
    _Motd = ebot_lib:log_in(Sock, Data),
    join_channels(Sock, Data),
    loop(State).

join_channels(Sock, Data) ->
    [ebot_lib:join(Sock, Channel)
     || Channel <- get_value(channels, Data)].

loop(State) ->
    receive
        Msg ->
            ?MODULE:loop(x(Msg, State))
    end.

x({tcp, _Socket, Data}, State) ->
    Buffer = State#state.buffer,
    List = Buffer ++ binary_to_list(Data),
    Rest = chomp(State, List),
    State#state{buffer=Rest};

x({tcp_closed = Reason, _Socket}, _State) ->
    exit(Reason);

x({tcp_error, _Socket, Reason}, _State) ->
    exit({tcp_error, Reason});

x({From, OurMsgs}, State) ->
    {Answer, NewState} = fixme:handle_msg(OurMsgs, State),
    From ! {From, Answer},
    NewState;

x(_What, State) ->
    State.


chomp(State, Data) ->
    log(State, "~p(~p): chomp got data: ~p~n", 
        [?MODULE,?LINE,Data]),
    Pos = string:str(Data, "\r\n"),
    case Pos of 
	0 -> Data; % not complete line
	_ ->
	    Line = string:substr(Data, 1, Pos-1),
	    Rest = string:substr(Data, Pos+2, 
                                 string:len(Data) - (Pos-1)),
            line(State, Line),
            chomp(State, Rest)
    end.

line(#state{socket=Sock} = State, "PING "++Rest) ->
    log(State, "~p(~p): ping: pong ~p~n",
        [?MODULE,?LINE,Rest]),
    ebot_lib:pong(Sock, Rest);

line(#state{socket=Sock, data=Data} = State, Line) ->
    log(State, "~p(~p): line: ~p~n",
        [?MODULE,?LINE,Line]),
    Self = self(),
    Ms = get_value(matchspecs, Data),
    lists:foreach(
      fun({Re,{Mod,Fun} = CallBack}) ->
              case re:run(Line, Re) of
                  {match,_} = Match ->
                      log(State, "~p(~p): line match: ~p~n",
                          [?MODULE,?LINE,CallBack]),
                      spawn(fun() ->
                                    Mod:Fun(Sock, Line, Match)
                            end);
                  _ ->
                      false
              end
      end, Ms).


%% No fuss, connect!
sock_connect(Data) ->
    {ok, Sock} =
        gen_tcp:connect(get_value(server, Data),
                        get_value(port, Data),
                        [binary,
                         {packet, 0},
                         {nodelay, true},
                         {keepalive, true},
                         {active, true},
                         {reuseaddr, true}]),
    Sock.

log(#state{logfun = {M,F}}, Fs, As) ->
    M:F(Fs, As).
