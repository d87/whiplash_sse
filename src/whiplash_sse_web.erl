%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for whiplash_sse.

-module(whiplash_sse_web).
-author("Mochi Media <dev@mochimedia.com>").


-export([start/1, stop/0, loop/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

%% External API

start(Options) ->

    

    % amqp_channel:call(Channel, #'queue.declare'{queue = <<"Q1">>}),

    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).



message_recv(Response) ->
    {ok, Connection} = amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel,
        #'exchange.declare'{exchange = <<"events">>, type = <<"fanout">>}),
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel,
        #'queue.declare'{exclusive = true}),
    amqp_channel:call(Channel,
        #'queue.bind'{exchange = <<"events">>, queue = Queue}),
    amqp_channel:subscribe(Channel,
        #'basic.consume'{queue = Queue, no_ack = true}, self()),

    message_recv_loop(Response).

message_recv_loop(Response) ->
    receive
        %% This is the first message received
        #'basic.consume_ok'{} ->
            % io:format("basic.consume_ok ~p ~n", [self()]),
            message_recv_loop(Response);

        %% This is received when the subscription is cancelled
        #'basic.cancel_ok'{} ->
            io:format("basic.cancel_ok"),
            ok;

        {#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
            io:format(" [x] Received ~p ~p ~n", [Body, self()]),
            case binary:split(Body, [<<":">>]) of
                [Data|[]] ->
                    Event = "message";
                [Event|Data] ->
                    ok
            end,
            io:format("event: ~p~ndata: ~p~n", [Event,Data]),
            Response:send([<<"event: ">>, Event, <<"\r\n">>,
                           <<"data: ">>, Data, <<"\r\n\r\n">>]),
            message_recv_loop(Response)
        % _Any ->
            % io:format("Rcv: ~p~n", [_Any])
    end.


loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "hello_world" ->
                        Req:respond({200, [{"Content-Type", "text/plain"}], 
                            "Hello world!\n"});
                    "stream" ->
                        Response = Req:start_raw_response({200, [
                                        {"Access-Control-Allow-Origin", "http://nevihta.d87:4000"},
                                        {"Access-Control-Allow-Credentials", "true"},
                                        {"Content-Type", "text/event-stream"}
                                        ]}),
                        message_recv(Response);
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
