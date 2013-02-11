%%% 8.1 A gen_server Database Server with transactions
-module(db_gen_server).
-author('skers@kth.se').

%% API
-export([start/0]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([lock/0, unlock/0, write/2, read/1, match/1, delete/1, stop/0]).
-behaviour(gen_server).

-define(DBBACKEND, db_ets).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ?DBBACKEND, []).

%% Makes it compatible with my_db, for the tests to pass.
start() ->
    {Answer, _Pid} = start_link(),
    Answer.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(DbBackend) ->
    DbRef = DbBackend:new(),
    {ok, DbRef}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({write, Key, Element}, _From, DbRef) ->
    {reply, ok, db_rec:write(Key, Element, DbRef)};

handle_call({read, Key}, _From, DbRef) ->
    {reply, db_rec:read(Key, DbRef), DbRef};

handle_call({match, Element}, _From, DbRef) ->
    {reply, db_rec:match(Element, DbRef), DbRef};

handle_call({delete, Key}, _From, DbRef) ->
    {reply, ok, db_rec:delete(Key, DbRef)}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, DbRef) ->
    {stop, normal, DbRef};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Client, helper functions
write(Key, Element) ->
    gen_server:call(?MODULE, {write, Key, Element}).

read(Key) ->
    gen_server:call(?MODULE, {read, Key}).

match(Element) ->
    gen_server:call(?MODULE, {match, Element}).

delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

stop() ->
    gen_server:cast(?MODULE, stop).

%% Start saving db operations
lock() ->
    gen_server:call(?MODULE, {lock}).

unlock() ->
    gen_server:call(?MODULE, {unlock}).

