%% Erlang Test File for UAST-Grep
%% Tests: functions, modules, variables, control flow, error handling

-module(test).
-behaviour(gen_server).

%% API exports
-export([start_link/0, start_link/1, process/1, stop/0]).
-export([calculate_sum/2, transform/1, get_status_message/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Macros
-define(MAX_ITEMS, 100).
-define(DEFAULT_NAME, "UAST-Grep").
-define(SERVER, ?MODULE).

%% Type specifications
-type item_list() :: [term()].
-type status() :: ok | not_found | server_error.
-type result() :: {ok, term()} | {error, term()}.

%% Record definitions
-record(state, {
    name :: string(),
    count = 0 :: non_neg_integer(),
    cache = #{} :: map()
}).

-record(person, {
    name :: string(),
    age :: non_neg_integer(),
    email = undefined :: string() | undefined
}).

%% ============================================================================
%% API Functions
%% ============================================================================

%% @doc Start the server with default name
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(?DEFAULT_NAME).

%% @doc Start the server with a custom name
-spec start_link(Name :: string()) -> {ok, pid()} | {error, term()}.
start_link(Name) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Name], []).

%% @doc Process a list of items
-spec process(Items :: item_list()) -> result().
process(Items) ->
    gen_server:call(?SERVER, {process, Items}).

%% @doc Stop the server
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% ============================================================================
%% gen_server Callbacks
%% ============================================================================

init([Name]) ->
    log(Name, "Initializing processor"),
    {ok, #state{name = Name}}.

handle_call({process, Items}, _From, State) ->
    Results = process_items(Items, State),
    NewState = State#state{count = length(Results)},
    {reply, {ok, Results}, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    log(State#state.name, "Terminating"),
    ok.

%% ============================================================================
%% Internal Functions
%% ============================================================================

%% @doc Process items and return transformed results
-spec process_items(item_list(), #state{}) -> item_list().
process_items(Items, State) ->
    %% List comprehension
    Results = [transform(Item) || Item <- Items],

    %% For-each equivalent using lists:foreach
    lists:foreach(fun(Item) ->
        log(State#state.name, io_lib:format("Processing: ~p", [Item]))
    end, Items),

    %% Recursive processing with accumulator
    process_items_loop(Items, [], 0),

    Results.

%% @doc Recursive loop example
-spec process_items_loop(item_list(), item_list(), non_neg_integer()) -> item_list().
process_items_loop([], Acc, _Counter) ->
    lists:reverse(Acc);
process_items_loop([H | T], Acc, Counter) when Counter < ?MAX_ITEMS ->
    Transformed = transform(H),
    process_items_loop(T, [Transformed | Acc], Counter + 1);
process_items_loop(_, Acc, _Counter) ->
    lists:reverse(Acc).

%% @doc Transform a single item
-spec transform(term()) -> term().
transform(Item) when is_list(Item), is_integer(hd(Item)) ->
    [X * 2 || X <- Item];
transform(Item) when is_binary(Item) ->
    string:uppercase(binary_to_list(Item));
transform(Item) when is_integer(Item) ->
    Item * 2;
transform(Item) when is_float(Item) ->
    Item * 2.0;
transform(Item) ->
    Item.

%% @doc Get status message
-spec get_status_message(status()) -> string().
get_status_message(Status) ->
    %% Case expression
    case Status of
        ok -> "OK";
        not_found -> "Not Found";
        server_error -> "Server Error";
        _ -> "Unknown"
    end.

%% @doc Calculate sum of two numbers
-spec calculate_sum(number(), number()) -> number().
calculate_sum(A, B) ->
    A + B.

%% @doc Log a message
-spec log(string(), string()) -> ok.
log(Name, Message) ->
    io:format("[~s] ~s~n", [Name, Message]).

%% @doc Risky operation with error handling
-spec risky_operation(string()) -> result().
risky_operation(Filename) ->
    %% Try-catch expression
    try
        {ok, Content} = file:read_file(Filename),
        Length = byte_size(Content),
        log(?DEFAULT_NAME, io_lib:format("Read ~p bytes", [Length])),
        {ok, Content}
    catch
        error:enoent ->
            log(?DEFAULT_NAME, "File not found"),
            {error, file_not_found};
        error:Reason ->
            log(?DEFAULT_NAME, io_lib:format("Error: ~p", [Reason])),
            {error, Reason};
        _:_ ->
            {error, unknown}
    after
        log(?DEFAULT_NAME, "Operation complete")
    end.

%% @doc Pattern matching example
-spec describe(term()) -> string().
describe(Value) ->
    %% Guards and pattern matching
    if
        is_integer(Value), Value > 0 ->
            "Positive integer";
        is_integer(Value), Value < 0 ->
            "Negative integer";
        is_integer(Value) ->
            "Zero";
        is_list(Value) ->
            io_lib:format("List with ~p elements", [length(Value)]);
        is_tuple(Value) ->
            io_lib:format("Tuple with ~p elements", [tuple_size(Value)]);
        true ->
            "Unknown type"
    end.

%% @doc Higher-order function
-spec map_items(fun((term()) -> term()), item_list()) -> item_list().
map_items(Fun, Items) ->
    lists:map(Fun, Items).

%% @doc Anonymous function example
-spec double_all(item_list()) -> item_list().
double_all(Items) ->
    lists:map(fun(X) -> X * 2 end, Items).

%% @doc Fold example
-spec sum_all(item_list()) -> number().
sum_all(Items) ->
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, Items).

%% @doc Record usage
-spec create_person(string(), non_neg_integer()) -> #person{}.
create_person(Name, Age) ->
    #person{name = Name, age = Age}.

%% @doc Check if person is adult
-spec is_adult(#person{}) -> boolean().
is_adult(#person{age = Age}) when Age >= 18 ->
    true;
is_adult(_) ->
    false.
