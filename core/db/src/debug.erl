-module(debug).
-export([item_process/0, max_process/0, max_client/0, rest_process/0,
		conn_process/0]).


conn_process() ->
	Pid = whereis(riak_backend),
	process_info(Pid).

item_process() ->
        PropsItem = wpool:stats(riak_menu_item),
        WorkerProcess = proplists:get_value(workers, PropsItem),
        {ResSum, ResL} = lists:foldl(fun({Num, InfoList}, {Sum, List}) ->
                QVal = proplists:get_value(message_queue_len, InfoList),
                MemVal = proplists:get_value(memory, InfoList),
                RunTime = proplists:get_value(runtime, InfoList, 0),
                { Sum + MemVal, [{Num, QVal, MemVal, {runtime, RunTime}} | List] }
        end,{0, []}, WorkerProcess),
        {ResSum, lists:reverse(lists:keysort(3, ResL))}.

rest_process() ->
        PropsRes = wpool:stats(riak_restaurant),
        WorkerProcess = proplists:get_value(workers, PropsRes),
        {ResSum, ResL} = lists:foldl(fun({Num, InfoList}, {Sum, List}) ->
                QVal = proplists:get_value(message_queue_len, InfoList),
                MemVal = proplists:get_value(memory, InfoList),
                RunTime = proplists:get_value(runtime, InfoList, 0),
                { Sum + MemVal, [{Num, QVal, MemVal, {runtime, RunTime}} | List] }
        end,{0, []}, WorkerProcess),
        {ResSum, lists:reverse(lists:keysort(3, ResL))}.


max_process() ->
        Processes = processes(),
        {ResSum, ResL} = lists:foldl(fun(Proc, {Sum, List} = Acc)  ->
                case is_process_alive(Proc) of
                true ->
                        InfoList = process_info(Proc, [memory, message_queue_len]),
                        ProcessI = process_info(Proc, [current_function, current_location,
                                                status, heap_size, stack_size, last_calls]),
                        QVal = proplists:get_value(message_queue_len, InfoList),
                        MemVal = proplists:get_value(memory, InfoList),
                        { Sum + MemVal, [{Proc, QVal, MemVal, ProcessI} | List] };
                _ -> Acc
                end
        end, {0, []}, Processes),
        {ResSum, lists:reverse(lists:keysort(3, ResL))}.

max_client() ->
        Processes = lists:map(fun({Proc}) -> Proc end, ets:tab2list(test)),
        {ResSum, ResL} = lists:foldl(fun(Proc, {Sum, List} = Acc)  ->
                case is_process_alive(Proc) of
                true ->
                        InfoList = process_info(Proc, [memory, message_queue_len]),
                        ProcessI = process_info(Proc, [current_function, current_location,
                                                status, heap_size, stack_size, last_calls]),
                        QVal = proplists:get_value(message_queue_len, InfoList),
                        MemVal = proplists:get_value(memory, InfoList),
                        { Sum + MemVal, [{Proc, QVal, MemVal, ProcessI} | List] };
                _  -> Acc
                end
        end, {0, []}, Processes),
        {ResSum, lists:reverse(lists:keysort(3, ResL))}.
