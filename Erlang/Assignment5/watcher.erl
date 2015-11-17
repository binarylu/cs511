-module(watcher).
-export([start/0, thread_watcher/2]).
-import(sensor, [create_sensor/2]).

start() ->
    SensorTotal = 13,
    SensorsPerWatcher = 10,
    WatcherNum = case SensorTotal rem SensorsPerWatcher of
                     0 -> SensorTotal div SensorsPerWatcher;
                     _ -> SensorTotal div SensorsPerWatcher + 1
                 end,
    create_watcher(WatcherNum - 1, SensorTotal, SensorsPerWatcher),
    loop().

loop() ->
    loop().

create_watcher(-1, _, _) -> done;
create_watcher(WatcherID, SensorTotal, SensorsPerWatcher) ->
    SensorNum = case WatcherID == SensorTotal div SensorsPerWatcher of
                    true -> SensorTotal rem SensorsPerWatcher;
                    false -> SensorsPerWatcher
                end,
    spawn(?MODULE, thread_watcher, [WatcherID, SensorNum]),
    create_watcher(WatcherID - 1, SensorTotal, SensorsPerWatcher).

thread_watcher(WatcherID, SensorNum) ->
    List = create_sensor(WatcherID, SensorNum - 1),
    io:fwrite("Watcher #~p starts: ~p~n", [WatcherID, List]),
    watcher_loop(WatcherID, List).

watcher_loop(WatcherID, List) ->
    NewList = receive
                  {SensorID, Measurement} ->
                      io:fwrite("(~p)Sensor #~p: ~p~n", [WatcherID, SensorID, Measurement]),
                      List;
                  {'DOWN', _, process, Pid, {SensorID, Reason}} ->
                      io:fwrite("Sensor ~p died, reason: ~p~n", [SensorID, Reason]),
                      {PID, _} = spawn_monitor(sensor, thread_sensor, [self(), SensorID]),
                      NewL = [{SensorID, PID} | lists:delete({SensorID, Pid}, List)],
                      io:fwrite("Watcher #~p new sensor list: ~w~n", [WatcherID, NewL]),
                      NewL
              end,
    watcher_loop(WatcherID, NewList).
