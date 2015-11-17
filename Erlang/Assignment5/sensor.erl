-module(sensor).
-export([create_sensor/2, thread_sensor/2]).

%%create_sensor(WatcherID, SensorSubID) ->
    %%create_sensor(WatcherID, SensorSubID).

%create_sensor(_, -1, List) -> List;
create_sensor(WatcherID, SensorSubID) ->
    SensorID = WatcherID * 10 + SensorSubID,
    %%io:fwrite("WatcherID: ~p create sensor, SensorID: ~p~n", [WatcherID, SensorID]),
    {PID, _} = spawn_monitor(?MODULE, thread_sensor, [self(), SensorID]),
    %%create_sensor(WatcherID, SensorSubID - 1, [{SensorID, PID} | List]).
    case SensorSubID of
        0 -> [{SensorID, PID}];
        _ -> [{SensorID, PID} | create_sensor(WatcherID, SensorSubID - 1)]
    end.

thread_sensor(Watcher, SensorID) ->
    random:seed(now()),
    sensor_loop(Watcher, SensorID).

sensor_loop(Watcher, SensorID) ->
    Measurement = random:uniform(11),
    Sleep_time = random:uniform(10000),
    timer:sleep(Sleep_time),
    if
        Measurement == 11 ->
            exit({SensorID, anomalous_reading});
        true ->
            Watcher ! {SensorID, Measurement}
    end,
    sensor_loop(Watcher, SensorID).
