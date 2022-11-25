-module(database).
-author("nicolas").
-import(crypto,[hash/2]).
-export([createDatabase/1,chordActor/1,databaseKiller/1,insert/3,query/2]).

findSuccessor(SortedMapList,MinValue)-> % function searches the list to find the first value grater than or equal to MinValue
  findSuccessor(SortedMapList,MinValue,SortedMapList).% saves original list in case of a wrap around ex min value is larger than largest
% value in list so return min value of list aka head
findSuccessor([],_,Original)-> % if you get to the end of the list return first item in list
  hd(Original);
findSuccessor(SortedMapList,MinValue,Original)->
  {Key,_} = hd(SortedMapList),
  if
    Key >= MinValue ->
      hd(SortedMapList);
    true ->
      findSuccessor(tl(SortedMapList),MinValue,Original)
  end.

findPredecessor(FingerTable,Key)-> % function searches the list to find the biggest value less than or equal to MinValue
  findPredecessor(FingerTable,Key,hd(FingerTable)).% No predecessor so return successor needs to make another round in loop
% value in list so return min value of list aka head
findPredecessor([],_, ImmediateSuccessor)-> % if you get to the end of the list return first item in list
  ImmediateSuccessor;
findPredecessor(FingerTable,Key,LastBestValue)->
  {ActorHash,_} = hd(FingerTable),
  if
    Key >= ActorHash -> % find actor with a large hash that is still less than key looking for
      if
        hd(FingerTable) > LastBestValue ->
          findPredecessor(tl(FingerTable),Key,hd(FingerTable));
        true -> % necessary for max value insert to work aka key is greater than largest actor hash
          findPredecessor(tl(FingerTable),Key,LastBestValue)
      end;
    true -> % this item goes to far return last actor that was less than key but keep going maybe better actor at end of finger table
      findPredecessor(tl(FingerTable),Key,LastBestValue)
  end.

createFingerTable(_,_,0,FingerTable) ->%List max size is 160
  FingerTable; % table is a list of {hashvalue,PID}
createFingerTable(ActorHash,SortedListOfPids,I,FingerTable)-> % I is size of fingerTable and current index being filled
  %io:format("~w~n",[SortedListOfPids]),
  NextEntryMinHash = (ActorHash + round(math:pow(2,I-1))) rem round(math:pow(2,160)), % formula in doc hash size is 160 so table is 160
  Hash = findSuccessor(SortedListOfPids,NextEntryMinHash), %% get smallest actor hash to fill current spot
  createFingerTable(ActorHash,SortedListOfPids,I-1,[Hash | FingerTable]).

chordActor(HashId)-> % startingPoint of actor
  receive
    {init, MapOfPids}->
      % make a finger table based of map of hash => PID, first make map a list sorted based on hash
      NewFingerTable = createFingerTable(HashId, lists:keysort(1,maps:to_list(MapOfPids)),160,[]), % initial FingerTable is empty and start filling fingerTable at index 1
      % Map of PIDs is sorted so that the search for next in line actors is most efficient
      chordActor(NewFingerTable,#{},HashId,MapOfPids) % fingerTable is filled with {hashID,PID} tuples of the actors in network
  % carries map of PIDs to build future fingerTables from
  end.

chordActor( FingerTable,DataTable,HashId, MapOfPids)-> %final main actor
  receive
    {found,Key,SearchersPID}->
      try SearchersPID ! {queryResult,maps:get(Key,DataTable)}
      catch _:_ -> SearchersPID ! {queryResult,null} end,
      chordActor(FingerTable,DataTable,HashId,MapOfPids);

    {find,Key,SearchersPID}->
      KeyHash = decimalShaHash(Key),
      {ToAskHash,ToAskPID} = findPredecessor(FingerTable,decimalShaHash(Key)), % returns tuple {HashKey, PID}
      InMyDataTable = checkActorDataTable(DataTable,Key),
      if
        InMyDataTable == true-> % if in my data table done return value
          SearchersPID ! {queryResult,maps:get(Key,DataTable)},
          NewMap = DataTable;
        HashId >= ToAskHash-> % looped to front
          if
            KeyHash > ToAskHash, KeyHash > HashId -> % value bigger than biggest node so search in smallest node
              ToAskPID ! {found,Key,SearchersPID},
              NewMap = DataTable;
            ToAskHash >= KeyHash-> % min value in min node
              ToAskPID ! {found,Key,SearchersPID},
              NewMap = DataTable;
            true -> % haven't got to best spot yet loop around
              ToAskPID ! {find,Key,SearchersPID},
              NewMap = DataTable
          end;
        true -> % increasing but don't know if passed optimal node so keep going until wrap around
          if
            KeyHash > HashId, ToAskHash >= KeyHash-> % found best node
              ToAskPID ! {found,Key,SearchersPID},
              NewMap = DataTable;
            true -> % passed best spot
              ToAskPID ! {find,Key,SearchersPID},
              NewMap = DataTable
          end
      end,
      chordActor(FingerTable,NewMap,HashId,MapOfPids);

    {finalAddKeyValue,Key,Value,RequesteePID} -> % I am node that should hold key, value pair
      NewMap = maps:put(Key,Value,DataTable),
      RequesteePID ! {dataInserted,Value}, % tell data is inserted
      chordActor(FingerTable,NewMap,HashId,MapOfPids);

    {addKeyValue,Key,Value,RequesteePID}->
      KeyHash = decimalShaHash(Key),
      {ToAskHash,ToAskPID} = findPredecessor(FingerTable,KeyHash), % returns tuple {HashKey, PID}
      if
        HashId >= ToAskHash-> % looped to front
          if
            KeyHash > ToAskHash, KeyHash > HashId -> % value bigger than biggest node so insert in smallest node
              ToAskPID ! {finalAddKeyValue,Key,Value,RequesteePID},
              NewMap = DataTable;
            ToAskHash >= KeyHash-> % min value in min node
              ToAskPID ! {finalAddKeyValue,Key,Value,RequesteePID},
              NewMap = DataTable;
            true -> % haven't got to best spot yet
              ToAskPID ! {addKeyValue,Key,Value,RequesteePID},
              NewMap = DataTable
          end;
        true -> % increasing but don't know if passed optimal node so keep going until wrap around
          if
            KeyHash > HashId, ToAskHash >= KeyHash-> % found best node
              ToAskPID ! {finalAddKeyValue,Key,Value,RequesteePID},
              NewMap = DataTable;
            true -> % passed best spot
              ToAskPID ! {addKeyValue,Key,Value,RequesteePID},
              NewMap = DataTable
          end
      end,
      chordActor(FingerTable,NewMap,HashId,MapOfPids)
  end.

decimalShaHash(N)->
  binary:decode_unsigned(crypto:hash(sha,N)). % use sha 1 like doc says max size is unsigned 160 bit value = 1461501637330902918203684832716283019655932542976

createDatabase(NumberOfActors)-> % number of request means each actor must make that many SuperVisor of network will get responses from actors
  MapOfActors = spawnMultipleActors(NumberOfActors,#{}), % hashed key,PID map returned
  ListOfActors = [X || {_,X} <- maps:to_list(MapOfActors)], % remove hash keys only want pids of actors from now on
  init(ListOfActors,MapOfActors), % init first then start to begin searching(don't want actors to search from actors not done with init)
  %return list of actors so twitter engine can use them so search/ insert
  ListOfActors.

init([],_)-> % sends actors everything they need to initialize finger table and data map
  ok;
init(ListOfActors,MapOfActors)->
  PID = hd(ListOfActors),
  PID !  {init,MapOfActors},
  init(tl(ListOfActors),MapOfActors).

insert(ListOfDatabaseActors,Key,Value)->
  PID = lists:nth(rand:uniform(length(ListOfDatabaseActors)),ListOfDatabaseActors), % insert starting at random actors
  PID ! {addKeyValue,Key,Value,self()},
  receive
    {dataInserted,_}->
      true
  end.

query(ListOfDatabaseActors,Key)-> % optimize don't pick random pick based on key hash? swap to map to get hashId to pick instead?
  PID = lists:nth(rand:uniform(length(ListOfDatabaseActors)),ListOfDatabaseActors), % insert starting at random actors
  PID ! {find,Key,self()},
  receive
    {queryResult,Result}->
      Result
  end.

databaseKiller([])-> %Tell actors to kill themselves the swarm has converged
  ok;
databaseKiller(ListOfDatabaseActors)->
  PID = hd(ListOfDatabaseActors),
  exit(PID,kill),
  databaseKiller(tl(ListOfDatabaseActors)).

spawnMultipleActors(0,MapOfPid)->
  MapOfPid;
spawnMultipleActors(NumberOfActorsToSpawn,MapOfPid)->
  ActorIDHash = decimalShaHash( numberToString(NumberOfActorsToSpawn)),
  NewMap = maps:put(ActorIDHash,spawn(database,chordActor,[ActorIDHash]), MapOfPid),
  spawnMultipleActors( NumberOfActorsToSpawn-1,NewMap).

numberToString(N) when N < 94 -> % 94 possible chars
  [(N+33)]; % 33 = '!' 33 + 93 = 126 = '~' last acceptable char to us
numberToString(N) when N >= 94->
  numberToString(N div 94) ++ numberToString(N rem 94).

checkActorDataTable(DataTable,Value)->
  Val = maps:find(Value,DataTable),
  if
    Val == error->
      false;
    true-> % returned value
      true
  end.