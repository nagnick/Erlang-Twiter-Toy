-module(database).
-author("nicolas").
-import(crypto,[hash/2]).
-export([createDatabase/1,chordActor/2,actorKiller/1,fillWithData/2]).

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

chordActor(SuperVisor,HashId)-> % startingPoint of actor
  receive
    {init, MapOfPids}->
      % make a finger table based of map of hash => PID, first make map a list sorted based on hash
      NewFingerTable = createFingerTable(HashId, lists:keysort(1,maps:to_list(MapOfPids)),160,[]), % initial FingerTable is empty and start filling fingerTable at index 1
      % Map of PIDs is sorted so that the search for next in line actors is most efficient
      chordActor(SuperVisor,NewFingerTable,HashId,MapOfPids) % fingerTable is filled with {hashID,PID} tuples of the actors in network
  % carries map of PIDs to build future fingerTables from
  end.

chordActor(SuperVisor, FingerTable,HashId,MapOfPids)-> % second step of actor
  %io:format("ActorHashID:~w~p~n",[HashId,self()]),
  receive
  %add searchSet receive to start the searching process
    {searchSet,SearchSetList}->
      chordActor(SuperVisor, FingerTable,#{},HashId,SearchSetList,MapOfPids,0)
  end.

chordActor(SuperVisor, FingerTable,DataTable,HashId,SearchSetList, MapOfPids, HopsRunningSum)-> %final main actor
  receive
    {simulate}->
      queryListFromInsideActor(SearchSetList,FingerTable),
      chordActor(SuperVisor, FingerTable,DataTable,HashId,SearchSetList,MapOfPids,HopsRunningSum);

    {queryResult,Result,Hops}-> % got result of its search back
      NewSearchSetList = SearchSetList -- [decimalShaHash(Result)],
      %io:format("Found ~s~nFrom~p~n",[Result,self()]), % if fails gets a tuple{badmap,Map}
      if
        length(NewSearchSetList) == 0 ->
          SuperVisor ! {done,self(),Hops + HopsRunningSum};
        true->
          io:format("") % do nothing to prevent if clause error
      end,
      chordActor(SuperVisor, FingerTable,DataTable,HashId,NewSearchSetList,MapOfPids,HopsRunningSum+Hops);

    {found,Key,SearchersPID,Hops}->
      Result = maps:get(Key,DataTable),
      SearchersPID ! {queryResult,Result,Hops+1},
      chordActor(SuperVisor, FingerTable,DataTable,HashId,SearchSetList,MapOfPids,HopsRunningSum);

    {find,Key,SearchersPID,Hops}->
      {ToAskHash,ToAskPID} = findPredecessor(FingerTable,Key), % returns tuple {HashKey, PID}
      InMyDataTable = checkActorDataTable(DataTable,Key),
      if
        InMyDataTable == true-> % if in my data table done return value
          SearchersPID ! {queryResult,maps:get(Key,DataTable),Hops},
          NewMap = DataTable;
        HashId >= ToAskHash-> % looped to front
          if
            Key > ToAskHash, Key > HashId -> % value bigger than biggest node so search in smallest node
              ToAskPID ! {found,Key,SearchersPID,Hops+1},
              NewMap = DataTable;
            ToAskHash >= Key-> % min value in min node
              ToAskPID ! {found,Key,SearchersPID,Hops+1},
              NewMap = DataTable;
            true -> % haven't got to best spot yet loop around
              ToAskPID ! {find,Key,SearchersPID,Hops+1},
              NewMap = DataTable
          end;
        true -> % increasing but don't know if passed optimal node so keep going until wrap around
          if
            Key > HashId, ToAskHash >= Key-> % found best node
              %io:format("Ask ~p from ~p ~n~w~n",[ToAskPID,self(),FingerTable]),
              ToAskPID ! {found,Key,SearchersPID,Hops+1},
              NewMap = DataTable;
            true -> % passed best spot
              ToAskPID ! {find,Key,SearchersPID,Hops+1},
              NewMap = DataTable
          end
      end,
      chordActor(SuperVisor, FingerTable,NewMap,HashId,SearchSetList,MapOfPids,HopsRunningSum);

    {finalAddKeyValue,Key,Value} -> % I am node that should hold key, value pair
      NewMap = maps:put(Key,Value,DataTable),
      %io:format("My ~p,~w Data Table~w~n",[self(),HashId,NewMap]),
      SuperVisor ! {dataInserted,Value}, % tell supervisor data is inserted so it knows when to start simulation;
      chordActor(SuperVisor, FingerTable,NewMap,HashId,SearchSetList,MapOfPids,HopsRunningSum);

    {addKeyValue,Key,Value}->
      {ToAskHash,ToAskPID} = findPredecessor(FingerTable,Key), % returns tuple {HashKey, PID}
      if
        HashId >= ToAskHash-> % looped to front
          if
            Key > ToAskHash, Key > HashId -> % value bigger than biggest node so insert in smallest node
              %io:format("Ask ~p from ~p ~n~w~n",[ToAskPID,self(),FingerTable]),
              ToAskPID ! {finalAddKeyValue,Key,Value},
              NewMap = DataTable;
            ToAskHash >= Key-> % min value in min node
              %io:format("Ask ~p from ~p ~n~w~n",[ToAskPID,self(),FingerTable]),
              ToAskPID ! {finalAddKeyValue,Key,Value},
              NewMap = DataTable;
            true -> % haven't got to best spot yet
              ToAskPID ! {addKeyValue,Key,Value},
              NewMap = DataTable
          end;
        true -> % increasing but don't know if passed optimal node so keep going until wrap around
          if
            Key > HashId, ToAskHash >= Key-> % found best node
              %io:format("Ask ~p from ~p ~n~w~n",[ToAskPID,self(),FingerTable]),
              ToAskPID ! {finalAddKeyValue,Key,Value},
              NewMap = DataTable;
            true -> % passed best spot
              ToAskPID ! {addKeyValue,Key,Value},
              NewMap = DataTable
          end
      end,
      chordActor(SuperVisor, FingerTable,NewMap,HashId,SearchSetList,MapOfPids,HopsRunningSum)
  end.

queryListFromInsideActor([],_)->
  ok;
queryListFromInsideActor(List,FingerTable)->
  self() !{find,hd(List),self(),0}, % start looking at me
  queryListFromInsideActor(tl(List),FingerTable).

decimalShaHash(N)->
  binary:decode_unsigned(crypto:hash(sha,N)). % use sha 1 like doc says max size is unsigned 160 bit value = 1461501637330902918203684832716283019655932542976

createDatabase(NumberOfActors)-> % number of request means each actor must make that many SuperVisor of network will get responses from actors
  MapOfActors = spawnMultipleActors(NumberOfActors,#{}), % hashed key,PID map returned
  ListOfActors = [X || {_,X} <- maps:to_list(MapOfActors)], % remove hash keys only want pids of actors from now on
  init(ListOfActors,MapOfActors), % init first then start to begin searching(don't want actors to search from actors not done with init)
  Data = [],
  fillWithData(ListOfActors,Data),
  %return list of actors so twitter engine can use them so search/ insert
  ListOfActors.
  %actorKiller(ListOfActors).

init([],_)-> % sends actors everything they need to initialize finger table and data map
  ok;
init(ListOfActors,MapOfActors)->
  PID = hd(ListOfActors),
  PID !  {init,MapOfActors},
  init(tl(ListOfActors),MapOfActors).

fillWithData(_,[])->
  ok;
fillWithData(ListOfActors,CollisionFreeDataSet)->
  PID = lists:nth(rand:uniform(length(ListOfActors)),ListOfActors), % insert starting at random actors
  PID ! {addKeyValue,decimalShaHash(hd(CollisionFreeDataSet)),hd(CollisionFreeDataSet)},
  receive
    {dataInserted,Value}->
      fillWithData(ListOfActors,CollisionFreeDataSet -- [Value])
  end.

actorKiller([])-> %Tell actors to kill themselves the swarm has converged
  ok;
actorKiller(ListOfActors)->
  PID = hd(ListOfActors),
  exit(PID,kill),
  actorKiller(tl(ListOfActors)).

spawnMultipleActors(0,MapOfPid)->
  MapOfPid;
spawnMultipleActors(NumberOfActorsToSpawn,MapOfPid)->
  ActorIDHash = decimalShaHash( numberToString(NumberOfActorsToSpawn)),
  NewMap = maps:put(ActorIDHash,spawn(project3,chordActor,[self(),ActorIDHash]), MapOfPid),
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