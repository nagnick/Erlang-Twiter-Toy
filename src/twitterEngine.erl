-module(twitterEngine).
-import(database,[createDatabase/1,chordActor/1,databaseKiller/1,insert/3,query/2]).

-export([]).
%users send/receive tweets. engine distributes tweets
%user stuff User can only contact the engine to accomplish tasks

userActor(Username,UserFeed,EnginePID)-> % main userloop
  receive
    {tweet, Message}-> % received from engine
      userActor(Username,[Message | UserFeed],EnginePID)
  end.
%userActions

userSendTweet(EnginePID,UserName,Tweet)->
  EnginePID ! {distributeTweet,UserName,Tweet}.

userFollowUser(EnginePID,Username,ToFollowUserName)->
  EnginePID ! {addFollower,Username, ToFollowUserName}.

registerUser(EnginePID, Username)->
  EnginePID ! {registerUser,Username}.


%engine stuff
start()-> % returns enginePID
  UserDatabase = createDatabase(10),
  HashTagDatabase = createDatabase(5),
  spawn(twitterEngine,engineActor,[UserDatabase,HashTagDatabase]).
killEngine(EnginePID)->
  EnginePID ! die.

logOn(UserName,EnginePID)->
  EnginePID ! {logOn,UserName,self()},
  receive
    PID->
      PID %% return the PID of the userActor for the username given
  end.

engineActor(UserDatabase,HashTagDatabase)->
  receive
    die ->
      databaseKiller(UserDatabase),
      databaseKiller(HashTagDatabase); % loop ends here
    {registerUser,Username}->
      PID = spawn(twitterEngine,userActor,[Username,[],self()]),% actor values = Username,UserFeed,EnginePID
      insert(UserDatabase,Username, {[],[],[],PID}),%USERDATA = {UsersTweets,Following,Followers,UserActorPID}
      engineActor(UserDatabase,HashTagDatabase);
    {logOn,Username,PID}->
      {_,_,_,UserActorPID} = query(UserDatabase,Username),
      PID ! UserActorPID,
      engineActor(UserDatabase,HashTagDatabase);
    {addFollower,Username, ToFollowUserName}->
      {UsersTweets,Following,Followers,UserActorPID} = query(UserDatabase,Username),
      insert(UserDatabase,Username,{UsersTweets,[ToFollowUserName| Following],Followers,UserActorPID});
    {registerHashTag,HashTag,HashTagData}->
      insert(HashTagDatabase,HashTag,HashTagData),
      engineActor(UserDatabase,HashTagDatabase);
    {distributeTweet,Tweeter,Tweet}->
      %add parsing to add hashtag tweets to hashTag database
      {_,_,SendToList,_} = query(UserDatabase,Tweeter),
      spawn(twitterEngine,distributerActor,[SendToList,Tweet]),
      engineActor(UserDatabase,HashTagDatabase);
    %WIP
    {retriveUser,RequestingProcess,UserName}->
      RequestingProcess ! query(UserDatabase,UserName);
    {retriveHashTag,RequestingProcess,HashTag}->
      RequestingProcess ! query(HashTagDatabase,HashTag),
      engineActor(UserDatabase,HashTagDatabase)
  end.

distributerActor([],_)-> % distributes tweets to actors in SendToList
  ok;
distributerActor(SendToList,Tweet)->
  hd(SendToList) ! {tweet,Tweet},
  distributerActor(tl(SendToList),Tweet).