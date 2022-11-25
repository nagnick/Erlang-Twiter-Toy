-module(twitterEngine).
-import(database,[createDatabase/1,chordActor/1,databaseKiller/1,insert/3,query/2]).

-export([]).
%users send/receive tweets. engine distributes tweets
%user stuff
userActor(TweetList,FollowerList,FollowingList,UserDatabase)->
  receive
    {tweet, Message}->
      userActor([Message | TweetList],FollowerList,FollowingList,UserDatabase);
    {follow, UserId}->
      NewFollowingList = query(UserDatabase,UserId),
      userActor(TweetList,FollowerList,NewFollowingList,UserDatabase)
  end.

registerUser(Database, Username)->
  PID = spawn(twitterEngine,userActor,[[],[],[],Database]),
  %add PID to userData and push into database
  Value = {PID,[],[],[]},
  insert(Database,Username,Value).


%engine stuff
start()-> % returns enginePID
  UserDatabase = createDatabase(10),
  HashTagDatabase = createDatabase(5),
  spawn(twitterEngine,engineActor,[]).

engineActor(UserDatabase,HashTagDatabase)->
  receive
    {registerUser,UserName,UserData}->
      insert(UserDatabase,UserName,UserData);
    {distributeTweet,SendToList,Tweet}->
      spawn(twitterEngine,distributerActor,[SendToList,Tweet]);
    {retriveUser,RequestingProcess,UserName}->
      RequestingProcess ! query(UserDatabase,UserName)
  end,
  engineActor(UserDatabase,HashTagDatabase).

distributerActor([],_)-> % distributes tweets to actors in SendToList
  ok;
distributerActor(SendToList,Tweet)->
  hd(SendToList) ! Tweet,
  engineActor(tl(SendToList),Tweet).