-module(twitterEngine).
%-import(database,[createDatabase/1,chordActor/1,databaseKiller/1,userDatabaseKiller/1,insert/3,query/2,numberToString/1]).

-export([simulate/2,endSimulate/1,zipF/4,userSubscribeTo/3,userSendTweet/3,registerUser/2,logOn/2,startEngine/1,killEngine/1,engineActor/5,distributerActor/2,userActor/5]).
%simulator stuff
simulate(NumberOfUsers,ZipfAlpha)->
  EnginePID = startEngine(NumberOfUsers),
  ListOfUserNames = spinUpUsers(EnginePID,NumberOfUsers,[]),
  io:format("Total Users ~p",[NumberOfUsers]),
  EnginePID ! {setupSim, ListOfUserNames,ZipfAlpha},
  EnginePID.
endSimulate(EnginePID)->
  EnginePID !{endSimulation,self()},
  receive
    {results,NumberOfUsers,NumOfTweets,DistributedTo}->
      io:format("Total Users ~p~nTotal Tweets sent ~p~n distributed across ~p",[NumberOfUsers,NumOfTweets,DistributedTo])
  end.
% use zipf zipf(alpha, TotalNumberofUsers,RankOfUser,0) returns an array of numbers one for each user... this number will be the
% amount of subscribers for each user. some are very popular most are not.
% if all users have a rank value 1 - n we can calculate their popularity
zipF(AlphaExponentCharacterizingDistribution, 1, X,RunningSum)-> % implemented from http://www.math.wm.edu/~leemis/chart/UDR/PDFs/Zipf.pdf
  Sum = 1 + RunningSum,
  math:ceil(1/math:pow(X,AlphaExponentCharacterizingDistribution)* Sum); % ceiling to get integers >=1
zipF(AlphaExponentCharacterizingDistribution, N, X, RunningSum)-> % N is total users and X is the rank of a random user
  Sum = math:pow(1/N,AlphaExponentCharacterizingDistribution) + RunningSum,
  zipF(AlphaExponentCharacterizingDistribution,N-1,X,Sum).

spinUpUsers(_,0,ListOfUserNames)->
  ListOfUserNames;
spinUpUsers(EnginePID,NumberOfUsers,ListOfUserNames)->
  RandomUsername = numberToString(NumberOfUsers), % create random username based off userNumber
  registerUser(EnginePID,RandomUsername),
  spinUpUsers(EnginePID,NumberOfUsers-1,[RandomUsername|ListOfUserNames]).

%users send/receive tweets. engine distributes tweets
%user stuff User can only contact the engine to accomplish tasks
userActor(Username,UserFeed,TotalNumberOfUsers,NumberOfSubscribers,EnginePID)-> % main userloop this will become the socket in part 2
  receive
    {tweet, Message}-> % received from engine % in part 2 these will be relayed over the socket if connected
      userSimulatedLogic(TotalNumberOfUsers,NumberOfSubscribers,EnginePID,Username),
      userActor(Username,[Message | UserFeed],TotalNumberOfUsers,NumberOfSubscribers,EnginePID);
    {tweets, ListOfTweets}-> % received from engine as a result of following someone new
      userSimulatedLogic(TotalNumberOfUsers,NumberOfSubscribers,EnginePID,Username),
      userActor(Username,[ListOfTweets|UserFeed],TotalNumberOfUsers,NumberOfSubscribers,EnginePID);
    {simSubCountUpdate, NumOfSubs}-> % part of simulator startup first thing done by actor in sim
      userActor(Username,UserFeed,TotalNumberOfUsers,NumOfSubs,EnginePID), % no simulated logic until after this as actor data incomplete
      userSimulatedLogic(TotalNumberOfUsers,NumberOfSubscribers,EnginePID,Username)
  end.
userSimulatedLogic(TotalNumberOfUsers,NumberOfSubscribers,EnginePID,Username)->
  % random chance of sending a tweet
  ChanceToTweet = rand:uniform(TotalNumberOfUsers),
  ChanceToBeActive = rand:uniform(24),
  % in simulate we assign users subscribers according to zipf the ones with more subs will send tweets with greater probability
  if
    ChanceToBeActive < 23 -> % random simulation of user being live random num out of 24 if less than 7 then live...
      if
        NumberOfSubscribers < ChanceToTweet -> % the more subs you have the more likely you are to tweet
          userSendTweet(EnginePID,Username,"MYTWEET")
      end
  end.
%userActions
userSubscribeTo(EnginePID,UserOrHashTag,ID)-> % done
  EnginePID ! {subscribeTo,self(),UserOrHashTag,ID}.% UserOrHashTag = 'user' for user and 'hash' for hashTag

userSendTweet(EnginePID,UserName,Tweet)-> % done
  % tweets can be original or a retweet(gotten from userfeed) left for user to decide as engine see's no difference between them
  EnginePID ! {distributeTweet,UserName,Tweet}.


%engine stuff
startEngine(TotalNumberOfUsers)-> % returns enginePID % done
  UserDatabase = #{},
  HashTagDatabase = #{},
  spawn(twitterEngine,engineActor,[UserDatabase,HashTagDatabase,TotalNumberOfUsers,0,0]).
killEngine(EnginePID)-> % done
  EnginePID ! die.

registerUser(EnginePID, Username)-> % done
  EnginePID ! {registerUser,Username}.

logOn(UserName,EnginePID)->%userStartUp part 2 stuff WIP pass this engine useractor to the websocket actor as api
  EnginePID ! {logOn,UserName,self()},
  receive
    PID->
      PID %% return the PID of the userActor for the username given
  end.

engineActor(UserDatabase,HashTagDatabase,TotalNumberOfUsers,NumOfTweets,DistributedTo)->
  receive
    die -> % done
      %userDatabaseKiller(UserDatabase), % special version that kills the userActors as well
      %databaseKiller(HashTagDatabase),
      ok;
      % loop ends here don't call self
    {endSimulation,SuperVisor}->
      %userDatabaseKiller(UserDatabase), % special version that kills the userActors as well
      %databaseKiller(HashTagDatabase),
      SuperVisor ! {results,TotalNumberOfUsers,NumOfTweets,DistributedTo};
    {setupSim, ListOfUserNames,ZipfAlpha}-> % done
      %set each user up to have all its subs based on zipF distribution
      NewUserDatabase = engineSimSetup(UserDatabase,ListOfUserNames,ListOfUserNames,ZipfAlpha,TotalNumberOfUsers,1),
      engineActor(NewUserDatabase,HashTagDatabase,TotalNumberOfUsers,NumOfTweets,DistributedTo);
    {registerUser,Username}-> % done
      PID =  spawn(twitterEngine,userActor,[Username,[],TotalNumberOfUsers,0,self()]), %TotalNumberOfUsers used for tweet simulation...
      %insert(UserDatabase,Username, {[],[],PID}),%USERDATA = {User'sTweets,Subscribers(followers),UserActorPID}
      engineActor(maps:put(Username,{[],[],PID},UserDatabase),HashTagDatabase,TotalNumberOfUsers,NumOfTweets,DistributedTo);
    {logOn,Username,PID}-> % done
      {_,_,ActorPID} = maps:get(Username,UserDatabase),
      PID ! ActorPID,
      engineActor(UserDatabase,HashTagDatabase,TotalNumberOfUsers,NumOfTweets,DistributedTo);
    {subscribeTo,UserPID,user,UserToSubscribeTo}-> % WIP
      %should i check if UserToSubscribeTo exists in DB? return null if not in DB
      {Tweets,Subs,SubActorPID} = maps:get(UserToSubscribeTo,UserDatabase),
      %insert(UserDatabase,UserToSubscribeTo,{Tweets,[UserPID|Subs],SubActorPID}),% update followers list should i check if already in list?
      %Send to user all tweets messed from before subed (user must organize if desired)
      UserPID ! {tweets,Tweets},
      engineActor(maps:put(UserToSubscribeTo,{Tweets,[UserPID|Subs],SubActorPID},UserDatabase),HashTagDatabase,TotalNumberOfUsers,NumOfTweets,DistributedTo);
    {subscribeTo,UserPID,hash,HashToSubscribeTo}-> %WIP
      %should i check if HashToSubscribeTo exists in DB? return null if not in DB
      {Tweets,Subs,SubActorPID} = maps:get(HashToSubscribeTo,HashTagDatabase),
      %insert(HashTagDatabase,HashToSubscribeTo,{Tweets,[UserPID|Subs],SubActorPID}),% update followers list
      %Send to user all tweets messed from before subed (user must organize if desired)
      UserPID ! {tweets,Tweets},
      engineActor(UserDatabase,maps:put(HashToSubscribeTo,{Tweets,[UserPID|Subs],SubActorPID},HashTagDatabase),TotalNumberOfUsers,NumOfTweets,DistributedTo);
    {distributeTweet,Tweeter,Tweet}-> % WIP
      % tweet = [heloo world #uf #top5 @mybestfriend]
      % ListofTags = [#uf ,#top5] from parser
      %add parsing to add hashtag tweets to hashTag database
        %ListofTags =[],% parser(Tweet),
        %ListOfMentions =
        %self() ! {distributeHashTag,hd(ListofTags),Tweet},
      {Tweets,Subs,SubActorPID} = maps:get(Tweeter,UserDatabase), % send to followers
      spawn(twitterEngine,distributerActor,[Subs,Tweet]),% send tweet with distributer in own process
      % add to Tweeter's Tweet list
      %insert(UserDatabase,Tweeter,{[Tweet | Tweets],Subs,SubActorPID}), % update users list of tweets
      engineActor(maps:put(Tweeter,{[Tweet | Tweets],Subs,SubActorPID},UserDatabase),HashTagDatabase,TotalNumberOfUsers,NumOfTweets + 1,DistributedTo + length(Subs));
    {distributeHashTag,HashTag,Tweet}-> % done
      %Result = query(HashTagDatabase,HashTag),
      Result = maps:get(HashTag,HashTagDatabase),
      if
        Result == null ->
          {Tweets,Subs} = {[],[]},
          %insert(HashTagDatabase,HashTag, {[],[]});
          HashTagDatabase2 = maps:put(HashTag,{[],[]},HashTagDatabase);
        true ->
          {Tweets,Subs} = Result,
          HashTagDatabase2 = HashTagDatabase
      end,
      distributerActor(Subs,Tweet),
      %insert(HashTagDatabase,HashTag, {[Tweet|Tweets]}),
      HashTagDatabase3 = maps:put(HashTag,{[Tweet|Tweets]},HashTagDatabase2),
      engineActor(UserDatabase,HashTagDatabase3,TotalNumberOfUsers,NumOfTweets,DistributedTo)
  end.

engineSimSetup(UserDatabase,[],_,_,_,_)-> % set each user to have a num of subs based on zipf distribution
  UserDatabase;
engineSimSetup(UserDatabase,ListOfUsers,CompleteListOfUsers,ZipfAlpha,TotalNumberOfUsers,Rank)->
  NumberOfSubsUserShouldHave = trunc(zipF(ZipfAlpha,TotalNumberOfUsers,Rank,0)),% convert float from zipf to int
  {ListOfSubs,_} = lists:split(NumberOfSubsUserShouldHave,CompleteListOfUsers), % returns first n number of users from
  % they will become the followers of this user
  %{UserTweets,_,UserActorPID} = query(UserDatabase,hd(ListOfUsers)),
  {UserTweets,_,UserActorPID} = maps:get(hd(ListOfUsers),UserDatabase),
  % update the user's sub count
  UserActorPID ! {simSubCountUpdate, length(ListOfSubs)},
  %insert(UserDatabase,hd(ListOfUsers),{UserTweets,ListOfSubs,UserActorPID}),
  ListOfSubPIDS = convertUserNamesToPids(UserDatabase,ListOfSubs,[]),
  engineSimSetup(maps:put(hd(ListOfUsers),{UserTweets,ListOfSubPIDS,UserActorPID},UserDatabase),tl(ListOfUsers),CompleteListOfUsers,ZipfAlpha,TotalNumberOfUsers,Rank+1).

distributerActor([],_)-> % distributes tweets to actors in SendToList allows engine to focus on main functionality
  ok;
distributerActor(SendToList,Tweet)->
  hd(SendToList) ! {tweet,Tweet},
  distributerActor(tl(SendToList),Tweet).

%helpers
numberToString(N) when N < 94 -> % 94 possible chars
  [(N+33)]; % 33 = '!' 33 + 93 = 126 = '~' last acceptable char to us
numberToString(N) when N >= 94->
  numberToString(N div 94) ++ numberToString(N rem 94).
convertUserNamesToPids(_,[],PidList)->
  PidList;
convertUserNamesToPids(UserDatabase,UsernameList,PidList)->
  {_,_,UserPID} = maps:get(hd(UsernameList),UserDatabase),
  convertUserNamesToPids(UserDatabase,tl(UsernameList),[UserPID|PidList]).