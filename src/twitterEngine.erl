-module(twitterEngine).
%-import(database,[createDatabase/1,chordActor/1,databaseKiller/1,userDatabaseKiller/1,insert/3,query/2,numberToString/1]).

-export([tweetSpecialCharParse/2,simulate/3,userSubscribeTo/3,userSendTweet/3,registerUser/2,logOn/2,startEngine/1,killEngine/1,engineActor/5,distributerActor/2,userActor/3,processHashTags/3,processMentions/3]).
%simulator stuff
simulate(NumberOfUsers,ZipfAlpha,SimulationRunTimeInMS)->
  EnginePID = startEngine(NumberOfUsers),
  ListOfUserNames = spinUpUsers(EnginePID,NumberOfUsers,[]),
  io:format("Setup complete. Starting simulation...~n"),
  EnginePID ! {setupSim, ListOfUserNames,ZipfAlpha},
  StartTime = erlang:monotonic_time(),%recommend replacement to now()
  timer:sleep(SimulationRunTimeInMS),
  io:format("Timer up final messages being processed. May take awhile if engine has large backlog >1000.~n"),
  {_,Num} = erlang:process_info(EnginePID, message_queue_len),
  io:format("Messages engine must process ~p before exiting.~n",[Num]),
  EnginePID !{endSimulation,self()},
  receive
    {results,NumOfSpecialTweets,NumOfTweets,DistributedTo}->
      io:format("Simulation statistics:~n"),
      io:format("Total Users ~p~nTotal Tweets sent ~p~nTotal Number of tweets with @'s and #'s: ~p~nTweets distributed across to:~p users~n",[NumberOfUsers,NumOfTweets,NumOfSpecialTweets,DistributedTo]),
      EndTime = erlang:monotonic_time(),
      REALTIME = erlang:convert_time_unit(EndTime-StartTime,native,microsecond),
      io:format("Total runtime including setup and final message processign of engine:~p~n", [REALTIME]),
      io:format("Engine Tweet average throughput per ms: ~p~n",[NumOfTweets/REALTIME])
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
userActor(Username,UserFeed,EnginePID)-> % starter
  receive
    die ->
      ok;
    {simStart, NumOfSubs}-> % part of simulator startup first thing done by actor in sim
      userActor(Username,UserFeed,NumOfSubs,EnginePID)
  end.
userActor(Username,UserFeed,NumberOfSubscribers,EnginePID)-> % main userloop this will become the socket in part 2
      NumOfTweetsToSend = rand:uniform(NumberOfSubscribers), % the more subs you have the more likely you are to tweet more messages
      ChanceToBeActive = rand:uniform(24),
      % in simulate we assign users subscribers according to zipf the ones with more subs will send tweets with greater probability
      if
        ChanceToBeActive < 7 -> % random simulation of user being live.(kinda like 7 hours a day)
          userSendRandomTweets(EnginePID,Username,NumOfTweetsToSend,UserFeed);
        true-> % not live sleep for 8 hours then wait for a tweet to send you back as notifications do
          timer:sleep(80) % 8 hour sleep like humans but not in hours
      end,
      receive
        die ->
          ok;
        {tweet, Message}-> % received from engine % in part 2 these will be relayed over the socket if connected
          userActor(Username,[Message | UserFeed],NumberOfSubscribers,EnginePID);
        {tweets, ListOfTweets}-> % received from engine as a result of following someone new
          userActor(Username,[ListOfTweets|UserFeed],NumberOfSubscribers,EnginePID);
        {simSubCountUpdate, NumOfSubs}-> % part of simulator startup first thing done by actor in sim
          userActor(Username,UserFeed,NumOfSubs,EnginePID)
  end.
%user simulated action.
userSendRandomTweets(_,_,0,_)->
  ok;
userSendRandomTweets(EnginePID,Username,N,UserFeed)->
  ChanceToSend = rand:uniform(10),
  if
    ChanceToSend == 1 -> % send basic tweet
      userSendTweet(EnginePID,Username,"MYTWEET");
    ChanceToSend == 2 -> % send Retweet
      if
        length(UserFeed) == 0 ->
          ok;
        true ->
          OriginalTweet = lists:nth(rand:uniform(length(UserFeed)),UserFeed), % pick random tweet to reTweet
          userSendRetweet(EnginePID,Username,OriginalTweet)
      end;
    ChanceToSend == 3-> % send tweet with a random hashTag
      userSendTweet(EnginePID,Username,"MYTWEET#"++numberToString(rand:uniform(length(UserFeed)+12)));
    true -> % do nothing most people who are active don't tweet
      ok
  end,
  userSendRandomTweets(EnginePID,Username,N-1,UserFeed).
%userActions
userSubscribeTo(EnginePID,UserOrHashTag,ID)-> % done
  EnginePID ! {subscribeTo,self(),UserOrHashTag,ID}.% UserOrHashTag = 'user' for user and 'hash' for hashTag

userSendTweet(EnginePID,UserName,Tweet)-> % done
  % tweets can be original or a retweet(gotten from userfeed) left for user to decide as engine see's no difference between them
  EnginePID ! {distributeTweet,{UserName,Tweet}}.

userSendRetweet(EnginePID,Username,OriginalTweet)->% done originalTweet = {OriginalSender, OriginalMessage}
  {OSender,OMessage} = OriginalTweet,
  Tweet = OMessage ++ ("@" ++ OSender), % retweets you send the original tweet as your own but at end you mention original poster
  EnginePID ! {distributeTweet,{Username,Tweet}}.
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

engineActor(UserDatabase,HashTagDatabase,NumberOfSpecialTweets,NumOfTweets,DistributedTo)->
  receive
    die -> % done
      ok;
      % loop ends here don't call self
    {endSimulation,SuperVisor}-> % done
      % loop ends here send simulator final engine stats
      SuperVisor ! {results,NumberOfSpecialTweets,NumOfTweets,DistributedTo},
      killActorInUserDatabase(UserDatabase);
    {setupSim, ListOfUserNames,ZipfAlpha}-> % done Only called once at setup
      %set each user up to have all its subs based on zipF distribution
      NewUserDatabase = engineSimSetup(UserDatabase,ListOfUserNames,ListOfUserNames,ZipfAlpha,NumberOfSpecialTweets,1),
      % after setup set NumberOfSpecialTweets to 0 as it starts as Total USers for above function to work
      engineActor(NewUserDatabase,HashTagDatabase,0,NumOfTweets,DistributedTo);
    {registerUser,Username}-> % done
      PID =  spawn(twitterEngine,userActor,[Username,[],self()]), %TotalNumberOfUsers used for tweet simulation...
      engineActor(maps:put(Username,{[],[],PID},UserDatabase),HashTagDatabase,NumberOfSpecialTweets,NumOfTweets,DistributedTo);
    {logOn,Username,PID}-> % done
      {_,_,ActorPID} = maps:get(Username,UserDatabase),
      PID ! ActorPID,
      engineActor(UserDatabase,HashTagDatabase,NumberOfSpecialTweets,NumOfTweets,DistributedTo);
    {subscribeTo,UserPID,user,UserToSubscribeTo}-> % done
      {Tweets,Subs,SubActorPID} = maps:get(UserToSubscribeTo,UserDatabase),
      %Send to user all tweets sent from before subed
      UserPID ! {tweets,Tweets},
      engineActor(maps:put(UserToSubscribeTo,{Tweets,[UserPID|Subs],SubActorPID},UserDatabase),HashTagDatabase,NumberOfSpecialTweets,NumOfTweets,DistributedTo);
    {subscribeTo,UserPID,hash,HashToSubscribeTo}-> % done
      {Tweets,Subs} = maps:get(HashToSubscribeTo,HashTagDatabase),
      %Send to user all tweets sent from before subed
      UserPID ! {tweets,Tweets},
      engineActor(UserDatabase,maps:put(HashToSubscribeTo,{Tweets,[UserPID|Subs]},HashTagDatabase),NumberOfSpecialTweets,NumOfTweets,DistributedTo);
    {distributeTweet,FullTweet}-> % done
      {Tweeter,Tweet} = FullTweet,
      % Tweets sent as {User,message} tweet will be just the message
      ListOfHashTags = tweetSpecialCharParse(Tweet,"#"),
      ListOfMentions = tweetSpecialCharParse(Tweet,"@"),
      spawn(twitterEngine,processHashTags,[ListOfHashTags,FullTweet,self()]),
      spawn(twitterEngine,processMentions,[ListOfMentions,FullTweet,UserDatabase]),
      {Tweets,Subs,SubActorPID} = maps:get(Tweeter,UserDatabase), % send to followers
      spawn(twitterEngine,distributerActor,[Subs,FullTweet]),% send tweet with distributer in own process
      % add to Tweeter's Tweet list
      engineActor(maps:put(Tweeter,{[FullTweet | Tweets],Subs,SubActorPID},UserDatabase),HashTagDatabase,NumberOfSpecialTweets + length(ListOfMentions),NumOfTweets + 1,DistributedTo + length(Subs));
    {distributeHashTag,HashTag,Tweet}-> % done
      {Tweets,Subs} = try maps:get(HashTag,HashTagDatabase)
      catch _:_ -> {[],[]} end,
      spawn(twitterEngine,distributerActor,[Subs,Tweet]),
      HashTagDatabase2 = maps:put(HashTag,{[Tweet|Tweets],Subs},HashTagDatabase),
      engineActor(UserDatabase,HashTagDatabase2,NumberOfSpecialTweets+1,NumOfTweets,DistributedTo + length(Subs))
  end.

engineSimSetup(UserDatabase,[],_,_,_,_)-> % set each user to have a num of subs based on zipf distribution
  UserDatabase;
engineSimSetup(UserDatabase,ListOfUsers,CompleteListOfUsers,ZipfAlpha,TotalNumberOfUsers,Rank)->
  NumberOfSubsUserShouldHave = trunc(zipF(ZipfAlpha,TotalNumberOfUsers,Rank,0)),% convert float from zipf to int
  {ListOfSubs,_} = lists:split(NumberOfSubsUserShouldHave,CompleteListOfUsers), % returns first n number of users from
  % they will become the followers of this user
  {UserTweets,_,UserActorPID} = maps:get(hd(ListOfUsers),UserDatabase),
  % update the user's sub count
  UserActorPID ! {simStart, length(ListOfSubs)},
  ListOfSubPIDS = convertUserNamesToPids(UserDatabase,ListOfSubs,[]),
  engineSimSetup(maps:put(hd(ListOfUsers),{UserTweets,ListOfSubPIDS,UserActorPID},UserDatabase),tl(ListOfUsers),CompleteListOfUsers,ZipfAlpha,TotalNumberOfUsers,Rank+1).

processHashTags([],_,_)->
  ok;
processHashTags(ListOfHashTags,Tweet,EnginePID)-> % sends hashtags to engine to distribute/store
  EnginePID! {distributeHashTag,hd(ListOfHashTags),Tweet},
  processHashTags(tl(ListOfHashTags),Tweet,EnginePID).

processMentions([],_,_)->
  ok;
processMentions(ListOfMentions,Tweet,UserDatabase)-> % sends tweet to users mentioned in tweet
  {_,_,ActorPID} = try maps:get(string:sub_string(hd(ListOfMentions),2),UserDatabase) % substring to drop @ char from front...
  catch _:_ -> {[],[],null} end, % incase where actor does not exist
  if
    ActorPID == null-> % no actor with that name don't do anything
      ok;
    true ->
      ActorPID ! {tweet, Tweet}
  end,
  processMentions(tl(ListOfMentions),Tweet,UserDatabase).

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

actorKiller([])-> %Tell actors to kill themselves
  ok;
actorKiller(ListOfActors)->
  PID = hd(ListOfActors),
  PID ! die,
  actorKiller(tl(ListOfActors)).

killActorInUserDatabase(UserDatabase)->
      UserList = [PIDs || {_,{_,_,PIDs}} <- maps:to_list(UserDatabase)],
      actorKiller(UserList). % kill the user actors

specialCharSearch([],_,ListOfHashTags)->
  ListOfHashTags;
specialCharSearch(List_of_Tweet,Char,ListOfHashTags)->
    Split_string = hd(List_of_Tweet),
    StartOfHash = string:find(Split_string,Char),
    if
      StartOfHash == nomatch->
        specialCharSearch(tl(List_of_Tweet),Char,ListOfHashTags);
      true ->
        Matched_tag = string:equal(Char,string:sub_string(StartOfHash,1, 1)),
        if
          Matched_tag->
            %io:fwrite("HashTag is ~p ~n",[StartOfHash]),
            specialCharSearch(tl(List_of_Tweet),Char,[StartOfHash|ListOfHashTags]);
            true->
              specialCharSearch(tl(List_of_Tweet),Char,ListOfHashTags)
        end
end.

tweetSpecialCharParse(Tweet,SpecialChar)-> % returns list of hashtags if "#" or mentions if "@"...
  List_of_Tweet = string:split(Tweet, " ",all),
  specialCharSearch(List_of_Tweet,SpecialChar,[]).