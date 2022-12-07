-module(engine).
-export([tweetSpecialCharParse/2,userSubscribeTo/3,userSendTweet/3,userSendRetweet/3,registerUser/2,logOn/2,startEngine/1,killEngine/1,engineActor/5,distributerActor/2,userActor/3,processHashTags/3,processMentions/3]).
% point of access for client to access user account this must change to accept the websocket design
logOn(UserName,EnginePID)->%userStartUp part 2 stuff WIP pass this engine useractor to the websocket actor as api
  EnginePID ! {logOn,UserName,self()},
  receive
    PID->
      PID %% return the PID of the userActor for the username given
  end.
%users send/receive tweets. engine distributes tweets
%user stuff User can only contact the engine to accomplish tasks
userActor(Username,UserFeed,EnginePID)-> % starter
  %% add websocket functionality to this actor and engine
  % this actor will hold a socket for each user connection
  receive
    die ->
      ok;
    {simStart, NumOfSubs}-> % part of simulator startup first thing done by actor in sim
      userActor(Username,UserFeed,NumOfSubs,EnginePID)
  end.
userActor(Username,UserFeed,NumberOfSubscribers,EnginePID)-> % main userloop this will become the socket in part 2
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

engineActor(UserDatabase,HashTagDatabase,NumberOfSpecialTweets,NumOfTweets,DistributedTo)->
  % this actor will have to be able to recive connection requests through logOn and pair that connection to the appropriate user actor
  % maybe put a socketServer listen on a seperate actor that will pass the connection to this user and give it to userActors to pass information back and forth
  receive
    die -> % done
      killActorInUserDatabase(UserDatabase),
      ok;
  % loop ends here don't call self
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