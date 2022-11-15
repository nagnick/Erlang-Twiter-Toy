-module(database).
-author("nicolas").
-import(crypto,[hash/2]).
-export([decimalShaHash/1]).
decimalShaHash(N)-> % must pass in a string
  binary:decode_unsigned(crypto:hash(sha,N)). % use sha 1 like doc says max size is unsigned 160 bit value = 1461501637330902918203684832716283019655932542976