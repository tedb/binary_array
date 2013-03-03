-module(binary_array).
-author("Ted Behling; https://github.com/tedb").
-export([new/0, new/1, new/2, position/2, position_binary_search/2, nth/2, sort/1, insert/2, size/1, length/1, to_list/1]).
-record(?MODULE, {element_size, bin}).
% we need to not import erlang:length/1
-compile({no_auto_import,[length/1]}).
-ifdef(TEST).
-export([large_binary_position_test_generate/1]).
-endif.

% binary_array serves the use case where a list of fixed-length binaries needs to be held in memory and the position of elements queried,
% but a list of short binaries is memory-inefficient.

% Possible future change: Currently, a binary_array is one large binary; consider changing to be a dict of prefixes with the binary split up into buckets for faster searching

% Returns a new binary_array for later use, defaulting to 10-byte elements and an empty binary
new() ->
  new(10, <<>>).
new(ElementSize) ->
  new(ElementSize, <<>>).
new(ElementSize, Bin) when is_integer(ElementSize), is_binary(Bin), erlang:size(Bin) rem ElementSize =:= 0 ->
  #?MODULE{element_size = ElementSize, bin = Bin}.

% Returns the first 0-based numeric position in the array where key was found, or nomatch
position(Element, #?MODULE{element_size = ElementSize, bin = Bin} = _BinaryArray) when is_binary(Element), erlang:size(Element) =:= ElementSize ->
  % binary:matches returns a list of all matches, as list of {Offset, Length}
  % we need to make sure we only get matches w/ the correct length and offset multiple (since we have no delimiters)
  % Offset is in bytes, need to convert it to an element position
  AllMatches = binary:matches(Bin, Element),
  case [Offset div ElementSize || {Offset, _Length} <- AllMatches, Offset rem ElementSize == 0] of
    [] ->
      nomatch;
    GoodMatchPositions ->
      [GoodMatchPosition|_Rest] = GoodMatchPositions,
      GoodMatchPosition
  end.

% same as position, but uses a binary search algorithm instead of sequential search
% assumes the binary_array is already sorted!
% this should perform in much faster O(log(n)) time instead of O(n) time
position_binary_search(Element, #?MODULE{element_size = ElementSize, bin = Bin} = BinaryArray) when is_binary(Element), erlang:size(Element) =:= ElementSize ->
  do_binary_search(Element, Bin, 0, length(BinaryArray) - 1).

% Returns the position of Element with 0 offset
% Match base case of an empty binary
do_binary_search(_Element, Bin, _StartPos, _EndPos) when Bin =:= <<>> ->
  nomatch;
do_binary_search(Element, Bin, _StartPos, _EndPos) when Element =:= Bin ->
  0;
%do_binary_search(_Element, _Bin, StartPos, EndPos) when StartPos =:= EndPos ->
%  nomatch;
do_binary_search(Element, Bin, StartPos, EndPos) ->
  %io:format(user, "do_binary_search(El: ~p, Bin: ~p, Start: ~p, End: ~p)~n", [Element, Bin, StartPos, EndPos]),
  MidpointPos = (StartPos + EndPos) div 2,
  ElementSize = erlang:size(Element),
  MidpointElement = binary:part(Bin, {ElementSize * MidpointPos, ElementSize}),
  if
    Element =:= MidpointElement ->
      %io:format(user, "element(~p) == midpoint(~p) at midpointpos ~p~n", [Element, MidpointElement, MidpointPos]),
      MidpointPos;
    StartPos =:= EndPos, Element =/= MidpointElement->
      nomatch;
    Element < MidpointElement ->
      %io:format(user, "element(~p) < midpoint(~p) at midpointpos ~p~n", [Element, MidpointElement, MidpointPos]),
      do_binary_search(Element, Bin, StartPos, MidpointPos - 1);
    true ->
      %io:format(user, "element(~p) > midpoint(~p) at midpointpos ~p~n", [Element, MidpointElement, MidpointPos]),
      do_binary_search(Element, Bin, MidpointPos + 1, EndPos)
  end.

% Returns the binary at the given position
% NOTE this is different from lists:nth/2 in that this is zero based!
nth(Position, #?MODULE{element_size = ElementSize, bin = Bin} = _BinaryArray) when is_integer(Position) ->
  binary:part(Bin, {ElementSize * Position, ElementSize}).

% Returns a sorted binary_array
% This is a very naive algorithm; this will temporarily use about 4x or more the memory during the sort, and is probably slow
% It is recommended to call erlang:garbage_collect after invoking this
sort(BinaryArray) ->
  SortedBin = erlang:list_to_binary( lists:sort( to_list(BinaryArray) ) ),
  BinaryArray#?MODULE{bin = SortedBin}.

% Returns a binary_array with Element appended at the end
insert(NewElement, #?MODULE{element_size = ElementSize, bin = Bin} = BinaryArray) when is_binary(NewElement), erlang:size(NewElement) == ElementSize ->
  BinaryArray#?MODULE{bin = <<Bin/binary, NewElement/binary>>}.

% Returns the number of elements in the binary_array (same as length)
size(BinaryArray) ->
  length(BinaryArray).

% Returns the number of elements in the binary_array (same as size)
length(#?MODULE{element_size = ElementSize, bin = Bin} = _BinaryArray) ->
  erlang:size(Bin) div ElementSize.

% Returns all the elements as a list of binaries
to_list(#?MODULE{element_size = ElementSize, bin = Bin} = _BinaryArray) ->
  [ X || <<X:ElementSize/binary>> <= Bin ].

% Start tests - run tests with "eunit:test(binary_array)" or "rebar eunit"

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
  ?assertEqual({?MODULE,10,<<>>}, ?MODULE:new()),
  ?assertEqual({?MODULE,5,<<>>}, ?MODULE:new(5)),
  ?assertEqual({?MODULE,7,<<"abcdefghijklmn">>}, ?MODULE:new(7, <<"abcdefghijklmn">>)),
  ok.

empty_test() ->
  % initialize 3-byte array
  B = ?MODULE:new(3),
  ?assertEqual(nomatch, ?MODULE:position(<<"abc">>, B)),
  ok.

insert_position_test() ->
  % initialize 3-byte array
  B = binary_array:new(3),
  % insert first element... yields abc at position 0
  B2 = ?MODULE:insert(<<"abc">>, B),
  ?assertEqual({?MODULE, 3, <<"abc">>}, B2),
  ?assertEqual(0, ?MODULE:position(<<"abc">>, B2)),
  % insert second element... yields abcdef, with def at position 1
  B3 = ?MODULE:insert(<<"def">>, B2),
  ?assertEqual(1, ?MODULE:position(<<"def">>, B3)),
  % insert third element... yields abcdefghi, with def still at position 1 and ghi at position 2
  B4 = ?MODULE:insert(<<"ghi">>, B3),
  ?assertEqual(1, ?MODULE:position(<<"def">>, B4)),
  ?assertEqual(2, ?MODULE:position(<<"ghi">>, B4)),
  % make sure all is well behind the scenes
  ?assertEqual({?MODULE,3,<<"abcdefghi">>}, B4),
  % make sure that if we query the position of a binary whose value straddles two others, it doesn't match
  ?assertEqual(nomatch, ?MODULE:position(<<"cde">>, B4)),
  % make sure querying for a non-existent binary returns nomatch
  ?assertEqual(nomatch, ?MODULE:position(<<"zzz">>, B4)),
  ok.

position_binary_search_test() ->
  % initialize 3-byte array
  B = binary_array:new(3),
  ?assertEqual(nomatch, ?MODULE:position_binary_search(<<"zzz">>, B)),
  % insert first element... yields abc at position 0
  B2 = ?MODULE:insert(<<"abc">>, B),
  ?assertEqual(0, ?MODULE:position_binary_search(<<"abc">>, B2)),
  % insert second element... yields abcdef, with def at position 1
  B3 = ?MODULE:insert(<<"def">>, B2),
  ?assertEqual(1, ?MODULE:position_binary_search(<<"def">>, B3)),
  % insert third element... yields abcdefghi, with def still at position 1 and ghi at position 2
  B4 = ?MODULE:insert(<<"ghi">>, B3),
  ?assertEqual(1, ?MODULE:position_binary_search(<<"def">>, B4)),
  ?assertEqual(2, ?MODULE:position_binary_search(<<"ghi">>, B4)),
  % make sure that if we query the position of a binary whose value straddles two others, it doesn't match
  ?assertEqual(nomatch, ?MODULE:position_binary_search(<<"cde">>, B4)),
  % make sure querying for a non-existent binary returns nomatch
  ?assertEqual(nomatch, ?MODULE:position_binary_search(<<"zzz">>, B4)),

  % test a larger binary
  B5 = binary_array:new(3, <<"abcdefghijklmnopqrstuvwx">>),
  ?assertEqual(0, ?MODULE:position_binary_search(<<"abc">>, B5)),
  ?assertEqual(1, ?MODULE:position_binary_search(<<"def">>, B5)),
  ?assertEqual(2, ?MODULE:position_binary_search(<<"ghi">>, B5)),
  ?assertEqual(3, ?MODULE:position_binary_search(<<"jkl">>, B5)),
  ?assertEqual(4, ?MODULE:position_binary_search(<<"mno">>, B5)),
  ?assertEqual(5, ?MODULE:position_binary_search(<<"pqr">>, B5)),
  ?assertEqual(6, ?MODULE:position_binary_search(<<"stu">>, B5)),
  ?assertEqual(7, ?MODULE:position_binary_search(<<"vwx">>, B5)),
  ?assertEqual(nomatch, ?MODULE:position_binary_search(<<"zzz">>, B5)),
  ok.

nth_test() ->
  B = ?MODULE:new(5, <<"bcdefghijk1234567890abcdefghij">>),
  ?assertEqual(<<"bcdef">>, ?MODULE:nth(0, B)),
  ?assertEqual(<<"ghijk">>, ?MODULE:nth(1, B)),
  ?assertEqual(<<"fghij">>, ?MODULE:nth(5, B)),
  ok.

sort_list_test() ->
  % initialize 10-byte array
  B = ?MODULE:new(10, <<"bcdefghijk1234567890abcdefghij">>),
  ?assertEqual([<<"bcdefghijk">>, <<"1234567890">>, <<"abcdefghij">>], ?MODULE:to_list(B)),
  B2 = ?MODULE:sort(B),
  ?assertEqual({?MODULE, 10, <<"1234567890abcdefghijbcdefghijk">>}, B2),
  ?assertEqual([<<"1234567890">>, <<"abcdefghij">>, <<"bcdefghijk">>], ?MODULE:to_list(B2)),
  ok.

size_test() ->
  B1 = ?MODULE:new(),
  ?assertEqual(0, ?MODULE:size(B1)),

  B2 = ?MODULE:new(10, <<"bcdefghijk1234567890abcdefghij">>),
  ?assertEqual(3, ?MODULE:length(B2)),
  ok.

large_binary_position_1k_test() ->
  NbrElements = 1000,
  {Time, B} = timer:tc(?MODULE, large_binary_position_test_generate, [NbrElements]),
  io:format(user, "Large binary in large_binary_position_test_do for ~p elements generated in ~p ms~n", [NbrElements, Time/1000]),

  large_binary_position_test_do(NbrElements, B).

large_binary_position_1m_test() ->
  NbrElements = 1000 * 1000,
  {Time, B} = timer:tc(?MODULE, large_binary_position_test_generate, [NbrElements]),
  io:format(user, "Large binary in large_binary_position_test_do for ~p elements generated in ~p ms~n", [NbrElements, Time/1000]),

  large_binary_position_test_do(NbrElements, B).

large_binary_position_2m_test() ->
  NbrElements = 1000 * 1000 * 2,
  {Time, B} = timer:tc(?MODULE, large_binary_position_test_generate, [NbrElements]),
  io:format(user, "Large binary in large_binary_position_test_do for ~p elements generated in ~p ms~n", [NbrElements, Time/1000]),

  large_binary_position_test_do(NbrElements, B).

% Give us 60 seconds to test, per http://erlang.2086793.n4.nabble.com/Timeout-control-in-EUnit-td2114916.html
large_binary_position_15m_test_() -> 
  NbrElements = 1000 * 1000 * 15,
  {Time, B} = timer:tc(?MODULE, large_binary_position_test_generate, [NbrElements]),
  io:format(user, "Large binary in large_binary_position_test_do for ~p elements generated in ~p ms~n", [NbrElements, Time/1000]),

  {timeout, 60, fun() -> 
    ok = large_binary_position_test_do(NbrElements, B)
  end}. 

% Give us 60 seconds to test, per http://erlang.2086793.n4.nabble.com/Timeout-control-in-EUnit-td2114916.html
large_binary_position_30m_test_() -> 
  NbrElements = 1000 * 1000 * 30,
  {Time, B} = timer:tc(?MODULE, large_binary_position_test_generate, [NbrElements]),
  io:format(user, "Large binary in large_binary_position_test_do for ~p elements generated in ~p ms~n", [NbrElements, Time/1000]),

  {timeout, 60, fun() -> 
    ok = large_binary_position_test_do(NbrElements, B)
  end}. 

% Given a large binary, return the position of a number 90% through the list
large_binary_position_test_do(NbrElements, B) ->
  % Make sure the 10 millionth number is in the right position
  PosNumber = (NbrElements div 10) * 9,
  {Time, FoundPosition} = timer:tc(?MODULE, position_binary_search, [<<PosNumber:80/integer>>, B]),
  io:format(user, "position_binary_search for ~p elements took ~p us", [NbrElements, Time]),
  ?assertEqual(PosNumber - 1, FoundPosition),
  ok.

% Generate a binary_array of length NbrElements, size 10 bytes
large_binary_position_test_generate(NbrElements) ->
  LargeBin = lists:foldl(fun(Element, Acc) ->
    <<Acc/binary, Element:80/integer>>
  end, <<>>, lists:seq(1, NbrElements)),
  ?MODULE:new(10, LargeBin).

-endif.