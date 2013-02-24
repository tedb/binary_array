-module(binary_array).
-author("Ted Behling; https://gist.github.com/tedb").
-export([new/0, new/1, new/2, position/2, nth/2, sort/1, insert/2, to_list/1]).
-record(?MODULE, {element_size, bin}).
-include_lib("eunit/include/eunit.hrl").

% binary_array serves the use case where a list of fixed-length binaries needs to be held in memory and the position of elements queried,
% but a list of short binaries is memory-inefficient.

% Possible future change: Currently, a binary_array is one large binary; consider changing to be a dict of prefixes with the binary split up into buckets for faster searching

% Returns a new binary_array for later use, defaulting to 10-byte elements and an empty binary
new() ->
  new(10, <<"">>).
new(ElementSize) ->
  new(ElementSize, <<"">>).
new(ElementSize, Bin) ->
  #?MODULE{element_size = ElementSize, bin = Bin}.

% Returns the first 0-based numeric position in the array where key was found, or nomatch
position(Element, BinaryArray) when size(Element) == BinaryArray#?MODULE.element_size ->
  % binary:matches returns a list of all matches, as list of {Offset, Length}
  % we need to make sure we only get matches w/ the correct length and offset multiple (since we have no delimiters)
  % Offset is in bytes, need to convert it to an element position
  AllMatches = binary:matches(BinaryArray#?MODULE.bin, Element),
  ElementSize = BinaryArray#?MODULE.element_size,
  case [Offset div ElementSize || {Offset, _Length} <- AllMatches, Offset rem ElementSize == 0] of
    [] ->
      nomatch;
    GoodMatchPositions ->
      [GoodMatchPosition|_Rest] = GoodMatchPositions,
      GoodMatchPosition
  end.

% Returns the binary at the given position
% NOTE this is different from lists:nth/2 in that it is zero based!
nth(Position, BinaryArray) when is_integer(Position) ->
  ElementSize = BinaryArray#?MODULE.element_size,
  binary:part(BinaryArray#?MODULE.bin, {ElementSize * Position, ElementSize}).

% Returns a sorted binary_array
% This is a very naive algorithm; this will temporarily use about 4x or more the memory during the sort, and is probably slow
% It is recommended to call erlang:garbage_collect after invoking this
sort(BinaryArray) ->
  SortedBin = erlang:list_to_binary( lists:sort( to_list(BinaryArray) ) ),
  BinaryArray#?MODULE{bin = SortedBin}.

% Turns a binary_array with Element appended at the end
insert(NewElement, BinaryArray) when size(NewElement) == BinaryArray#?MODULE.element_size ->
  Elements = BinaryArray#?MODULE.bin,
  BinaryArray#?MODULE{bin = <<Elements/binary, NewElement/binary>>}.

to_list(BinaryArray) ->
  ElementSize = BinaryArray#?MODULE.element_size,
  [ X || <<X:ElementSize/binary>> <= BinaryArray#?MODULE.bin ].

% Start tests - run tests with eunit:test(binary_array)

new_test() ->
  ?assertEqual({?MODULE,10,<<"">>}, ?MODULE:new()),
  ?assertEqual({?MODULE,5,<<"">>}, ?MODULE:new(5)),
  ?assertEqual({?MODULE,7,<<"abcdefghijklmn">>}, ?MODULE:new(7, <<"abcdefghijklmn">>)),
  ok.

empty_test() ->
  % initialize 3-byte array
  B = ?MODULE:new(3),
  nomatch = ?MODULE:position(<<"abc">>, B),
  ok.

insert_position_test() ->
  % initialize 3-byte array
  B = binary_array:new(3),
  % insert first element... yields abc at position 0
  B2 = ?MODULE:insert(<<"abc">>, B),
  ?assertEqual({?MODULE, 3, <<"abc">>}, B2),
  ?assertEqual(0, ?MODULE:position(<<"abc">>, B2)),
  % insert second element... yields abcdef, with def at position 3
  B3 = ?MODULE:insert(<<"def">>, B2),
  ?assertEqual(1, ?MODULE:position(<<"def">>, B3)),
  % insert third element... yields abcdefghi, with def still at position 3 and ghi at position 6
  B4 = ?MODULE:insert(<<"ghi">>, B3),
  ?assertEqual(1, ?MODULE:position(<<"def">>, B4)),
  ?assertEqual(2, ?MODULE:position(<<"ghi">>, B4)),
  % finally, make sure all is well behind the scenes
  ?assertEqual({?MODULE,3,<<"abcdefghi">>}, B4),
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