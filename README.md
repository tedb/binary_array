binary_array
===

binary_array is a simple Erlang library to provide memory-compact arrays of fixed-length binaries.  For the use case of a large (>1000 elements) list of short (<20 bytes) binaries all the same length, binary_array provides an alternative that greatly reduces the memory footprint of the array while providing some key operations (insert, retrieve by position, find a position, sort).  This limited set of operations fits the intended use case for this library but more could be added in the future.  Note, a binary_array is 0-based (like an Erlang array), while Erlang lists and tuples are 1-based.

Compilation
---

Compile:

```sh
$ rebar compile
```

Run unit tests:

```sh
$ EUNIT=verbose rebar eunit
```

Using with Your Rebar App
---

If your Erlang app is built using Rebar, you can easily include binary_array as a dependency by adding this to your rebar.config file:

```erlang
{deps, [
	{binary_array, ".*", {git, "git@github.com:tedb/binary_array.git"}}
]}.
```
If you already have a deps section, just add the middle line to the list.  See the [rebar documentation](https://github.com/basho/rebar/wiki/Dependency-management) for details.

Example
---

This example shows how to download, compile, and exercise the binary_array library.  This assumes you already have [rebar](https://github.com/basho/rebar) (an Erlang build tool) installed on your path.

The $ represents your Bash prompt, and the N> represents your Erlang prompt.

```sh
$ git clone git@github.com:tedb/binary_array.git
Cloning into 'binary_array'...

$ cd binary_array

$ rebar compile
==> binary_array (compile)
Compiled src/binary_array.erl

$ rebar eunit
==> binary_array (eunit)
Compiled src/binary_array.erl
  All 5 tests passed.

$ erl -pa ebin
Erlang R15B02 [...etc...]
Eshell V5.9.2  (abort with ^G)
```

```erlang
1> B = binary_array:new(5).
{binary_array,5,<<>>}

2> B2 = binary_array:insert(<<"Abcde">>, B).
{binary_array,5,<<"Abcde">>}

3> B3 = binary_array:insert(<<"_zzzz">>, B2).
{binary_array,5,<<"Abcde_zzzz">>}

4> B4 = binary_array:insert(<<"01234">>, B3).
{binary_array,5,<<"Abcde_zzzz01234">>}

5> PositionUnsorted = binary_array:position(<<"Abcde">>, B4).
0

6> BSorted = binary_array:sort(B4).
{binary_array,5,<<"01234Abcde_zzzz">>}

7> PositionSorted = binary_array:position(<<"Abcde">>, BSorted).
1

8> ListOfBinaries = binary_array:to_list(BSorted).
[<<"01234">>,<<"Abcde">>,<<"_zzzz">>]

9> init:stop().
ok

$
```

License
---

GPLv2

Contributing
---

Please fork the Github repo and send me a pull request with any changes you would like me to consider including.

