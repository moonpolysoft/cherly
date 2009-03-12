Cherly
=======

Cherly (sher-lee) is an in-VM caching library for Erlang.  It is implemented as a linked in driver and the main lookup mechanism is Judy arrays.  There's almost no copying or reallocation of binary data involved, so cherly should be blindingly fast.  Cherly is designed for the needs of Dynomite, but it can live as a caching library in its own right.

Surely you can't be serious?

How To Use
=======

        {ok, C} = cherly:start(128),      %start takes 1 argument, the maximum cache size in bytes
        cherly:put(C, "key", <<"value">>), %keys are strings and values are binaries or lists of binaries
        case cherly:get(C, "key") of
          not_found -> io:format("don't got it!~n");
          {ok, Value} -> io:format("we got our value: ~p~n", [Value])
        end,
        cherly:put(C, "key2", [<<"value1">>, <<"value2">>]),
        {ok, [Value1, Value2]} = cherly;get(C, "key2"),
        io:format("got back out ~p~n", [[Value1, Value2]]).
        
Frequently Used Questions
=======

* Why not use ETS to do this?

  > ETS is a fine hashtable, and plenty fast.  However building an LRU around it proved surprisingly difficult.  After playing around a bunch, the best way to go about it was to put the entire thing in a port driver.
  
* How does the LRU work?

  > Right now it's a doubly linked list.  Lookups cause a node in the list to get unlinked and sent to the head of the list.  Ejecting items off of the LRU's tail happens when new keys come in and the cache is out of space.  All these operations are O(1) which is rad.
  
* Why does cherly just deal with binaries?

  > Cherly is very specific in its designed use case: storing possibly large binary values in memory with as little overhead as possible.  Since I'm expecting that some binaries could be up to 1MB in size and possibly more, causing the ERTS to copy the actual bytes in and out of the port driver is unacceptable.  Therefore cherly uses the outputv callback and the driver_outputv function in order to transfer binaries by reference instead of by value.  This means that cherly does not need a slab allocator or anything silly like that.  The binaries are already allocated.  Cherly simply needs to increment the reference count and hang on to it for as long as needed.

Dependencies
=======

* Runtime
  
        Judy >= 1.0.4: http://judy.sourceforge.net/
        Erlang >= R12B-3

* Build (test)
  
        Check >= 0.9.5 http://check.sourceforge.net/


Installation
========

* From source:

        ./configure && make
        sudo make install
        
* From a release tarball:

        sudo mv cherly-0.0.1-osx.tar.gz /usr/lib/erlang/releases/
        erl -boot start_sasl -eval "release_handler:unpack_release(\"cherly-0.0.1-osx\")." -s erlang halt
        