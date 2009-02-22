Cherly
=======

Cherly (sher-lee) is an in-VM caching library for Erlang.  It is implemented as a linked in driver and the main lookup mechanism is Judy arrays.  There's almost no copying or reallocation of binary data involved, so cherly should be blindingly fast.  Cherly is designed for the needs of Dynomite, but it can live as a caching library in its own right.

Surely you can't be serious?