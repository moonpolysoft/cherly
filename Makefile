all:
	(cd c;$(MAKE))
	(cd erl;$(MAKE))

clean:
	(cd c;$(MAKE) clean)
	(cd erl;$(MAKE) clean)

test:
	(cd c; $(MAKE) test)
	(cd erl; $(MAKE) test)

install: all
	(cd c; $(MAKE) install)
	(cd erl; $(MAKE) install)

debug:
	(cd c;$(MAKE) debug)
	(cd erl;$(MAKE) debug)
