all:
	(cd dev;$(MAKE))
	(cd src;$(MAKE))
	(cd test;$(MAKE))

clean:
	(cd dev;$(MAKE) clean)
	(cd src;$(MAKE) clean)
	(cd test;$(MAKE) clean)
