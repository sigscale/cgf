## Makefile.am
##
## Process this file with automake to produce Makefile.in

ASNCFLAGS = $(ERLCFLAGS) \
		+ber +noobj

noinst_DATA = 

%.erl %.hrl:$(top_srcdir)/asn_src/%.asn1
	$(ERLC) $(ASNCFLAGS) $<

CLEANFILES = $(noinst_DATA) \
		$(einclude_DATA) \
		*.asn1db

