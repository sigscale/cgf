## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

check_DATA = .dialyzer_ok .common_test_ok

testmods = cgf_test_lib \
		cgf_lib_SUITE

DIA_ARGS =
CT_ARGS =

.dialyzer_ok:	$(addsuffix .beam, $(testmods))
	dialyzer $(DIA_ARGS) --src $(top_srcdir)/src -I $(top_builddir)/src

%.beam:$(top_srcdir)/test/%.erl
	$(ERLC) $(ERLCFLAGS) $<

.common_test_ok:	$(addsuffix .beam, $(testmods))
	@if [ ! -d log ]; then mkdir log; fi
	@if [ ! -f ct.cfg ]; then cp $(top_srcdir)/test/ct.cfg .; fi
	@if [ ! -f test.spec ]; then cp $(top_srcdir)/test/test.spec .; fi
	@if [ ! -d cgf_lib_SUITE_data ]; then mkdir cgf_lib_SUITE_data; fi
	ct_run -no_auto_compile \
			-pa $(top_builddir)/ebin \
			$(CT_ARGS) \
			-logdir log -config ct.cfg -spec test.spec -dir . \
			-erl_args -noshell -sname $(LOGNAME)-ct

clean-local:
	-rm -r *.beam log/*

