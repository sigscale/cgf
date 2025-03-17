## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

check_DATA = .dialyzer_ok .common_test_ok

testmods = cgf_test_lib \
		cgf_test_event \
		cgf_api_SUITE \
		cgf_rest_api_SUITE \
		cgf_lib_SUITE \
		cgf_cdr_SUITE

DIA_ARGS := ${DIALYZER_ARGS}
CT_ARGS := ${CT_RUN_ARGS}
CT_ARGS += -spec test.spec

.dialyzer_ok:	$(addsuffix .beam, $(testmods))
	dialyzer $(DIA_ARGS) --src -I $(top_builddir)/src $(top_srcdir)/src $(top_builddir)/src

%.beam:$(top_srcdir)/test/%.erl $(top_srcdir)/include/cgf_3gpp_file.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include $<

.common_test_ok:	$(addsuffix .beam, $(testmods))
	@if [ ! -d log ]; then \
		mkdir log; \
	fi
	@if [ ! -f ct.cfg ]; then \
		cp $(top_srcdir)/test/ct.cfg .; \
	fi
	@if [ ! -f test.spec ]; then \
		cp $(top_srcdir)/test/test.spec .; \
	fi
	@if [ ! -d cgf_lib_SUITE_data ]; then \
		mkdir cgf_lib_SUITE_data; \
	fi
	@if [ ! -d cgf_cdr_SUITE_data ]; then \
		mkdir cgf_cdr_SUITE_data; \
	fi
	@if [ ! -d cgf_cdr_SUITE_data/ssh ]; then \
		mkdir cgf_cdr_SUITE_data/ssh; \
	fi
	@if [ ! -d cgf_cdr_SUITE_data/ssh/system ]; then \
		mkdir cgf_cdr_SUITE_data/ssh/system; \
		$(top_srcdir)/scripts/ssh_keys.sh --system-dir cgf_cdr_SUITE_data/ssh/system; \
	fi
	@if [ ! -d cgf_cdr_SUITE_data/ssh/user ]; then \
		mkdir cgf_cdr_SUITE_data/ssh/user; \
	fi
	@if [ ! -d cgf_cdr_SUITE_data/ssh/user/ct ]; then \
		mkdir cgf_cdr_SUITE_data/ssh/user/ct; \
		$(top_srcdir)/scripts/ssh_keys.sh --user-dir cgf_cdr_SUITE_data/ssh/user ct; \
	fi
	ct_run -no_auto_compile \
			-pa $(top_builddir)/ebin \
			$(CT_ARGS) \
			-logdir log \
			-config ct.cfg \
			-dir . \
			-erl_args -noshell -sname ct$$$$

clean-local:
	-rm -r *.beam log/*

