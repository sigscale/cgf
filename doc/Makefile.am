## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

modhtml = cgf.html \
		cgf_app.html \
		cgf_sup.html \
		cgf_server.html \
		cgf_event.html \
		cgf_log_sup.html \
		cgf_log_server_sup.html \
		cgf_log_server.html \
		cgf_log_job_sup.html \
		cgf_log.html \
		cgf_log_codec_ecs.html \
		cgf_tools.html \
		cgf_tap.html \
		cgf_sftpd.html

doc_DATA = overview.edoc $(modhtml) \
		modules-frame.html overview-summary.html \
		index.html stylesheet.css edoc-info \
		sigscale-logo.png \
		supervision.svg \
		3gpp-architecture.svg

overview.edoc: overview.edoc-in
	sed -e 's!%VERSION%!$(VERSION)!' \
			-e 's!%DESCRIPTION%!$(PACKAGE)!' < $< > $@

%.html:$(top_srcdir)/src/%.erl	overview.edoc $(top_srcdir)/doc/supervision.edoc
	$(ERL) -noshell -run edoc_run application \
			"'$(PACKAGE_NAME)'" "'$(top_builddir)'" \
			'[{sort_functions,false},{private,true},{todo,true},{source_path,["$(top_srcdir)/src","$(top_builddir)/src"]},{subpackages,false},{def,{docsrc,"$(top_srcdir)/doc/{@?}"}},{stylesheet_file, "$(top_srcdir)/doc/sigscale.css"}]'
	for i in `ls *.html`; do \
			sed -e 's!erlang logo!sigscale logo!' \
					-e 's!href=\"http://www.erlang.org/\"!href=\"http://www.sigscale.org/\"!' \
					-e 's!erlang.png!sigscale-logo.png!' $$i > $$i.tmp; \
			mv $$i.tmp $$i; \
	done
	-cp -n $(top_srcdir)/doc/sigscale-logo.png .
	-cp -n $(top_srcdir)/doc/supervision.svg .
	-cp -n $(top_srcdir)/doc/3gpp-architecture.svg .

CLEANFILES = $(doc_DATA) $(modhtml) erlang.png

