## Makefile.am
##
## Process this file with automake to produce Makefile.in

schema_DATA = $(abs_builddir)/ECS_base.json \
		$(abs_builddir)/ECS_ecs.json \
		$(abs_builddir)/ECS_event.json \
		$(abs_builddir)/ECS_network.json \
		$(abs_builddir)/ECS_service.json \
		$(abs_builddir)/ECS_3GPP_Bx.json \
		$(abs_builddir)/ECS_3GPP_Bx_MOCallRecord.json

check_DATA = .validation_ok

$(abs_builddir)/%.json:	$(srcdir)/%.json
	if [ ! -e $@ ]; then \
		$(LN_S) $< $@; \
	fi

.validation_ok:
	@if ajv help > /dev/null 2>&1; then \
		if ! ajv compile -s ECS_base.json; \
         then exit 1; fi; \
		if ! ajv compile -s ECS_event.json; \
         then exit 1; fi; \
		if ! ajv compile -s ECS_network.json; \
         then exit 1; fi; \
		if ! ajv compile -s ECS_service.json; \
         then exit 1; fi; \
		if ! ajv compile -s ECS_3GPP_Bx.json; \
         then exit 1; fi; \
		if ! ajv compile -s ECS_3GPP_Bx_MOCallRecord.json; \
         then exit 1; fi; \
	else \
      echo "ajv-cli needed to validate JSON Schema"; \
   fi

