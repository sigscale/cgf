## Makefile.am
##
## Process this file with automake to produce Makefile.in

schema_DATA = $(abs_builddir)/ECS_base.json \
		$(abs_builddir)/ECS_ecs.json \
		$(abs_builddir)/ECS_event.json \
		$(abs_builddir)/ECS_network.json \
		$(abs_builddir)/ECS_service.json \
		$(abs_builddir)/ECS_user.json \
		$(abs_builddir)/ECS_Bx_CS.json \
		$(abs_builddir)/ECS_Bx_PS.json \
		$(abs_builddir)/ECS_Bx_IMS.json \
		$(abs_builddir)/ECS_Bx_SMS.json \
		$(abs_builddir)/ECS_Bx_MMS.json \
		$(abs_builddir)/ECS_Bx_MMTEL.json \
		$(abs_builddir)/ECS_Bx_CHF.json \
		$(abs_builddir)/ECS_Bx_RF.json \
		$(abs_builddir)/ECS_Bx.json

check_DATA = .validation_ok

$(abs_builddir)/%.json:	$(srcdir)/%.json
	if [ ! -e $@ ]; then \
		$(LN_S) $< $@; \
	fi

.validation_ok:
	@if ajv help > /dev/null 2>&1; then \
		if ! ajv -c ajv-formats compile -s ECS_base.json; \
         then exit 1; fi; \
		if ! ajv -c ajv-formats compile -s ECS_event.json; \
         then exit 1; fi; \
		if ! ajv compile -s ECS_network.json; \
         then exit 1; fi; \
		if ! ajv compile -s ECS_service.json; \
         then exit 1; fi; \
		if ! ajv -c ajv-formats compile -s ECS_user.json; \
         then exit 1; fi; \
		if ! ajv -c ajv-formats compile -s ECS_Bx_CS.json; \
         then exit 1; fi; \
		if ! ajv -c ajv-formats compile -s ECS_Bx_PS.json; \
         then exit 1; fi; \
		if ! ajv -c ajv-formats compile -s ECS_Bx_IMS.json; \
         then exit 1; fi; \
		if ! ajv -c ajv-formats compile -s ECS_Bx_SMS.json; \
         then exit 1; fi; \
		if ! ajv -c ajv-formats compile -s ECS_Bx_MMS.json; \
         then exit 1; fi; \
		if ! ajv -c ajv-formats compile -s ECS_Bx_MMTEL.json; \
         then exit 1; fi; \
		if ! ajv -c ajv-formats compile -s ECS_Bx_CHF.json; \
         then exit 1; fi; \
		if ! ajv -c ajv-formats compile -s ECS_Bx_RF.json; \
         then exit 1; fi; \
		if ! ajv -c ajv-formats compile \
			-r ECS_base.json \
			-r ECS_ecs.json \
			-r ECS_event.json \
			-r ECS_network.json \
			-r ECS_service.json \
			-r ECS_user.json \
			-r ECS_Bx_CS.json \
			-r ECS_Bx_PS.json \
			-r ECS_Bx_IMS.json \
			-r ECS_Bx_SMS.json \
			-r ECS_Bx_MMS.json \
			-r ECS_Bx_MMTEL.json \
			-r ECS_Bx_CHF.json \
			-r ECS_Bx_RF.json \
			-s ECS_Bx.json; \
         then exit 1; fi; \
	else \
      echo "ajv-cli needed to validate JSON Schema"; \
		echo "   install: npm npm -g install ajv-cli ajv-formats"; \
   fi

