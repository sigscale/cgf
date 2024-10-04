# Validation testing of generated JSON

For any logged event the JSON object may be validated against the
JSON Schemas using [ajv-cli](https://github.com/ajv-validator/ajv-cli)
using this command line:
```bash
ajv validate --strict=false --verbose --all-errors -c ajv-formats \
      -r ECS_base.json \
      -r ECS_ecs.json \
      -r ECS_event.json \
      -r ECS_network.json \
      -r ECS_service.json \
      -r ECS_user.json \
      -r ECS_data_stream.json \
      -r ECS_Bx_CS.json \
      -r ECS_Bx_PS.json \
      -r ECS_Bx_IMS.json \
      -r ECS_Bx_SMS.json \
      -r ECS_Bx_MMS.json \
      -r ECS_Bx_MMTEL.json \
      -r ECS_Bx_CHF.json \
      -r ECS_Bx_TAP.json \
      -r ECS_Bx_ABMF.json \
      -r ECS_Bx_RF.json \
      -s ECS_Bx.json \
      -d event.json
```
