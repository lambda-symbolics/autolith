(in-package #:autolith)

;;;; -- NeMo Relay C ABI --

(cffi:define-foreign-library nemo-relay-ffi
  (:darwin (:or "libnemo_relay_ffi.dylib" "libnemo_relay_ffi"))
  (:unix (:or "libnemo_relay_ffi.so" "libnemo_relay_ffi"))
  (t (:default "libnemo_relay_ffi")))

(cffi:defctype nemo-relay-status :int32)

(cffi:defcstruct nemo-relay-metric-measurement
  (name :pointer)
  (kind :int32)
  (value-type :int32)
  (u64-value :uint64)
  (i64-value :int64)
  (f64-value :double)
  (unit :pointer)
  (description :pointer)
  (attributes-json :pointer)
  (boundaries :pointer)
  (boundaries-len :size))

;;;; -- Runtime, Errors, and Plugin Configuration --

(cffi:defcfun ("nemo_relay_flush_subscribers" %nemo-relay-flush-subscribers)
    nemo-relay-status)
(cffi:defcfun ("nemo_relay_last_error" %nemo-relay-last-error) :string)
(cffi:defcfun ("nemo_relay_string_free" %nemo-relay-string-free) :void
  (value :pointer))

(cffi:defcfun ("nemo_relay_set_last_error_message"
               %nemo-relay-set-last-error-message)
    :void
  (message :pointer))

(cffi:defcfun ("nemo_relay_observability_plugin_kind"
               %nemo-relay-observability-plugin-kind)
    :pointer)
(cffi:defcfun ("nemo_relay_observability_default_config_json"
               %nemo-relay-observability-default-config-json)
    nemo-relay-status
  (out-json :pointer))
(cffi:defcfun ("nemo_relay_observability_component_spec_json"
               %nemo-relay-observability-component-spec-json)
    nemo-relay-status
  (config-json :pointer)
  (enabled :bool)
  (out-json :pointer))

(cffi:defcfun ("nemo_relay_validate_plugin_config"
               %nemo-relay-validate-plugin-config)
    nemo-relay-status
  (config-json :pointer)
  (out-json :pointer))
(cffi:defcfun ("nemo_relay_initialize_plugins"
               %nemo-relay-initialize-plugins)
    nemo-relay-status
  (config-json :pointer)
  (out-json :pointer))
(cffi:defcfun ("nemo_relay_initialize_with_dynamic_plugins"
               %nemo-relay-initialize-with-dynamic-plugins)
    nemo-relay-status
  (config-json :pointer)
  (dynamic-plugins-json :pointer)
  (out-activation :pointer)
  (out-report-json :pointer))
(cffi:defcfun ("nemo_relay_plugin_activation_clear"
               %nemo-relay-plugin-activation-clear)
    nemo-relay-status
  (activation :pointer))
(cffi:defcfun ("nemo_relay_plugin_activation_free"
               %nemo-relay-plugin-activation-free)
    :void
  (activation-slot :pointer))
(cffi:defcfun ("nemo_relay_clear_plugin_configuration"
               %nemo-relay-clear-plugin-configuration)
    nemo-relay-status)
(cffi:defcfun ("nemo_relay_active_plugin_report_json"
               %nemo-relay-active-plugin-report-json)
    nemo-relay-status
  (out-json :pointer))
(cffi:defcfun ("nemo_relay_list_plugin_kinds_json"
               %nemo-relay-list-plugin-kinds-json)
    nemo-relay-status
  (out-json :pointer))

;;;; -- Scope Stacks and Propagation --

(cffi:defcfun ("nemo_relay_scope_stack_create"
               %nemo-relay-scope-stack-create)
    nemo-relay-status
  (out :pointer))
(cffi:defcfun ("nemo_relay_scope_stack_create_from_propagation_json"
               %nemo-relay-scope-stack-create-from-propagation-json)
    nemo-relay-status
  (context-json :pointer)
  (out :pointer))
(cffi:defcfun ("nemo_relay_scope_stack_set_thread"
               %nemo-relay-scope-stack-set-thread)
    nemo-relay-status
  (stack :pointer))
(cffi:defcfun ("nemo_relay_scope_stack_capture_thread"
               %nemo-relay-scope-stack-capture-thread)
    nemo-relay-status
  (out :pointer))
(cffi:defcfun ("nemo_relay_scope_stack_restore_thread"
               %nemo-relay-scope-stack-restore-thread)
    nemo-relay-status
  (binding :pointer))
(cffi:defcfun ("nemo_relay_scope_stack_active"
               %nemo-relay-scope-stack-active)
    :bool)
(cffi:defcfun ("nemo_relay_scope_stack_free"
               %nemo-relay-scope-stack-free)
    :void
  (stack :pointer))
(cffi:defcfun ("nemo_relay_capture_propagation_context_json"
               %nemo-relay-capture-propagation-context-json)
    nemo-relay-status
  (out :pointer))
(cffi:defcfun ("nemo_relay_capture_propagation_context_with_root_json"
               %nemo-relay-capture-propagation-context-with-root-json)
    nemo-relay-status
  (root-uuid :pointer)
  (out :pointer))
(cffi:defcfun ("nemo_relay_capture_traceparent"
               %nemo-relay-capture-traceparent)
    nemo-relay-status
  (out :pointer))
(cffi:defcfun ("nemo_relay_propagation_context_to_traceparent"
               %nemo-relay-propagation-context-to-traceparent)
    nemo-relay-status
  (context-json :pointer)
  (out :pointer))

;;;; -- Scopes, Events, Metrics, and Manual Lifecycles --

(cffi:defcfun ("nemo_relay_get_handle" %nemo-relay-get-handle)
    nemo-relay-status
  (out :pointer))
(cffi:defcfun ("nemo_relay_push_scope" %nemo-relay-push-scope)
    nemo-relay-status
  (name :pointer)
  (scope-type :int32)
  (parent :pointer)
  (attributes :uint32)
  (data-json :pointer)
  (metadata-json :pointer)
  (input-json :pointer)
  (timestamp-unix-micros :pointer)
  (out :pointer))
(cffi:defcfun ("nemo_relay_pop_scope" %nemo-relay-pop-scope)
    nemo-relay-status
  (handle :pointer)
  (output-json :pointer)
  (metadata-json :pointer)
  (timestamp-unix-micros :pointer))
(cffi:defcfun ("nemo_relay_event" %nemo-relay-event)
    nemo-relay-status
  (name :pointer)
  (parent :pointer)
  (data-json :pointer)
  (metadata-json :pointer)
  (timestamp-unix-micros :pointer))
(cffi:defcfun ("nemo_relay_event_v2" %nemo-relay-event-v2)
    nemo-relay-status
  (name :pointer)
  (parent :pointer)
  (data-json :pointer)
  (data-schema-json :pointer)
  (metadata-json :pointer)
  (severity :pointer)
  (timestamp-unix-micros :pointer))
(cffi:defcfun ("nemo_relay_metric_json" %nemo-relay-metric-json)
    nemo-relay-status
  (name :pointer)
  (parent :pointer)
  (measurements-json :pointer)
  (metadata-json :pointer)
  (timestamp-unix-micros :pointer))
(cffi:defcfun ("nemo_relay_metric" %nemo-relay-metric)
    nemo-relay-status
  (name :pointer)
  (parent :pointer)
  (measurements :pointer)
  (measurements-len :size)
  (metadata-json :pointer)
  (timestamp-unix-micros :pointer))
(cffi:defcfun ("nemo_relay_scope_handle_free"
               %nemo-relay-scope-handle-free)
    :void
  (handle :pointer))

(cffi:defcfun ("nemo_relay_tool_call" %nemo-relay-tool-call)
    nemo-relay-status
  (name :pointer)
  (args-json :pointer)
  (parent :pointer)
  (attributes :uint32)
  (data-json :pointer)
  (metadata-json :pointer)
  (tool-call-id :pointer)
  (timestamp-unix-micros :pointer)
  (out :pointer))
(cffi:defcfun ("nemo_relay_tool_call_end" %nemo-relay-tool-call-end)
    nemo-relay-status
  (handle :pointer)
  (result-json :pointer)
  (data-json :pointer)
  (metadata-json :pointer)
  (timestamp-unix-micros :pointer))
(cffi:defcfun ("nemo_relay_tool_handle_free"
               %nemo-relay-tool-handle-free)
    :void
  (handle :pointer))

(cffi:defcfun ("nemo_relay_llm_call" %nemo-relay-llm-call)
    nemo-relay-status
  (name :pointer)
  (native-json :pointer)
  (parent :pointer)
  (attributes :uint32)
  (data-json :pointer)
  (metadata-json :pointer)
  (model-name :pointer)
  (timestamp-unix-micros :pointer)
  (out :pointer))
(cffi:defcfun ("nemo_relay_llm_call_end" %nemo-relay-llm-call-end)
    nemo-relay-status
  (handle :pointer)
  (response-json :pointer)
  (data-json :pointer)
  (metadata-json :pointer)
  (timestamp-unix-micros :pointer))
(cffi:defcfun ("nemo_relay_llm_handle_free"
               %nemo-relay-llm-handle-free)
    :void
  (handle :pointer))

(cffi:defcfun ("nemo_relay_tool_handle_uuid"
               %nemo-relay-tool-handle-uuid)
    :pointer
  (handle :pointer))
(cffi:defcfun ("nemo_relay_tool_handle_name"
               %nemo-relay-tool-handle-name)
    :pointer
  (handle :pointer))
(cffi:defcfun ("nemo_relay_tool_handle_attributes"
               %nemo-relay-tool-handle-attributes)
    :uint32
  (handle :pointer))
(cffi:defcfun ("nemo_relay_tool_handle_parent_uuid"
               %nemo-relay-tool-handle-parent-uuid)
    :pointer
  (handle :pointer))

(cffi:defcfun ("nemo_relay_llm_handle_uuid"
               %nemo-relay-llm-handle-uuid)
    :pointer
  (handle :pointer))
(cffi:defcfun ("nemo_relay_llm_handle_name"
               %nemo-relay-llm-handle-name)
    :pointer
  (handle :pointer))
(cffi:defcfun ("nemo_relay_llm_handle_attributes"
               %nemo-relay-llm-handle-attributes)
    :uint32
  (handle :pointer))
(cffi:defcfun ("nemo_relay_llm_handle_parent_uuid"
               %nemo-relay-llm-handle-parent-uuid)
    :pointer
  (handle :pointer))

;;;; -- Exporters --

(cffi:defcfun ("nemo_relay_atif_exporter_create"
               %nemo-relay-atif-exporter-create)
    nemo-relay-status
  (session-id :pointer)
  (agent-name :pointer)
  (agent-version :pointer)
  (model-name :pointer)
  (out :pointer))
(cffi:defcfun ("nemo_relay_atif_exporter_register"
               %nemo-relay-atif-exporter-register)
    nemo-relay-status
  (exporter :pointer)
  (name :pointer))
(cffi:defcfun ("nemo_relay_atif_exporter_deregister"
               %nemo-relay-atif-exporter-deregister)
    nemo-relay-status
  (name :pointer))
(cffi:defcfun ("nemo_relay_atif_exporter_export"
               %nemo-relay-atif-exporter-export)
    nemo-relay-status
  (exporter :pointer)
  (out :pointer))
(cffi:defcfun ("nemo_relay_atif_exporter_clear"
               %nemo-relay-atif-exporter-clear)
    nemo-relay-status
  (exporter :pointer))
(cffi:defcfun ("nemo_relay_atif_exporter_free"
               %nemo-relay-atif-exporter-free)
    :void
  (exporter :pointer))

(cffi:defcfun ("nemo_relay_atof_exporter_create"
               %nemo-relay-atof-exporter-create)
    nemo-relay-status
  (output-directory :pointer)
  (mode :pointer)
  (filename :pointer)
  (out :pointer))
(cffi:defcfun ("nemo_relay_atof_exporter_create_from_json"
               %nemo-relay-atof-exporter-create-from-json)
    nemo-relay-status
  (config-json :pointer)
  (out :pointer))
(cffi:defcfun ("nemo_relay_atof_exporter_register"
               %nemo-relay-atof-exporter-register)
    nemo-relay-status
  (exporter :pointer)
  (name :pointer))
(cffi:defcfun ("nemo_relay_atof_exporter_deregister"
               %nemo-relay-atof-exporter-deregister)
    nemo-relay-status
  (name :pointer))
(cffi:defcfun ("nemo_relay_atof_exporter_force_flush"
               %nemo-relay-atof-exporter-force-flush)
    nemo-relay-status
  (exporter :pointer))
(cffi:defcfun ("nemo_relay_atof_exporter_shutdown"
               %nemo-relay-atof-exporter-shutdown)
    nemo-relay-status
  (exporter :pointer))
(cffi:defcfun ("nemo_relay_atof_exporter_path"
               %nemo-relay-atof-exporter-path)
    nemo-relay-status
  (exporter :pointer)
  (out :pointer))
(cffi:defcfun ("nemo_relay_atof_exporter_free"
               %nemo-relay-atof-exporter-free)
    :void
  (exporter :pointer))

;;;; -- OpenTelemetry Subscribers --

(cffi:defcfun ("nemo_relay_otel_subscriber_create"
               %nemo-relay-otel-subscriber-create)
    nemo-relay-status
  (otel-type :pointer)
  (transport :pointer)
  (endpoint :pointer)
  (headers-json :pointer)
  (resource-attributes-json :pointer)
  (service-name :pointer)
  (service-namespace :pointer)
  (service-version :pointer)
  (instrumentation-scope :pointer)
  (timeout-millis :uint64)
  (out :pointer))
(cffi:defcfun ("nemo_relay_otel_subscriber_create_with_projection_options"
               %nemo-relay-otel-subscriber-create-with-projection-options)
    nemo-relay-status
  (otel-type :pointer)
  (transport :pointer)
  (endpoint :pointer)
  (headers-json :pointer)
  (resource-attributes-json :pointer)
  (service-name :pointer)
  (service-namespace :pointer)
  (service-version :pointer)
  (instrumentation-scope :pointer)
  (timeout-millis :uint64)
  (mark-projection :pointer)
  (mark-exclude-names-json :pointer)
  (attribute-mappings-json :pointer)
  (out :pointer))
(cffi:defcfun ("nemo_relay_otel_subscriber_create_with_projection_options_v2"
               %nemo-relay-otel-subscriber-create-with-projection-options-v2)
    nemo-relay-status
  (otel-type :pointer)
  (transport :pointer)
  (endpoint :pointer)
  (headers-json :pointer)
  (resource-attributes-json :pointer)
  (service-name :pointer)
  (service-namespace :pointer)
  (service-version :pointer)
  (instrumentation-scope :pointer)
  (timeout-millis :uint64)
  (mark-projection :pointer)
  (mark-exclude-names-json :pointer)
  (attribute-mappings-json :pointer)
  (promote-metadata-prefixes-json :pointer)
  (out :pointer))
(cffi:defcfun ("nemo_relay_otel_subscriber_create_with_projection_options_v3"
               %nemo-relay-otel-subscriber-create-with-projection-options-v3)
    nemo-relay-status
  (otel-type :pointer)
  (transport :pointer)
  (endpoint :pointer)
  (headers-json :pointer)
  (resource-attributes-json :pointer)
  (service-name :pointer)
  (service-namespace :pointer)
  (service-version :pointer)
  (instrumentation-scope :pointer)
  (timeout-millis :uint64)
  (mark-projection :pointer)
  (mark-exclude-names-json :pointer)
  (attribute-mappings-json :pointer)
  (promote-metadata-prefixes-json :pointer)
  (completed-span-context-ttl-millis :uint64)
  (out :pointer))
(cffi:defcfun ("nemo_relay_otel_subscriber_create_with_projection_options_v4"
               %nemo-relay-otel-subscriber-create-with-projection-options-v4)
    nemo-relay-status
  (otel-type :pointer)
  (transport :pointer)
  (endpoint :pointer)
  (headers-json :pointer)
  (header-env-json :pointer)
  (resource-attributes-json :pointer)
  (service-name :pointer)
  (service-namespace :pointer)
  (service-version :pointer)
  (instrumentation-scope :pointer)
  (timeout-millis :uint64)
  (mark-projection :pointer)
  (mark-exclude-names-json :pointer)
  (attribute-mappings-json :pointer)
  (promote-metadata-prefixes-json :pointer)
  (completed-span-context-ttl-millis :uint64)
  (out :pointer))
(cffi:defcfun ("nemo_relay_otel_subscriber_register"
               %nemo-relay-otel-subscriber-register)
    nemo-relay-status
  (subscriber :pointer)
  (name :pointer))
(cffi:defcfun ("nemo_relay_otel_subscriber_deregister"
               %nemo-relay-otel-subscriber-deregister)
    nemo-relay-status
  (name :pointer))
(cffi:defcfun ("nemo_relay_otel_subscriber_force_flush"
               %nemo-relay-otel-subscriber-force-flush)
    nemo-relay-status
  (subscriber :pointer))
(cffi:defcfun ("nemo_relay_otel_subscriber_runtime_diagnostics_json"
               %nemo-relay-otel-subscriber-runtime-diagnostics-json)
    nemo-relay-status
  (subscriber :pointer)
  (out-json :pointer))
(cffi:defcfun ("nemo_relay_otel_subscriber_shutdown"
               %nemo-relay-otel-subscriber-shutdown)
    nemo-relay-status
  (subscriber :pointer))
(cffi:defcfun ("nemo_relay_otel_subscriber_free"
               %nemo-relay-otel-subscriber-free)
    :void
  (subscriber :pointer))

(cffi:defcfun ("nemo_relay_otel_log_subscriber_create"
               %nemo-relay-otel-log-subscriber-create)
    nemo-relay-status
  (transport :pointer)
  (endpoint :pointer)
  (headers-json :pointer)
  (resource-attributes-json :pointer)
  (service-name :pointer)
  (service-namespace :pointer)
  (service-version :pointer)
  (instrumentation-scope :pointer)
  (timeout-millis :uint64)
  (minimum-severity :pointer)
  (max-queue-size :uint64)
  (max-export-batch-size :uint64)
  (scheduled-delay-millis :uint64)
  (completed-span-context-ttl-millis :uint64)
  (out :pointer))
(cffi:defcfun ("nemo_relay_otel_log_subscriber_create_v2"
               %nemo-relay-otel-log-subscriber-create-v2)
    nemo-relay-status
  (transport :pointer)
  (endpoint :pointer)
  (headers-json :pointer)
  (header-env-json :pointer)
  (resource-attributes-json :pointer)
  (service-name :pointer)
  (service-namespace :pointer)
  (service-version :pointer)
  (instrumentation-scope :pointer)
  (timeout-millis :uint64)
  (minimum-severity :pointer)
  (max-queue-size :uint64)
  (max-export-batch-size :uint64)
  (scheduled-delay-millis :uint64)
  (completed-span-context-ttl-millis :uint64)
  (out :pointer))
(cffi:defcfun ("nemo_relay_otel_log_subscriber_register"
               %nemo-relay-otel-log-subscriber-register)
    nemo-relay-status
  (subscriber :pointer)
  (name :pointer))
(cffi:defcfun ("nemo_relay_otel_log_subscriber_deregister"
               %nemo-relay-otel-log-subscriber-deregister)
    nemo-relay-status
  (name :pointer))
(cffi:defcfun ("nemo_relay_otel_log_subscriber_force_flush"
               %nemo-relay-otel-log-subscriber-force-flush)
    nemo-relay-status
  (subscriber :pointer))
(cffi:defcfun ("nemo_relay_otel_log_subscriber_runtime_diagnostics_json"
               %nemo-relay-otel-log-subscriber-runtime-diagnostics-json)
    nemo-relay-status
  (subscriber :pointer)
  (out-json :pointer))
(cffi:defcfun ("nemo_relay_otel_log_subscriber_shutdown"
               %nemo-relay-otel-log-subscriber-shutdown)
    nemo-relay-status
  (subscriber :pointer))
(cffi:defcfun ("nemo_relay_otel_log_subscriber_free"
               %nemo-relay-otel-log-subscriber-free)
    :void
  (subscriber :pointer))

(cffi:defcfun ("nemo_relay_otel_metric_subscriber_create"
               %nemo-relay-otel-metric-subscriber-create)
    nemo-relay-status
  (transport :pointer)
  (endpoint :pointer)
  (headers-json :pointer)
  (resource-attributes-json :pointer)
  (service-name :pointer)
  (service-namespace :pointer)
  (service-version :pointer)
  (instrumentation-scope :pointer)
  (timeout-millis :uint64)
  (export-interval-millis :uint64)
  (temporality :pointer)
  (max-instruments :uint64)
  (cardinality-limit :uint64)
  (out :pointer))
(cffi:defcfun ("nemo_relay_otel_metric_subscriber_create_v2"
               %nemo-relay-otel-metric-subscriber-create-v2)
    nemo-relay-status
  (transport :pointer)
  (endpoint :pointer)
  (headers-json :pointer)
  (header-env-json :pointer)
  (resource-attributes-json :pointer)
  (service-name :pointer)
  (service-namespace :pointer)
  (service-version :pointer)
  (instrumentation-scope :pointer)
  (timeout-millis :uint64)
  (export-interval-millis :uint64)
  (temporality :pointer)
  (max-instruments :uint64)
  (cardinality-limit :uint64)
  (out :pointer))
(cffi:defcfun ("nemo_relay_otel_metric_subscriber_register"
               %nemo-relay-otel-metric-subscriber-register)
    nemo-relay-status
  (subscriber :pointer)
  (name :pointer))
(cffi:defcfun ("nemo_relay_otel_metric_subscriber_deregister"
               %nemo-relay-otel-metric-subscriber-deregister)
    nemo-relay-status
  (name :pointer))
(cffi:defcfun ("nemo_relay_otel_metric_subscriber_force_flush"
               %nemo-relay-otel-metric-subscriber-force-flush)
    nemo-relay-status
  (subscriber :pointer))
(cffi:defcfun ("nemo_relay_otel_metric_subscriber_runtime_diagnostics_json"
               %nemo-relay-otel-metric-subscriber-runtime-diagnostics-json)
    nemo-relay-status
  (subscriber :pointer)
  (out-json :pointer))
(cffi:defcfun ("nemo_relay_otel_metric_subscriber_shutdown"
               %nemo-relay-otel-metric-subscriber-shutdown)
    nemo-relay-status
  (subscriber :pointer))
(cffi:defcfun ("nemo_relay_otel_metric_subscriber_free"
               %nemo-relay-otel-metric-subscriber-free)
    :void
  (subscriber :pointer))


;;;; -- Observability Accessors, Registries, and Plugin Callbacks --

(cffi:defcfun ("nemo_relay_scope_handle_uuid" %nemo-relay-scope-handle-uuid)
    :pointer
  (handle :pointer))
(cffi:defcfun ("nemo_relay_scope_handle_name" %nemo-relay-scope-handle-name)
    :pointer
  (handle :pointer))
(cffi:defcfun ("nemo_relay_scope_handle_scope_type"
               %nemo-relay-scope-handle-scope-type)
    :int32
  (handle :pointer))
(cffi:defcfun ("nemo_relay_scope_handle_attributes"
               %nemo-relay-scope-handle-attributes)
    :uint32
  (handle :pointer))
(cffi:defcfun ("nemo_relay_scope_handle_parent_uuid"
               %nemo-relay-scope-handle-parent-uuid)
    :pointer
  (handle :pointer))
(cffi:defcfun ("nemo_relay_scope_handle_data" %nemo-relay-scope-handle-data)
    :pointer
  (handle :pointer))
(cffi:defcfun ("nemo_relay_scope_handle_metadata"
               %nemo-relay-scope-handle-metadata)
    :pointer
  (handle :pointer))

(cffi:defcfun ("nemo_relay_event_uuid" %nemo-relay-event-uuid) :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_name" %nemo-relay-event-name) :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_kind" %nemo-relay-event-kind) :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_json" %nemo-relay-event-json) :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_atof_version"
               %nemo-relay-event-atof-version)
    :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_scope_category"
               %nemo-relay-event-scope-category)
    :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_category" %nemo-relay-event-category)
    :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_attributes" %nemo-relay-event-attributes)
    :uint32
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_attributes_json"
               %nemo-relay-event-attributes-json)
    :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_category_profile"
               %nemo-relay-event-category-profile)
    :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_data" %nemo-relay-event-data) :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_data_schema"
               %nemo-relay-event-data-schema)
    :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_metadata" %nemo-relay-event-metadata) :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_timestamp" %nemo-relay-event-timestamp)
    :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_input" %nemo-relay-event-input) :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_output" %nemo-relay-event-output) :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_model_name" %nemo-relay-event-model-name)
    :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_tool_call_id"
               %nemo-relay-event-tool-call-id)
    :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_parent_uuid"
               %nemo-relay-event-parent-uuid)
    :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_scope_type" %nemo-relay-event-scope-type)
    :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_annotated_request"
               %nemo-relay-event-annotated-request)
    :pointer
  (event :pointer))
(cffi:defcfun ("nemo_relay_event_annotated_response"
               %nemo-relay-event-annotated-response)
    :pointer
  (event :pointer))

(cffi:defcfun ("nemo_relay_register_subscriber"
               %nemo-relay-register-subscriber)
    nemo-relay-status
  (name :pointer)
  (callback :pointer)
  (user-data :pointer)
  (free-callback :pointer))
(cffi:defcfun ("nemo_relay_deregister_subscriber"
               %nemo-relay-deregister-subscriber)
    nemo-relay-status
  (name :pointer))

(cffi:defcfun ("nemo_relay_scope_register_subscriber"
               %nemo-relay-scope-register-subscriber)
    nemo-relay-status
  (scope-uuid :pointer)
  (name :pointer)
  (callback :pointer)
  (user-data :pointer)
  (free-callback :pointer))
(cffi:defcfun ("nemo_relay_scope_deregister_subscriber"
               %nemo-relay-scope-deregister-subscriber)
    nemo-relay-status
  (scope-uuid :pointer)
  (name :pointer))

(cffi:defcfun ("nemo_relay_register_event_metadata_injector"
               %nemo-relay-register-event-metadata-injector)
    nemo-relay-status
  (name :pointer)
  (priority :int32)
  (callback :pointer)
  (user-data :pointer)
  (free-callback :pointer))
(cffi:defcfun ("nemo_relay_deregister_event_metadata_injector"
               %nemo-relay-deregister-event-metadata-injector)
    nemo-relay-status
  (name :pointer))
(cffi:defcfun ("nemo_relay_scope_register_event_metadata_injector"
               %nemo-relay-scope-register-event-metadata-injector)
    nemo-relay-status
  (scope-uuid :pointer)
  (name :pointer)
  (priority :int32)
  (callback :pointer)
  (user-data :pointer)
  (free-callback :pointer))
(cffi:defcfun ("nemo_relay_scope_deregister_event_metadata_injector"
               %nemo-relay-scope-deregister-event-metadata-injector)
    nemo-relay-status
  (scope-uuid :pointer)
  (name :pointer))

(cffi:defcfun ("nemo_relay_register_mark_sanitize_guardrail"
               %nemo-relay-register-mark-sanitize-guardrail)
    nemo-relay-status
  (name :pointer)
  (priority :int32)
  (callback :pointer)
  (user-data :pointer)
  (free-callback :pointer))
(cffi:defcfun ("nemo_relay_deregister_mark_sanitize_guardrail"
               %nemo-relay-deregister-mark-sanitize-guardrail)
    nemo-relay-status
  (name :pointer))
(cffi:defcfun ("nemo_relay_register_scope_sanitize_start_guardrail"
               %nemo-relay-register-scope-sanitize-start-guardrail)
    nemo-relay-status
  (name :pointer)
  (priority :int32)
  (callback :pointer)
  (user-data :pointer)
  (free-callback :pointer))
(cffi:defcfun ("nemo_relay_deregister_scope_sanitize_start_guardrail"
               %nemo-relay-deregister-scope-sanitize-start-guardrail)
    nemo-relay-status
  (name :pointer))
(cffi:defcfun ("nemo_relay_register_scope_sanitize_end_guardrail"
               %nemo-relay-register-scope-sanitize-end-guardrail)
    nemo-relay-status
  (name :pointer)
  (priority :int32)
  (callback :pointer)
  (user-data :pointer)
  (free-callback :pointer))
(cffi:defcfun ("nemo_relay_deregister_scope_sanitize_end_guardrail"
               %nemo-relay-deregister-scope-sanitize-end-guardrail)
    nemo-relay-status
  (name :pointer))
(cffi:defcfun ("nemo_relay_scope_register_mark_sanitize_guardrail"
               %nemo-relay-scope-register-mark-sanitize-guardrail)
    nemo-relay-status
  (scope-uuid :pointer)
  (name :pointer)
  (priority :int32)
  (callback :pointer)
  (user-data :pointer)
  (free-callback :pointer))
(cffi:defcfun ("nemo_relay_scope_deregister_mark_sanitize_guardrail"
               %nemo-relay-scope-deregister-mark-sanitize-guardrail)
    nemo-relay-status
  (scope-uuid :pointer)
  (name :pointer))
(cffi:defcfun ("nemo_relay_scope_register_scope_sanitize_start_guardrail"
               %nemo-relay-scope-register-scope-sanitize-start-guardrail)
    nemo-relay-status
  (scope-uuid :pointer)
  (name :pointer)
  (priority :int32)
  (callback :pointer)
  (user-data :pointer)
  (free-callback :pointer))
(cffi:defcfun ("nemo_relay_scope_deregister_scope_sanitize_start_guardrail"
               %nemo-relay-scope-deregister-scope-sanitize-start-guardrail)
    nemo-relay-status
  (scope-uuid :pointer)
  (name :pointer))
(cffi:defcfun ("nemo_relay_scope_register_scope_sanitize_end_guardrail"
               %nemo-relay-scope-register-scope-sanitize-end-guardrail)
    nemo-relay-status
  (scope-uuid :pointer)
  (name :pointer)
  (priority :int32)
  (callback :pointer)
  (user-data :pointer)
  (free-callback :pointer))
(cffi:defcfun ("nemo_relay_scope_deregister_scope_sanitize_end_guardrail"
               %nemo-relay-scope-deregister-scope-sanitize-end-guardrail)
    nemo-relay-status
  (scope-uuid :pointer)
  (name :pointer))

(cffi:defcfun ("nemo_relay_register_plugin" %nemo-relay-register-plugin)
    nemo-relay-status
  (plugin-kind :pointer)
  (validate-callback :pointer)
  (register-callback :pointer)
  (user-data :pointer)
  (free-callback :pointer))
(cffi:defcfun ("nemo_relay_deregister_plugin" %nemo-relay-deregister-plugin)
    nemo-relay-status
  (plugin-kind :pointer))
(cffi:defcfun ("nemo_relay_plugin_context_register_subscriber"
               %nemo-relay-plugin-context-register-subscriber)
    nemo-relay-status
  (context :pointer)
  (name :pointer)
  (callback :pointer)
  (user-data :pointer)
  (free-callback :pointer))
(cffi:defcfun ("nemo_relay_plugin_context_register_event_metadata_injector"
               %nemo-relay-plugin-context-register-event-metadata-injector)
    nemo-relay-status
  (context :pointer)
  (name :pointer)
  (priority :int32)
  (callback :pointer)
  (user-data :pointer)
  (free-callback :pointer))
(cffi:defcfun ("nemo_relay_plugin_context_register_mark_sanitize_guardrail"
               %nemo-relay-plugin-context-register-mark-sanitize-guardrail)
    nemo-relay-status
  (context :pointer)
  (name :pointer)
  (priority :int32)
  (callback :pointer)
  (user-data :pointer)
  (free-callback :pointer))
(cffi:defcfun ("nemo_relay_plugin_context_register_scope_sanitize_start_guardrail"
               %nemo-relay-plugin-context-register-scope-sanitize-start-guardrail)
    nemo-relay-status
  (context :pointer)
  (name :pointer)
  (priority :int32)
  (callback :pointer)
  (user-data :pointer)
  (free-callback :pointer))
(cffi:defcfun ("nemo_relay_plugin_context_register_scope_sanitize_end_guardrail"
               %nemo-relay-plugin-context-register-scope-sanitize-end-guardrail)
    nemo-relay-status
  (context :pointer)
  (name :pointer)
  (priority :int32)
  (callback :pointer)
  (user-data :pointer)
  (free-callback :pointer))
