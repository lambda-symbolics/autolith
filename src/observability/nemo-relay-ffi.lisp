(in-package #:autolith)

;;;; -- NeMo Relay C ABI --

(cffi:define-foreign-library nemo-relay-ffi
  (:darwin (:or "libnemo_relay_ffi.dylib" "libnemo_relay_ffi"))
  (:unix (:or "libnemo_relay_ffi.so" "libnemo_relay_ffi"))
  (t (:default "libnemo_relay_ffi")))

(cffi:defctype nemo-relay-status :int32)

;;;; -- Runtime and Plugin Configuration --

(cffi:defcfun ("nemo_relay_flush_subscribers" %nemo-relay-flush-subscribers)
    nemo-relay-status)
(cffi:defcfun ("nemo_relay_last_error" %nemo-relay-last-error) :string)
(cffi:defcfun ("nemo_relay_string_free" %nemo-relay-string-free) :void
  (value :pointer))
(cffi:defcfun ("nemo_relay_set_last_error_message"
               %nemo-relay-set-last-error-message)
    :void
  (message :pointer))

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
(cffi:defcfun ("nemo_relay_scope_stack_free"
               %nemo-relay-scope-stack-free)
    :void
  (stack :pointer))
(cffi:defcfun ("nemo_relay_capture_propagation_context_json"
               %nemo-relay-capture-propagation-context-json)
    nemo-relay-status
  (out :pointer))

;;;; -- Scopes, Events, and Manual Lifecycles --

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
