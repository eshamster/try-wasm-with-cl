(defpackage :try-wasm-with-cl/src/js/main
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :main)
  (:import-from :try-wasm-with-cl/src/js/utils
                :pse-create))
(in-package :try-wasm-with-cl/src/js/main)

(enable-ps-experiment-syntax)

(defun.ps main ()
  (let* ((memory (new (#j.WebAssembly.Memory# (create :initial 1))))
         (global (new (#j.WebAssembly.Global# (create :value "i32"
                                                      :mutable true)
                                              0)))
         (debug (new (#j.WebAssembly.Global# (create :value "i32"
                                                     :mutable true)
                                             0))))
    ;; Cf. https://developer.mozilla.org/en-US/docs/WebAssembly/Understanding_the_text_format#webassembly_memory
    (flet ((console-log-string (offset length)
             (let* ((bytes (new (#j.Uint8Array# memory.buffer offset length)))
                    (string (chain (new (#j.TextDecoder# "utf8"))
                                   (decode bytes))))
               (console.log string))))
      (chain (#j.WebAssembly.instantiateStreaming#
              (fetch "wasm/main.wasm")
              (pse-create (:console (:log console.log
                                     :logs console-log-string)
                           :js (:mem memory
                                :global global
                                :debug debug))))
             (then (lambda (results)
                     (console.log results)
                     (console.log "--- call exported_func ---")
                     (results.instance.exports.exported_func)
                     (console.log "--- call test_memory ---")
                     (results.instance.exports.test_memory)
                     (console.log "--- call test_list ---")
                     (results.instance.exports.test_list)
                     (console.log "--- call test_log_string ---")
                     (results.instance.exports.test_log_string)
                     (console.log "--- call test_dump_memory ---")
                     (results.instance.exports.test_dump_memory)
                     (console.log "--- call test_shared_ptr ---")
                     (results.instance.exports.test_shared_ptr)
                     (console.log "--- call test_list_interpreter ---")
                     (results.instance.exports.test_list_interpreter)))))))
