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
         (import-object (pse-create (:console (:log console.log)
                                     :js (:mem memory
                                          :global global)))))
    (chain (#j.WebAssembly.instantiateStreaming# (fetch "wasm/main.wasm")
                                                 import-object)
           (then (lambda (results)
                   (console.log results)
                   (console.log "--- call exported_func ---")
                   (results.instance.exports.exported_func)
                   (console.log "--- call test_memory ---")
                   (results.instance.exports.test_memory)
                   (console.log "--- call test_list ---")
                   (results.instance.exports.test_list))))))

#|
const memory = new WebAssembly.Memory({initial:1});
const global = new WebAssembly.Global({value: "i32", mutable: true}, 0);
var importObject = {
    console: {
        log: console.log
    },
    js: {
        mem: memory
        global: global
    }
};

console.log("test");
WebAssembly.instantiateStreaming(fetch('wasm/main.wasm'), importObject)
    .then(results => {
        console.log("--- call exported_func ---");
        results.instance.exports.exported_func();
        console.log("--- call test_memory ---");
        results.instance.exports.test_memory();
        console.log("--- call test_list ---");
        results.instance.exports.test_list();
    });
|#
