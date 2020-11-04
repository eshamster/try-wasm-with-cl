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
         (import-object (pse-create (:console (:log console.log)
                                     :js (:mem memory)))))
    (console.log "test")
    (chain (#j.WebAssembly.instantiateStreaming# (fetch "wasm/main.wasm")
                                                 import-object)
           (then (lambda (results)
                   (console.log results)
                   (results.instance.exports.exported_func))))))

#|
const memory = new WebAssembly.Memory({initial:1});
var importObject = {
    console: {
        log: console.log
    },
    js: {
        mem: memory
    }
};

console.log("test");
WebAssembly.instantiateStreaming(fetch('wasm/main.wasm'), importObject)
    .then(results => {
        console.log(results);
        results.instance.exports.exported_func();
    });
|#
