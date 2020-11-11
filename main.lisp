(defpackage :try-wasm-with-cl/main
  (:nicknames :try-wasm-with-cl)
  (:use :try-wasm-with-cl/server
        :try-wasm-with-cl/src/wasm/sample ;; to load
        )
  (:export :start-server
           :stop-server))
