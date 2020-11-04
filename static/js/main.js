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
