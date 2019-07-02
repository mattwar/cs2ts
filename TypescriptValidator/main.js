/**
 * A utility to transpile typscript code provided in command line argument, and evaluate it.
 * If code has errors or evaluation throws, %ERRORLEVEL% will be non-zero.
 * Usage example: 
 * node main.js "const s: string = 'it works!'; console.log(s)"
 */

const ts = require('typescript');
require('colors');

// expecting the typescript code to be in argv[2] since:
// argv[0] is path to node.exe.
// argv[1] is this script filename.
const source = process.argv[2];

if (!source) {
    console.log('Usage: node main.js "<typescript code>"'.red);
    process.exit(1);
}

console.log('TYPESCRIPT CODE'.cyan);
console.log(source);

const result = ts.transpileModule(source, {
    compilerOptions: {
        module: ts.ModuleKind.ES2015,
    }, reportDiagnostics: true
});

if (result.diagnostics && result.diagnostics.length > 0) {
    console.error(`start: ${result.diagnostics[0].start}`.red);
    console.error(`length: ${result.diagnostics[0].length}`.red);
    console.error(result.diagnostics[0].messageText.red);
    process.exit(1);
}

console.log('TRANSPILED CODE'.cyan);
console.log(result.outputText);

console.log('EVAL'.cyan);
eval(result.outputText);