var ts = require('./typescript');

module.exports = function (callback, source) {

    var result = ts.transpileModule(source, {
        compilerOptions: {
            module: ts.ModuleKind.ES2015,
        },
        reportDiagnostics: true
    });

    var message = 'success';

    if (result.diagnostics && result.diagnostics.length > 0) {
        message = '';

        for (i = 0; i < result.diagnostics.length; i++) {
            var start = result.diagnostics[i].start;
            var length = result.diagnostics[i].length;
            var messageText = result.diagnostics[i].messageText;

            if (i > 0)
                message = message + '\n';

            message = message + '(' + start + ', ' + length + '): ' + messageText;
        }
    }

    callback(null, message);
}
