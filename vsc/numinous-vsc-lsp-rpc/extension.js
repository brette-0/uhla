const { LanguageClient } = require("vscode-languageclient/node");

function activate(context) {
    // Path to your Go LSP server
    const serverExe = "C:\\Users\\benis\\Documents\\GitHub\\Numinous\\rpc\\rpc.exe";

    const serverOptions = {
        run: { command: serverExe },
        debug: { command: serverExe }
    };

    const clientOptions = {
        // Only run LSP for files detected as asm language
        documentSelector: [{ scheme: "file", language: "asm" }]
    };

    const client = new LanguageClient(
        "asmLsp",
        "Assembly Language Server",
        serverOptions,
        clientOptions
    );

    context.subscriptions.push(client.start());
}

function deactivate() {}

module.exports = { activate, deactivate };
