package main

/*

	To become feature complete we must:
		actions:
			illegal   -> legal
			synthetic -> expand
			macro     -> inline

		autocomplete with dependency referral

		colourise:
			opcodes:
			scripting:

		hover for:
			opcodes
				implicit:
					register determinism (string)
					encodes				 (true)		-> can be used to refer to scripting variables
					idtable				 (true)		-> uses a found identity table
				explicit|implicit:
					legal			     (bool)
					cycle time			 (int)
					size				 (int)
					supported MAOs		 (string)
					Mnemonic long name	 (string)
				synthetic:

			directives
			keywords
			registered

			variables
			constants

*/

import (
	"bufio"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"rpc/lsp"
	"rpc/rpc"
	"time"
)

const version = "alpha-1"

func main() {
	logger := getLogger()
	logger.Println("Numinous RPC logging init")

	scanner := bufio.NewScanner(os.Stdin)
	scanner.Split(rpc.Split)

	state := lsp.NewState()

	for scanner.Scan() {
		msg := scanner.Bytes()
		method, contents, err := rpc.DecodeMessage(msg)
		if err != nil {
			logger.Print(err)
			continue
		}

		handleMessage(method, state, contents, logger)
	}
}

func handleMessage(method string, state lsp.State_t, contents []byte, logger *log.Logger) {
	logger.Printf("Recieved request '%s'", method)

	switch method {
	case "initialize":
		var request lsp.InitializeRequest_t
		if err := json.Unmarshal(contents, &request); err != nil {
			logger.Print(err)
		}

		logger.Printf("Connected to %s : %s", request.Params.ClientInfo.Name, request.Params.ClientInfo.Version)

		msg := lsp.NewInitializeResponse(request.ID, version)
		reply := rpc.EncodeMessage(msg)

		writer := os.Stdout
		writer.Write([]byte(reply))

		logger.Printf("Replied to %s : %s", request.Params.ClientInfo.Name, request.Params.ClientInfo.Version)

	case "textDocument/didOpen":
		var request lsp.DidOpenTextDocumentNotification_t
		if err := json.Unmarshal(contents, &request); err != nil {
			logger.Printf("%s: %s", method, err)
		}

		logger.Printf("Opened: %s with size of %d", request.Params.TextDocumentItem.URI, len(request.Params.TextDocumentItem.Text))
		state.OpenDocument(request.Params.TextDocumentItem.URI, request.Params.TextDocumentItem.Text)

	case "textDocument/didChange":
		var request lsp.DidChangeTextDocumentNotification_t
		if err := json.Unmarshal(contents, &request); err != nil {
			logger.Printf("%s: %s", method, err)
		}

		logger.Printf("Changed: %s with size of %d", request.Params.TextDocument.URI, len(request.Params.ContentChanges))

		for _, change := range request.Params.ContentChanges {
			state.UpdateDocument(request.Params.TextDocument.URI, change.Text)
		}
	}
}

func getLogger() *log.Logger {
	const TimeFormatting = "2006-01-02_15-04-05"

	logfile, err := os.OpenFile(filepath.Join(os.TempDir(), fmt.Sprintf("Numinous-rpc-%s.log", time.Now().Format(TimeFormatting))), os.O_CREATE|os.O_TRUNC|os.O_WRONLY, 0666)

	if err != nil {
		panic(fmt.Sprintf("could not begin logging : %s", err.Error()))
	}

	return log.New(logfile, "[NuminousRPC]", log.Ldate|log.Ltime|log.Lshortfile)
}
