package main

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
	for scanner.Scan() {
		msg := scanner.Bytes()
		method, contents, err := rpc.DecodeMessage(msg)
		if err != nil {
			logger.Print(err)
			continue
		}

		handleMessage(method, contents, logger)
	}
}

func handleMessage(method string, contents []byte, logger *log.Logger) {
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
