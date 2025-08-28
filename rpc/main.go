package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"rpc/rpc"
)

func main() {
	logger := getLogger()
	logger.Println("Numinous RPC logging init")

	scanner := bufio.NewScanner(os.Stdin)
	scanner.Split(rpc.Split)
	for scanner.Scan() {
		msg := scanner.Text()
		handleMessage(msg, logger)
	}
}

func handleMessage(msg any, logger *log.Logger) {
	logger.Printf("Recieved : %s", msg)
}

func getLogger() *log.Logger {

	logfile, err := os.OpenFile(filepath.Join(os.TempDir(), "Numinous-rpc.log"), os.O_CREATE|os.O_TRUNC|os.O_WRONLY, 0666)

	if err != nil {
		panic(fmt.Sprintf("could not begin logging : %s", err.Error()))
	}

	return log.New(logfile, "[NuminousRPC]", log.Ldate|log.Ltime|log.Lshortfile)
}
