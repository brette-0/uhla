package rpc

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"strconv"
)

func main() {
	fmt.Println("penar")
}

const HeaderPrefix = "Content-Length: "

type BaseMessage_t struct {
	Method string `json:"method"`
}

func SendEncoded(msg map[string]any) {
	content, err := json.Marshal(msg)
	if err != nil {
		panic(err)
	}
	fmt.Fprintf(os.Stdout, "%s%d\r\n\r\n%s", HeaderPrefix, len(content), content)
}

func EncodeMessage(msg any) string {
	content, err := json.Marshal(msg)
	if err != nil {
		panic(err)
	}

	return fmt.Sprintf("%s%d\r\n\r\n%s", HeaderPrefix, len(content), content)
}

func DecodeMessage(msg []byte) (method string, content []byte, err error) {
	method, content, err = "", nil, nil

	header, content, found := bytes.Cut(msg, []byte{'\r', '\n', '\r', '\n'})
	if !found {
		err = errors.New("malformed Message : could not find header seperator")
		return
	}

	ContentLength, err := strconv.Atoi(string(header[len(HeaderPrefix):]))

	if err != nil {
		err = errors.New("malformed Message : could not decipher content length")
		return
	}

	var BaseMessage BaseMessage_t
	if err = json.Unmarshal(content[:ContentLength], &BaseMessage); err != nil {
		err = errors.New("malformed Message : could not unmarshal content")
		return
	}

	return BaseMessage.Method, content, nil
}

func Split(data []byte, _ bool) (int, []byte, error) {
	header, content, found := bytes.Cut(data, []byte{'\r', '\n', '\r', '\n'})
	if !found {
		return 0, nil, nil
	}

	ContentLength, err := strconv.Atoi(string(header[len(HeaderPrefix):]))

	if err != nil {
		err = errors.New("malformed Message : could not decipher content length")
		return 0, nil, nil
	}

	if len(content) < ContentLength {
		return 0, nil, nil
	}

	TotalLength := len(header) + 4 + ContentLength
	return TotalLength, data[:TotalLength], nil
}
