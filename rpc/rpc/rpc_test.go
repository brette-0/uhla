package rpc_test

import (
	"fmt"
	"os"
	"path/filepath"
	"testing" // library

	"rpc/rpc" // local
)

type EncodingExample struct {
	Testing bool
}

func TestEncode(t *testing.T) {
	expected := fmt.Sprintf("%s16\r\n\r\n{\"Testing\":true}", rpc.HeaderPrefix)
	actual := rpc.EncodeMessage(EncodingExample{Testing: true})

	if expected != actual {
		t.Fatalf("Expected %s, Actual: %s", expected, actual)
	}
}

func TestDecode(t *testing.T) {
	const TestExpect = "hi"
	const TestingString = "{\"Method\":\"" + TestExpect + "\"}"
	const TestingLength = len(TestingString)
	IncomingMessage := fmt.Sprintf("%s%d\r\n\r\n{\"Method\":\"hi\"}", rpc.HeaderPrefix, TestingLength)

	Method, Content, err := rpc.DecodeMessage([]byte(IncomingMessage))

	if err != nil {
		t.Fatal(err)
	}

	if len(Content) != TestingLength {
		t.Fatalf("Expected: %d, Actual %d", TestingLength, len(Content))
	}

	if Method != TestExpect {
		t.Fatalf("Expected: %s, Actual %s", TestExpect, Method)
	}
}

func TestGetLogger(t *testing.T) {
	_, err := os.OpenFile(filepath.Join(os.TempDir(), "Numinous-rpc.log"), os.O_CREATE|os.O_TRUNC|os.O_WRONLY, 0666)

	if err != nil {
		t.Fatalf("could not begin logging : %s", err.Error())
	}
	t.Logf("Log file created at: %s", filepath.Join(os.TempDir(), "Numinous-rpc.log"))
}
