package lsp

type TextDocumentSyncKind int32

const (
	TextDocumentSyncNone        TextDocumentSyncKind = 0
	TextDocumentSyncFull        TextDocumentSyncKind = 1
	TextDocumentSyncIncremental TextDocumentSyncKind = 2
)

type CompletionOptions_t struct {
	ResolveProvider   bool     `json:"resolveProvider,omitempty"`
	TriggerCharacters []string `json:"triggerCharacters,omitempty"`
}

// params will be passed based on method
type Request_t struct {
	RPC    string `json:"jsonrpc"`
	ID     int32  `json:"id"`
	Method string `json:"method"`
}

type Response_t struct {
	RPC string `json:"jsonrpc"`
	ID  *int32 `json:"id,omitempty"`

	// result
	// error
}

type Notification_t struct {
	RPC    string `json:"jsonrpc"`
	Method string `json:"method"`
}

type InitializeRequest_t struct {
	Request_t
	Params *InitializeRequestParams_t `json:"params"`
}

type InitializeRequestParams_t struct {
	ClientInfo *ClientInfo_t `json:"clientInfo"`
}

type ClientInfo_t struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

type InitializeResponse_t struct {
	Response_t
	Result *InitializeResult_t `json:"result"`
}

type InitializeResult_t struct {
	ServerCapabilities ServerCapabilities_t `json:"capabilities"`
	ServerInfo         ServerInfo_t         `json:"serverInfo"`
}

type ServerCapabilities_t struct {
	TextDocumentSync   TextDocumentSyncKind `json:"textDocumentSync"`
	HoverProvider      bool                 `json:"hoverProvider,omitempty"`
	CompletionProvider *CompletionOptions_t `json:"completionProvider,omitempty"`
}
type ServerInfo_t struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

type TextDocumentItem_t struct {
	URI        string `json:"uri"`
	LanguageID string `json:"languageId"`
	Version    int32  `json:"version"`
	Text       string `json:"text"`
}

type TextDocumentIdentifier_t struct {
	URI string `json:"uri"`
}

type VersionTextDocumentIdentifier_t struct {
	TextDocumentIdentifier_t
	Version int32 `json:"version"`
}

type DidOpenTextDocumentNotification_t struct {
	Notification_t
	Params DidOpenTextDocumentParams_t `json:"params"`
}

type DidOpenTextDocumentParams_t struct {
	TextDocumentItem TextDocumentItem_t `json:"textDocument"`
}

type State_t struct {
	Documents map[string]string
}

type DidChangeTextDocumentNotification_t struct {
	Notification_t
	Params DidChangeTextDocumentParams_t `json:"params"`
}

type DidChangeTextDocumentParams_t struct {
	TextDocument   VersionTextDocumentIdentifier_t `json:"textDocument"`
	ContentChanges []TextDocumentChangeEvent_t     `json:"contentChanges"`
}

type TextDocumentChangeEvent_t struct {
	Text string `json:"text"`
}
