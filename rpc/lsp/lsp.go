package lsp

type TextDocumentSyncKind int

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
	ID     int    `json:"id"`
	Method string `json:"method"`
}

type Response_t struct {
	RPC string `json:"jsonrpc"`
	ID  *int   `json:"id,omitempty"`

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

func NewInitializeResponse(id int, version string) InitializeResponse_t {
	return InitializeResponse_t{
		Response_t: Response_t{
			RPC: "2.0",
			ID:  &id,
		},
		Result: &InitializeResult_t{
			ServerCapabilities: ServerCapabilities_t{
				TextDocumentSync: TextDocumentSyncFull,
			},
			ServerInfo: ServerInfo_t{
				Name:    "Numinous RPC",
				Version: version,
			},
		},
	}
}
