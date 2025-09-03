package lsp

func NewInitializeResponse(id int32, version string) InitializeResponse_t {
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

func NewState() State_t {
	return State_t{Documents: map[string]string{}}
}

// I imagine there is some semantic layer we explore later that justifies the below. More tutorial to go I suppose
func (s *State_t) OpenDocument(uri, text string) {
	s.Documents[uri] = text
}

func (s *State_t) UpdateDocument(uri, text string) {
	s.Documents[uri] = text
}
