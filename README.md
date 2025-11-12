# Universal High Level Assembler (UHLA)

> UHLA is currently in an alpha state. The current in-dev features can be found at the bottom of the document.

UHLA is a macro-oriented high level assembler using principles from higher level languages to allow an orthodox scripting pipeline for rich conditional assembly. As a result of new compile time features, UHLA offers the powerful macro scripting capabilities targeting all the supported architectures, producing code optimal in both the generic and edge case contexts. It's targeted for new Assembly developers with a friendlier, clean, C-imitative syntax with optional safety nets for beginners and total control with config scripts and pragmas for the expert.

Supporting over 20 languages, anyone can easily get started on their Windows, Linux or Mac computer with a deeply configurable and completely portable executable. Written in C# UHLA focuses on the feature set of the language, ensuring the code-base is easily changeable for anyone seeking to contribute or fork! Be it adding/changing hardware support, appealing a language add/change, documentation suggestions or a contribution to the standard library Numinous will always be an open source tool.

### Core

| Member        | Completion | Core Test | Full Test | Error Handling |
| ------------- | ---------- | --------- | --------- | -------------- |
| `Core`        | ?          | ✕         | ✕         | ✕              |
| `Types`       | ?          | ✓         | ✓         | ✓              |
| `Language`    | ?          | ✓         | ✕         | ✕              |
| `Terminal`    | 66%        | ✓         | ✓         |                |
| `Lexer`       | 80%        | ✕         | ✕         | ✕              |
| `Linker`      | 50%        | ✓         | ✓         | ✓              |
| `Database`    | 40%        | ✓         | ✕         | ✕              |
| `Evaluation`  | 40%        | ✕         | ✕         | ✕              |
| `ObjectToken` | 100%       | ✓         | ✓         | ✓              |
| `EvalToken`   | 100%       | ✓         | ✓         | ✓              |


### Architectures


| Arhcitecture | Completion | Core Test | Full Test | Error Handling | Library |
| ------------ | ---------- | --------- | --------- | -------------- | ------- |
| NMOS 6502    | ?          | ✕         | ✕         | ✕              | ✕       |
| Ricoh 2a03   | ?          | ✕         | ✕         | ✕              | ✕       |

### Download:
- Windows
- Linux
- Mac

### Extensions:
- [Jetbrains Rider]()
- [Visual Studio]()
- [Visual Studio Code]()

### Documents:
- [**Assembler Guide**]()
- [**Assembly Guide**]()
- [**Library Docs**]()
- [**Contributing**]()
