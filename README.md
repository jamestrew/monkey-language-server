# monkey-language-server
LSP server for [monkey](https://interpreterbook.com/#the-monkey-programming-language) (with some liberties taken).

Just wanted to write an LSP server in Rust and see what it takes to turn ideas from an interpreter into a language server (it was much harder 😅).


## Features (demo)
Showcase of features implemented inside of neovim (with [tree-sitter highlighting](https://github.com/jamestrew/tree-sitter-monkey)).

- [x] diagnostics

![image](https://github.com/jamestrew/monkey-language-server/assets/66286082/0351ee18-631c-4492-8ebd-46199bd91d4b)

- [x] go to definition

https://github.com/jamestrew/monkey-language-server/assets/66286082/24e9af97-d515-4cdf-8b19-e0aa7b0edd5f

- [x] find references

https://github.com/jamestrew/monkey-language-server/assets/66286082/76897e41-445b-4a40-a13b-7aa31aac64e1

- [x] completion

https://github.com/jamestrew/monkey-language-server/assets/66286082/01f5547d-212e-49a0-ae3f-fcf5dd60be0f

- [x] hover (with light type inferences)

https://github.com/jamestrew/monkey-language-server/assets/66286082/c30943bd-f5f3-4ec6-b40d-e20f5355e399

