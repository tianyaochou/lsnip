# lsnip

An LSP server for snippets for editors that support multiple lsp servers but not snippets(for example [Helix](https://helix-editor.com))

Snippet syntax comforms to LSP snippet syntax.

## Usage

Default configuration location is `$HOME/.config/lsnip/snippets.toml`, custom location can be passed to `--config`(or `-c` for short).

Excluding other languages using

## Example configuration

```toml
[[snippets]]
label = "<div" # required, completion label
lang = "html" # optional, language the snippet is for
doc = "div element" # optional, description
body = """
<div>
  $0
</div>"""
```
## Future ideas

⚠️Run arbitrary shell command in snippets
