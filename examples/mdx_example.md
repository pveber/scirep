# Combining mdx and scirep

`scirep` can be used in combination with `mdx` to generate HTML
reports from markdown documents with executable code chunks. This very
file can be used with `mdx` in test mode through `dune` for
instance. The command `dune runtest examples` updates the markdown
file with toplevel outputs and stdout contents. The latter may include
HTML `<img>` elements, which are rendered appropriately in the final
document.

Note that `mdx` and `scirep` in eval mode do not have the same
behaviour, since `mdx` requires a leading `#` to evaluate a code
block. Also the rendering is a bit different in the two modes
(interpreter output cannot be grayed in `md2html` mode).

```ocaml
# #require "scirep";;
```

```ocaml
# open Gg
# open Vg

# let gray = I.const (Color.gray 0.5)
val gray : image = <abstr>

# let circle = P.empty |> P.circle (P2.v 0.5 0.5) 0.4
val circle : path = <abstr>

# let gray_circle = I.cut circle gray
val gray_circle : image = <abstr>

# let image =
  let area = `O { P.o with P.width = 0.04 } in
  let black = I.const Color.black in
  I.cut ~area circle black
val image : image = <abstr>

# Scirep.Show.vg image
<img src="data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB2ZXJzaW9uPSIxLjEiIHdpZHRoPSIxMDBtbSIgaGVpZ2h0PSIxMDBtbSIgdmlld0JveD0iMCAwIDEwMCAxMDAiIGNvbG9yLXByb2ZpbGU9ImF1dG8iIGNvbG9yLWludGVycG9sYXRpb249ImxpbmVhclJHQiIgY29sb3ItaW50ZXJwb2xhdGlvbi1maWx0ZXJzPSJsaW5lYXJSR0IiPjxnIGZpbGw9Im5vbmUiIHN0cm9rZS1taXRlcmxpbWl0PSI5Ljk4MTIzIiB0cmFuc2Zvcm09Im1hdHJpeCgxMDAgMCAwIC0xMDAgLTAgMTAwKSI+PGRlZnM+PHBhdGggaWQ9ImkxIiBkPSJNMC45IDAuNUEgMC40IDAuNCAwIDAgMSAwLjEgMC41QSAwLjQgMC40IDAgMCAxIDAuOSAwLjVaIi8+PC9kZWZzPjx1c2UgeGxpbms6aHJlZj0iI2kxIiBzdHJva2Utd2lkdGg9IjAuMDQiIHN0cm9rZT0iIzAwMDAwMCIvPjwvZz48L3N2Zz4="></img>
- : unit = ()
```
