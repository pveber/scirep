# scirep

This is a quick hack to convert a Markdown file containing LaTeX-style
math formulas and OCaml code to an HTML page. The OCaml code gets
evaluated and the output is printed. Graphical outputs can also be
integrated to the page. See the `examples` directory.

## Usage

Libraries used in the code of a markdown document can be specified on
the command-line. Upon cloning the repo, you can try:

```
$ dune build
$ dune install
$ scirep eval --libs vg examples/fold.md examples/fold.html
```

To output graphics, one has to use functions from the `Scirep.Show`
module. These functions just print an HTML rendering of pictures on
stdout, and the HTML elements are later detected and appropriately
displayed in the final document.

`scirep` can also be used in combination with `mdx` to generate HTML
reports from markdown documents with executable code chunks. Running
`mdx` in test mode on a document containing calls to the functions in
`Scirep.Show` will add HTML `<img>` elements in the markdown document.
Then, call `scirep` to generate an HTML document:

```
$ dune runtest examples
$ scirep md2html examples/mdx_example.md examples/mdx_example.html
```

### Acknowledgments

`scirep` builds on the following awesome libraries: `omd`, `vg`,
`higlo` and `core`. It also borrows quite some code from
[makmash](https://github.com/smondet/makmash),
[stog](http://zoggy.github.io/stog/) and
[mdx](https://github.com/realworldocaml/mdx).
