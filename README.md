# scirep

This is a quick hack to convert a Markdown file containing LaTeX-style
math formulas and OCaml code to an HTML page. The OCaml code gets
evaluated and the output is printed. Graphical outputs can also be
integrated to the page. See `test.md` for an example.

`scirep` builds on the following awesome libraries: `omd`, `vg`,
`higlo` and `core`. It also borrows quite some code from
[makmash](https://github.com/smondet/makmash) and
[stog](http://zoggy.github.io/stog/).
