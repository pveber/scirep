(lang dune 2.4)
(generate_opam_files true)
(using mdx 0.1)

(name scirep)
(source (github pveber/scirep))
(homepage "https://github.com/pveber/scirep/")
(bug_reports "https://github.com/pveber/scirep/issues")
(license CeCILL-B)
(authors "Philippe Veber")
(maintainers "philippe.veber@gmail.com")

(package
  (name scirep)
  (synopsis "Generate a webpage with mathematical formulae, OCaml output and graphics from markdown")
  (description "
This is a quick hack to convert a Markdown file containing LaTeX-style
math formulas and OCaml code to an HTML page. The OCaml code gets
evaluated and the output is printed. Graphical outputs can also be
integrated to the page. See test.md for an example.

scirep builds on the following awesome libraries: omd, vg, higlo and
core. It also borrows quite some code from makmash and stog.
")
  (depends
    compiler-libs
    core_kernel
    ezxmlm
    omd
    ppx_blob
    vg))
