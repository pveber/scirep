PKGS=compiler-libs.toplevel,core_kernel,ezxmlm,higlo.ocaml,omd,ppx_blob,vg.svg,xtmpl

test.html: test.md scirep 
	./scirep $< $@

scirep: scirep.ml style.css template.html
	ocamlfind ocamlc -rectypes -g -o $@ -package $(PKGS) -linkpkg $<
