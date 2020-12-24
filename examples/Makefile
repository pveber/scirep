fold.html: fold.md
	scirep eval --libs vg $< $@

recall_precision_curve.html: recall_precision_curve.md
	scirep eval --libs core_kernel,gsl,ocaml-r.graphics,ocaml-r.grDevices $< $@

damped.html: damped.md
	scirep eval --libs owl-ode,owl-plplot $< $@

deriving.html: deriving.md
	scirep eval --libs core,ppx_sexp_conv,bistro.ppx,bistro $< $@

mdx_example.html: mdx_example.md
	scirep md2html $< $@
