fold.html: fold.md
	scirep --libs vg $< $@

recall_precision_curve.html: recall_precision_curve.md
	scirep --libs core_kernel,gsl,ocaml-r.graphics,ocaml-r.grDevices $< $@

damped.html: damped.md
	scirep --libs owl-ode,owl-plplot $< $@

deriving.html: deriving.md
	scirep --libs core,ppx_sexp_conv,bistro.ppx,bistro $< $@
