# Recall-precision curves: use with caution!

#### Abstract
Recall-precision (RP) curves are useful to visualize the
trade-off of a binary classifier between detecting the maximum of
positive instances and outputing a faithful list of positive
instances. However, RP curves are particularly difficult to estimate
from a finite sample and ignoring uncertainty on an estimated curve
can lead to erroneous interpretations. This note illustrates the
problem and recalls several devices to deal with RP curves more
safely.

## Definitions

A typical setting for (binary) classification problem in machine
learning is to define two random variables $X$ (the features) and $Y
\in \lbrace -1, 1 \rbrace$ (the labels) following an *unknown* joint
distribution $\mathbb{P}[X,Y]$ and assume we have a finite
i.i.d. sample $\lbrace (x_1, y_1), \dots, (x_n, y_n) \rbrace$. From
this sample, we try to estimate a function $f$ that optimally predicts
$Y$ from $X$. To measure the performance of $f$, we typically follow
the *true positive rate* (also called *sensitivity* or *recall*):

$$
\text{TPR} = \mathbb{P}[f(X) = 1 \mid Y = 1]
$$

which measures the proportion of positive labels that are predicted as
such, and the *true negative rate* (or *specificity*):

$$
\text{TNR} = \mathbb{P}[f(X) = -1 \mid Y = -1]
$$

which measures the proportion of negative labels that are predicted as
such. In cases where the positive labels are rare, and we need to
"fish" them in a sea of negative labels, it is more useful to replace
specificity by the *positive predictive value* (or *precision*):

$$
\text{PPV} = \mathbb{P}[Y = 1 \mid f(X) = 1]
$$

which measures how confident we can be when we predict a positive
label.

Many classification learning methods estimate a real-valued function
$f$, which is by default thresholded at 0 to produce binary
predictions. Varying this threshold is a means to tune the
performance:

- lowering the threshold may increase recall and cannot reduce it
- raising the threshold will *tend* to increase precision but it may
  also reduce it.

Representing the curve $(\text{Recall}(\theta),
\text{Precision}(\theta))$ as a function of the threshold $\theta$ is
great to get a quantitative feeling of the possible trade-offs between
precision and recall: for a given method increasing recall
*eventually* tends to to reduce precision.

In a typical scenario, we first estimate a function $f$ on some
i.i.d. sample from the joint distribution of $X$ and $Y$, and that we
have another independent one, $\lbrace (x_1, y_1), \dots, (x_n, y_n)
\rbrace$, to evaluate the performance of this function. We call
*evaluation dataset* the set $\lbrace (f(x_1), y_1), \dots, (f(x_n),
y_n) \rbrace$.

In the rest of the note, we will the following preamble:

```ocaml
module Show = Scirep.Show;;
open Core_kernel;;
open Gsl;;

let rng = Rng.(make (default ()));;

type evaluation_data =
  Evaluation_data of (float * bool) list;;
```



## The binormal model

While RP curves can be used for any kind of binary classifier, it will
be useful to illustrate their use on a particular model, which can be
analytically devised. Following a [paper](#ref_Brodersen) by Brodersen
*et al*, we'll use the so-called *binormal model* defined by:

$$
\begin{aligned}
& Y        & \sim & \; \text{Bern}(\alpha)\\\\
& X \mid Y & \sim & \; \mathcal{N}(\mu_Y, \sigma_Y)
\end{aligned}
$$

The corresponding computational definitions is:

```ocaml
type binormal_params = {
  mu_pos : float ;
  sigma_pos : float ;
  mu_neg : float ;
  sigma_neg : float ;
  alpha : float ;
}
;;

let binormal_params ?(mu_pos = 1.) ?(sigma_pos = 1.) ?(mu_neg = 0.) ?(sigma_neg = 1.) alpha =
  { mu_pos ; mu_neg ; sigma_pos ; sigma_neg ; alpha }
;;
```

From the definition we can easily derive a simulator:

```ocaml
let binormal_simulation rng ~n { mu_pos ; mu_neg ; sigma_pos ; sigma_neg ; alpha } =
  let sample = List.init n ~f:(fun _ ->
      let mu, sigma, label = match Randist.bernoulli rng ~p:alpha with
        | 0 -> mu_neg, sigma_neg, false
        | 1 -> mu_pos, sigma_pos, true
        | _ -> assert false
      in
      mu +. Randist.gaussian rng ~sigma, label
    )
  in
  Evaluation_data sample
;;
```

However, one great value of the binormal model is that its density is
easy to compute:

```ocaml
let phi ~mu ~sigma x = Cdf.gaussian_P ~x:(x -. mu) ~sigma;;
let inv_phi ~mu ~sigma p = Cdf.gaussian_Pinv ~p ~sigma +. mu;;

let binormal_density p x =
  let phi_neg = phi ~mu:p.mu_neg ~sigma:p.sigma_neg in
  let inv_phi_pos = inv_phi ~mu:p.mu_pos ~sigma:p.sigma_pos in
  let alpha = p.alpha in
  alpha *. x
  /.
  (alpha *. x
   +.
   (1. -. alpha) *. (1. -. phi_neg (inv_phi_pos (1. -. x))))
;;

```

Let's see what it looks like:

```ocaml
type curve = {
  x : float array ;
  y : float array ;
  col : string option ;
  lwd : int option ;
  label : string option ;
}
;;

let binormal_curve ?(n = 100) ?col ?lwd ?label p =
  let max_i = float (n - 1) in
  let x = Array.init n ~f:(fun i -> float i /. max_i) in
  let y = Array.map x ~f:(binormal_density p) in
  { x ; y ; col ; lwd ; label }
;;

let plot_curve { x ; y ; col ; lwd ; _ } =
  OCamlR_graphics.lines ?lwd ?col ~x ~y ()
;;

let recall_precision_plot ?main curves fn =
  OCamlR_grDevices.svg fn ;
  OCamlR_graphics.plot
    ~plot_type:`Lines ?main
    ~xlab:"Recall" ~ylab:"Precision"
    ~xlim:(0., 1.) ~ylim:(0., 1.) ~x:[||] ~y:[||] () ;
  List.(iter (rev curves)) ~f:plot_curve ;
  OCamlR_grDevices.dev_off ()
;;

Show.svg (fun fn ->
  let p = binormal_params ~sigma_neg:0.7 0.1 in
  let c = binormal_curve p in
  recall_precision_plot
    ~main:"Binormal model density"
    [ c ] fn
);;
```

## Empirical precision-recall curve


```ocaml
let xy xs =
  let curve = Array.of_list xs in
  let x = Array.map curve ~f:fst in
  let y = Array.map curve ~f:snd in
  x, y
;;
(* Utility function *)

let empirical_curve_points (Evaluation_data xs) =
  let npos = List.count xs ~f:snd in
  let compare =
    Tuple.T2.compare ~cmp1:Float.descending ~cmp2:Bool.descending
  in
  List.sort xs ~compare
  |> List.fold ~init:(0, 0, [ 0., 1. ])
                  ~f:(fun (pos, neg, points) (_, b) ->
      let pos, neg = if b then pos + 1, neg else pos, neg +1 in
      let recall = float pos /. float npos in
      let precision = float pos /. float (pos + neg) in
      (pos, neg, (recall, precision) :: points)
    )
    |> trd3
    |> List.rev
;;

let empirical_curve ?col ?label ?lwd data =
  let x, y = xy (empirical_curve_points data) in
  { x ; y ; col ; label ; lwd }
;;
```

Let's see what it looks like on a particular sample:

```ocaml
Show.svg (fun fn ->
  let p = binormal_params 0.1 in
  let sim = binormal_simulation ~n:1000 rng p in
  let ec = empirical_curve sim in
  recall_precision_plot [ ec ] fn
);;
```

This saw-like aspect is very typical, and is a consequence of sorting
ties with positive labels first.


## The problem

We are finally in position for illustrating a real danger when
plotting RP curves:

```ocaml
Show.svg (fun fn ->
  let p = binormal_params 0.1 in
  let sims = List.init 30 ~f:(fun _ ->
    binormal_simulation ~n:1000 rng p
	|> empirical_curve ~col:"lightgrey"
  ) in
  let density = binormal_curve p in
  recall_precision_plot (density :: sims) fn
);;
```

We can see a very significant variability among the 30 empirical
curves, especially in the region that matters most, that is for high
precision. This shouldn't come as a surprise, since for a very
stringent threshold precision is computed on a very small sample. It
should be emphasized that this will always be the case, whatever the
size of the evaluation dataset. The phenomenon will be less severe for
well-performing classifiers, but still very much present.

## Average precision

While the empirical RP curve from a single sample is so difficult to
estimate, the *average precision* (integrated over recall) is more
stable and still useful to compare various classifiers. In other word,
we could use the area under the RP curve instead of the curve.

A [paper by Boyd and colleagues](#ref_Boyd) evaluates several
estimators and associated confidence intervals.

### Binormal estimator

Let's start with a parametric method, based on the binormal
model. Basically, it boils down to assuming the data is generated by
a binormal model and compute a maximum likelihood estimate for its
parameters. It is particularly easy here:


```ocaml
let binormal_estimate (Evaluation_data xs) =
  let select label =
    List.filter_map xs ~f:(fun (x, b) ->
	  if Bool.(b = label) then Some x else None
	)
    |> Array.of_list
  in
  let x = select false in
  let y = select true in
  let len t = float (Array.length t) in
  Stats.{
    mu_pos = mean y ;
    mu_neg = mean x ;
    sigma_pos = sd y ;
    sigma_neg = sd x ;
    alpha = len y /. (len x +. len y) ;
  }
;;
```

Let's see how it performs on an example:

```ocaml
Show.svg (fun fn ->
  let p = binormal_params 0.1 in
  let sim = binormal_simulation ~n:1000 rng p in
  let estimation = binormal_estimate sim in
  let curves = [
	binormal_curve estimation ~col:"green" ;
    binormal_curve ~lwd:2 p ;
	empirical_curve ~col:"lightgrey" sim ;
  ]
  in
  recall_precision_plot curves fn
);;
```

### Lower trapezoidal estimator

```ocaml
let sum a b ~f =
  let rec loop i acc =
    if i > b then acc
    else loop (i + 1) (acc +. f i)
  in
  loop a 0.
;;

let auc_trapezoidal_lt classification =
  let curve = empirical_curve_points classification in
  let n, r, pmin, pmax =
    let data =
      List.group curve ~break:(fun (r1,_) (r2, _) -> Float.(r1 <> r2))
      |> List.map ~f:(function
          | [] -> assert false
          | (r, _) :: _ as xs -> r, List.map xs ~f:snd
        )
      |> Array.of_list
    in
    Array.length data,
    Array.map data ~f:fst,
    Array.map data ~f:(fun (_, ps) -> List.reduce_exn ps ~f:Float.min),
    Array.map data ~f:(fun (_, ps) -> List.reduce_exn ps ~f:Float.max)
  in
  sum 0 (n - 2) ~f:(fun i -> (pmin.(i) +. pmax.(i + 1)) /. 2. *. (r.(i + 1) -. r.(i)))
;;
```

### Average precision estimator

This estimator by-passes the empirical curve entirely and tries to
estimate average precision directly:

```ocaml
let auc_average_precision (Evaluation_data xs) =
  let pos, _, sum =
    List.sort xs ~compare:(Tuple.T2.compare ~cmp1:Float.descending ~cmp2:Bool.descending)
    |> List.fold ~init:(0, 0, 0.) ~f:(fun (pos, neg, sum) (_, b) ->
        let pos, neg = if b then pos + 1, neg else pos, neg +1 in
        let sum =
          if b then
            let precision = float pos /. float (pos + neg) in
            precision +. sum
          else sum
        in
        (pos, neg, sum)
      )
  in
  sum /. float pos
;;
```

### Estimator comparison

```ocaml
Show.svg (fun fn ->
  let open OCamlR_base in
  let n = 1_000 in
  let sigma = 0.9 in
  let alpha = 0.1 in
  let p = binormal_params ~sigma_pos:sigma ~sigma_neg:sigma alpha in
  let samples = List.init 1_000 ~f:(fun _ -> binormal_simulation ~n rng p) in
  let compute f =
    List.map samples ~f
    |> Numeric.of_list
    |> Numeric.to_sexp
  in
  let trapezoidal_lt = compute auc_trapezoidal_lt in
  let average_precision = compute auc_average_precision in
  let l = List_.create [
      Some "trapezoidal", trapezoidal_lt ;
      Some "average_precision", average_precision ;
    ]
  in
  OCamlR_grDevices.svg fn ;
  OCamlR_graphics.list_boxplot l ;
  OCamlR_grDevices.dev_off ()
)
```

## Confidence intervals

```ocaml
let logit p = Float.log (p /. (1. -. p));;

let sigmoid x =
  let exp_x = Float.exp x in
  exp_x /. (1. +. exp_x)
;;

let logit_confidence_interval ~alpha ~theta_hat ~n =
  let eta_hat = logit theta_hat in
  let tau_hat = (float n *. theta_hat *. (1. -. theta_hat)) ** (-0.5) in
  let delta = tau_hat *. Cdf.gaussian_Pinv ~sigma:1. ~p:(1. -. alpha /. 2.) in
  sigmoid (eta_hat -. delta),
  sigmoid (eta_hat +. delta)
;;
```

```ocaml
let logit_confidence_interval_coverage_test ?(seed = 42) ?(sigma = 1.) ~n ~alpha () =
  let p = binormal_params ~sigma_pos:sigma ~sigma_neg:sigma alpha in
  let nsims = 1_000 in
  let samples = List.init nsims ~f:(fun _ -> binormal_simulation ~n rng p) in
  let npos = List.map samples ~f:(fun (Evaluation_data xs) -> List.count xs ~f:snd) in
  let estimates = List.map samples ~f:auc_trapezoidal_lt in
  let confidence_intervals = List.map2_exn estimates npos ~f:(fun theta_hat n ->
      logit_confidence_interval ~alpha:0.05 ~theta_hat ~n
    )
  in
  let best_estimate = Array.of_list estimates |> Stats.mean in
  let nsuccessess = List.count confidence_intervals ~f:(fun (lb, ub) -> Float.(lb <= best_estimate && best_estimate <= ub)) in
  float nsuccessess /. float nsims
;;

let _ = logit_confidence_interval_coverage_test ~n:1_000 ~alpha:0.1 ()
;;
```

## References

<a id="ref_Brodersen">[1]</a> 
*The binormal assumption on precision-recall curves*, Kay
H. Brodersen, Cheng Soon Ong, Klaas E. Stephan and Joachim M. Buhmann

<a id="ref_Boyd">[2]</a> 
*Area Under the Precision-Recall Curve: Point Estimates and Confidence
Intervals*, Kendrick Boyd, Kevin H. Eng and C. David Page
