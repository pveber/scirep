# List traversal with fold

The first and foremost feature of a functional language is that
variables can't be modified (they are said to be *immutable*). A
direct implication of this is that `for` and `while` loops become
useless: since iterations can't modify variables, they are doomed to
reproduce the same calculation each time. In functional programming,
loops are replaced by *recursive functions* to perform iterative
calculations. Let us start with an example.

## Sum of integers

Suppose we'd like to compute the sum of <<n>> integers
<<x_1,\dots,x_n>>. A simple recursive formulation for this is to say
that we can compute the sum of the first <<n-1>> integers and add the
last one to the result. In more formal terms:

$$
\begin{eqnarray}
S_n & = & \sum\_{i=1}^{n} x_i\\\\
& = & \sum\_{i=1}^{n - 1} x_i + x_n\\\\
& = & S\_{n-1} + x_n
\end{eqnarray}
$$

This formulation can be translated *verbatim* in OCaml:
```ocaml
let rec sum xs = match xs with
  | [] -> 0
  | h :: t -> h + sum t
;;
```
If the list is empty, we return the neutral element for the addition
(*i.e.* 0) and if it has at least one element we add it to the sum of
the remaining elements.

## Products of integers

Let's now write a function that computes the *product* of the integers
in a list. We can employ a very similar recursive formulation:
```ocaml
let rec product xs = match xs with
  | [] -> 1
  | h :: t -> h * product t
;;
```
Two things have changed:
  - the empty list case now returns the neutral element for the
    product, *i.e.* 1
  - the way the head of the list `h` is combined to the result of
    the recursive call. We now use the product operator `( * )`.

## Concatenation of strings

Interestingly, the function that concatenates all strings from a list
is absolutely similar:
```ocaml
let rec concat xs = match xs with
  | [] -> ""
  | h :: t -> h ^ concat t
;;
```
Now the neutral element is an empty string and the head of the list is
combined to the result of the recursive call using the append operator
for strings `( ^ )`.

## One function to write them all

Since those three functions are so similar, we can in fact see them as
particular instances of a more general function, usually named
`fold`. We have seen that what differs between two functions is a
binary operation and its associated neutral element. Let's set them as
*parameters*:
```ocaml
let rec fold f e xs = match xs with
  | [] -> e
  | h :: t -> f h (fold f e t)
;;
```
The type is now more complex, and displays *type variables*, which
express *typing constraints* between the arguments of `fold`. Let's
consider them from right to left:

  - the third argument is of type `'a list`, meaning we're going to
    process a list of values of type `'a` (in our case, `'a` was first
    `int` then `string`).
  - the second argument is an *initial value* of type `'b` (in our
    case it was successively `0`, `1` and `""`).
  - the first argument is a function to *combine* elements of the
    lists (e.g. integers) with the result we are computing
    (e.g. sum).

Our three functions can now be rewritten in the following way:
```ocaml
let sum xs = fold ( + ) 0 xs;;
let prod xs = fold ( * ) 1 xs;;
let concat xs = fold ( ^ ) "" xs;;
```
That is, once `fold` is available, their implementation becomes very
concise, which lets significantly less opportunities to make mistakes!

## Conclusion

`fold` is basically a general way to perform an *iterative
computation*, like we would do with a loop: we put an initial value in
a accumulator variable, then we consider successively the elements of
the list by updating the variable with its previous value combined
with the next element in the list. It is then *as expressive* as a
loop, but *a lot safer*, since we don't need to take care of the loop
nor handle the accumulator variable correctly.

Note that there are two "natural" ways to iterate over a list (from
left to right, or from right to left), and that is why the standard
library offers two functions, `List.fold_left` and
`List.fold_right`. As an exercice, try to figure out which one it is
that we wrote (beware, there's a trap).

Since a `fold` function is a way to iterate over a collection, we
could write some for other collections like trees, graphs, sets, hash
tables and so on. But that's yet another story.

P.S. A call to `fold` is hidden in this messy code. Will you find it?

```ocaml
let cut_outline path img =
  let area = `O { P.o with P.width = 0.001 } in
  I.cut ~area path img
;;
  
let quadrilateral a b c d p =
  p |>
  P.sub a |>
    P.line b |>
    P.line c |>
    P.line d |>
    P.close
;;

let nested_squares_path n =
  let init =
    (
      V2.v (-. 0.5) (-. 0.5),
      V2.v 0.5 (-. 0.5),
      V2.v 0.5 0.5,
      V2.v (-. 0.5) 0.5
	)
  in
  let f _ (accu, (a, b, c, d)) =
    let a' = V2.( a + 0.1 * (d - a) ) in
    let b' = V2.( b + 0.1 * (a - b) ) in
    let c' = V2.( c + 0.1 * (b - c) ) in
    let d' = V2.( d + 0.1 * (c - d) ) in
	(accu |> quadrilateral a' b' c' d',
	 (a', b', c', d'))
  in	
  List.init 100 ident
  |> fold f (P.empty, init)
  |> fst
;;

let nested_squares =
  cut_outline (nested_squares_path 100) (I.const Color.black)
  |> I.move (V2.v 0.5 0.5)
;;

show nested_squares;;
```
