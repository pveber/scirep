How to read the lines of a file
===============================

Many text file format are *line-oriented*, that is to say that lines
are basic units of their syntax. Examples include tabular formats like
CSV and its specialized variants (TSV, BED, GFF for instance) or
sequence formats like FASTQ, FASTA or SAM. File formats that are not
line-oriented often display a syntax including balanced-parenthesis
structures, like instance JSON, XML, and virtually all programming
languages. In latter are more difficult to parse, but can express
richer expressions.

In this note we show how to read the lines of a file, and how to
consume them to produce a result.

## Basic operations

The standard library of OCaml provides function to perform the basic
system calls to interact with a file, that is 

(1) opening a file

```ocaml
open_in;;
```

(2) reading from a file

```ocaml
input_char;;
```

(3) and finally closing a file

```ocaml
close_in;;
```

The central type here is called `in_channel` and represents a *file
handler* for reading (there's also `out_channel` for writing into
files). As an application, let's see how we count characters in a file
with this:

```ocaml
let character_count path =
  let ic = open_in path in
  let rec loop c =
    match input_char ic with
    | exception End_of_file -> c
    | _ -> loop (c + 1)
  in
  let r = loop 0 in
  close_in ic ;
  r
;;
```

It is both very important to remember and very easy to forget calling
`close_in` at the end of the function. If we don't call `close_in`, it
will eventually be called automatically when the OCaml garbage
collector disposes of `ic`. We don't know when it will happen though
(although there are means to make it happen when desired), and in the
meantime the file remains opened. Problem is, a process may only have
a (rather) limited number of simultaneously opened files. After that
number is reached, further attempts to open will fail.

## Factoring the code for opening files.

Even if we don't forget to call `close_in`, the same problem can
happen in case an exception is raised between `open_in` and
`close_in`, thus preventing the release of the resource (the file
handler). It is necessary to check if an exception has occurred while
reading the file and if so, to close it before letting the exception
be handled. So a safer version of our `character_count` function would
be:

```ocaml
let safer_character_count path =
  let ic = open_in path in
  let rec loop c =
    match input_char ic with
    | exception End_of_file -> c
    | _ -> loop (c + 1)
  in
  try
    let r = loop 0 in
    close_in ic ;
    r
  with exn -> (
      close_in ic ;
      raise exn
    )
;;
```

That's a lot to remember, and the specific logic of our function is
now obfuscated by the whole open-and-close-even-in-case-of-exception
story. Hopefully, it is possible to factor it:

```ocaml
let with_file fn ~f =
  let ic = open_in fn in
  try
    let r = f ic in
    close_in ic ; r
  with exn -> (
      close_in ic ;
      raise exn
    )
;;
```

And then write our `character_count` function more simply:

```ocaml
let character_count fn =
  let rec loop ic c =
    match input_char ic with
    | exception End_of_file -> c
    | _ -> loop ic (c + 1)
  in
  with_file fn ~f:(fun ic -> loop ic 0)
;;
```

Cleaner, right? If you're using `stdio`, `core_kernel` or `core`, you
can use `In_channel.with_file`; if you're using `containers`, there's
`CCIO.with_in`.

## Higher level functions for line-oriented formats

We can generalize a bit our last `character_count` function to obtain
a very useful one:

```ocaml
let fold_lines fn ~init ~f =
  let rec loop ic acc =
    match input_line ic with
    | l -> loop ic (f acc l)
    | exception End_of_file -> acc
  in
  with_file fn ~f:(fun ic -> loop ic init)
;;
```

For instance counting lines of a file is now as simple as 

```ocaml
let count_lines fn =
  fold_lines fn ~init:0 ~f:(fun n _ -> n + 1)
;;
```

Another insteresting variant would be a `foldi` function that provides
the line number to the function it iterates. It is left as an exercise
to the reader! A nice property of our `fold_lines` function is that we
can inspect the contents of a file without loading its contents all at
once, and still write our code in a functional way.

## More sophisticated stuff

As a teaser for more caml riding, I would like to mention **streaming
data structures**. The `fold_lines` function we wrote is handy for
many situations, but it has some limitations. It can easily be
specialized (e.g. to count lines, or fold on lines that do not start
with a comment symbol, or fold lines two by two) but all those
specializations are not very easy to construct and compose. Moreover,
it only handles the reading part, but it is often the case that we'd
like to also write an output file while reading the input.

The [streaming](https://github.com/odis-labs/streaming) library
offers a very good API to do this, here is an example:

```ocaml
let filter_empty_lines input output = 
  let open Streaming in
  Stream.of_file input
  |> Stream.filter (( <> ) "")
  |> Stream.to_file output
;;
```
