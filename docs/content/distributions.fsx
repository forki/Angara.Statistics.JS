(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/Angara.Statistics"
#r "Angara.Statistics.dll"
(** We will use the following code to present a PDF and a histogram of distributions supported by the package. *)
open Angara.Statistics

let chart d xmin xmax =
    let n, k, mt = 128, 1024, MT19937()
    let h = Seq.init k (fun _ -> draw mt d) |> histogram_ n xmin xmax
    let xh = [|for i in 0..2*n+1 ->
                xmin + float ((i+1)/2) * (xmax - xmin) / float n|]
    let yh = [|for i in 0..2*n+1 ->
                if i=0 || i=2*n+1 then 0. else float(h.[(i-1)/2]) / float k |]
    let xpdf = [|for i in 0..n -> xmin + float i * (xmax - xmin) / float n|]
    let ypdf = Array.map xpdf (fun x -> exp(log_pdf d x))


(**

### Uniform distribution

A `Distribution` is a value which allows to `draw` a pseudo-random number or compute a probability density function `log_pdf`.
The `Distribution` discriminated union supports most common probability distributions like `Uniform` or `Normal`. More details are in a separate document page.

For example, here is how you define normal distribution once you've referenced Angara.Statistics.dll:
*)
open Angara.Statistics

let distribution = Normal(37.0, 9.0)
(**

### Random number generator

To draw random numbers we use [Mersenne Twister](http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html),
one of the most commonly used random number generator.
*)
let generator = MT19937()
let random_values =
    generator.uniform_uint32(),  // low-level output
    generator.uniform_float64(), // a floating point number from [0,1)
    generator.normal()           // standard normal distribution with mean=0 and stdev=1
(*** include-value:random_values ***)
(**
To reproduce the generator state use either exact seed or copy-constructor:
*)
let seed = generator.get_seed()
let g' = MT19937(seed)
let g'' = MT19937(generator)
// the three instances should now produce
// exactly the same sequence of pseudo-random numbers
let test_3 = generator.normal(), g'.normal(), g''.normal()
(*** include-value:test_3 ***)
(**

To draw a number from a non-standard distribution use `draw : MT19337 -> Distribution -> float`.
For example, here we generate an array of 1000 random numbers from the above distribution.
*)
let samples = [| for _ in 1..1000 -> draw generator distribution |]
(**

### Probability density function

One single function `log_pdf : Distribution -> float -> float` computes logarithm of normalized probability density
of any distribution at any valid point.

If you do not have a Distribution but have a sample from the distribution used kernel density estimator
`kde : int -> float seq -> (float[] * float[])`. The estimator needs target number of points at which the density is to be evaluated.
If this number is not a power of two he number of points will be the next larger power of two. The estimator returns two arrays of `x` and `y` values:

The following sample code compares non-parametric density of the sample with the exact density from `log_pdf` function.
*)
(*** define-output:sample ***)
// estimate density curve in 16 points
let sampling_density_x, sampling_density_y = kde 16 samples
// compute exact probability density function
let analytic_density_y = [|for x in sampling_density_x -> exp(log_pdf distribution x)|]
// print the resulting table
printfn ".   x   est.density exact.dnsty"
Array.zip3 sampling_density_x sampling_density_y analytic_density_y
|> Array.iter(fun (x, y1, y2) -> printfn "%6.1f %11.5f %11.5f" x y1 y2)

(** The output of the above script:*)
(*** include-output:sample ***)

(**

### Sample statistics

Use `summary` and `qsummary` functions to quickly compute mean, standard deviation, 95% and 68% quantiles of a sample:
*)
(*** define-output:summary ***)
printfn "%A" (summary samples)
(*** include-output:summary ***)
(*** define-output:qsummary ***)
printfn "%A" (qsummary samples)
(*** include-output:qsummary ***)
(** The `lb95` in the `qsummary` record stands for "lower bound of 95% interval" with is actually a 2.5% quantile. Similarly `ub95` stands for 97.5% quantile. *)