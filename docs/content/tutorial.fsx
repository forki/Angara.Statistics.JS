(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/Angara.Statistics"
#r "../../packages/docs/Angara.Serialization/lib/net452/Angara.Serialization.dll"
#r "../../packages/docs/Angara.Serialization.Json/lib/net452/Angara.Serialization.Json.dll"
#r "../../packages/docs/Angara.Reinstate/lib/net452/Angara.Reinstate.dll"
#r "../../packages/docs/Angara.Html/lib/net452/Angara.Html.dll"
#r "../../packages/docs/Angara.Chart/lib/net452/Angara.Chart.dll"
#r "Angara.Statistics.dll"

(**

### Distributions

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
let sampling_density_x, sampling_density_y = kde 128 samples
// compute exact probability density function
let analytic_density_y = [|for x in sampling_density_x -> exp(log_pdf distribution x)|]

open Angara.Charting
let chart = 
    Chart.ofList [Plot.line(sampling_density_x, analytic_density_y, thickness=7., stroke="lightgray")
                  Plot.line(sampling_density_x, sampling_density_y)]

(*** include-value:chart ***)
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