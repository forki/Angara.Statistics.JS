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
# Distributions

## Continuous random variables

We will use the following code to present a PDF and a histogram of distributions supported by the package.
This compares output of `draw` and `log_pdf` functions for the same distribution. 

*)
open Angara.Statistics
open Angara.Charting

let chart d xmin xmax =
    let n, k, mt = 128, 131072, MT19937()
    let h = Seq.init k (fun _ -> draw mt d) |> histogram_ n xmin xmax
    let xh = [|for i in 0..2*n+1 ->
                xmin + float (i/2) * (xmax - xmin) / float n|]
    let yh = [|for i in 0..2*n+1 ->
                if i=0 || i=2*n+1 then 0. else float n / (xmax - xmin) * float(h.[(i-1)/2]) / float k |]
    let xpdf = [|for i in 0..n -> xmin + float i * (xmax - xmin) / float n|]
    let ypdf = Array.map (fun x -> exp(log_pdf d x)) xpdf
    Chart.ofList [Plot.line(xpdf, ypdf, thickness=7., stroke="lightgray")
                  Plot.line(xh, yh)]

(**

### Uniform distribution

`Uniform(lower_bound, upper_bound)` signifies a uniform distribution on [lower_bound, upper_bound) interval.
Upper bound must be greater than lower bound.

*)

let chart_uniform =
    let d = Uniform(-1.,2.)
    chart d -1.5 2.5

(*** include-value:chart_uniform ***)
(**

### Log-uniform distribution

`Uniform(lower_bound, upper_bound)`. A uniform distribution of `log(x)` is a non-uniform for `x`.
Both bounds must be greater than zero. 

*)

let chart_loguniform =
    let d = LogUniform(0.5,1.5)
    chart d 0. 2.

(*** include-value:chart_loguniform ***)
(**

### Normal distribution

`Normal(mean, standard_deviation)`. This is often used for real-valued random variables which exact distributions are not known. 

*)

let chart_normal =
    let d = Normal(37., 9.)
    chart d 10. 64.

(*** include-value:chart_normal ***)
(**

### Log-normal distribution

`LogNormal(mean, standard_deviation_log)`. A normal distribution of `log(x)`. The second parameter is a standard deviation of log(x), not a standard deviation of x. 

*)

let chart_lognormal =
    let d = LogNormal(37., 0.3)
    chart d 10. 90.

(*** include-value:chart_lognormal ***)
(**

### Exponential distribution

`Exponential(mean)`
describes the time between events in a process
in which events occur continuously and independently at a constant average rate.
The only parameter must be greater than zero.
Values of an exponentially distributed random variable are always greater than zero.

*)

let chart_exponential =
    let d = Exponential(1.)
    chart d 0. 5.

(*** include-value:chart_exponential ***)
(**

### Gamma distribution

`Gamma(alpha, beta)` - a family of distributions of positive values.
The parameters alpha and beta are sometimes called shape and rate. Both must be greater than zero.
`Gamma(1, 1/lambda) === Exponential(lambda)`.
A special case of Gamma _Γ(k/2, 1/2)_ is a chi-squared distribution _χ²(k)_.

*)

let chart_gamma =
    let d = Gamma(3.0, 0.5)
    chart d 0. 20.

(*** include-value:chart_gamma ***)
(**

## Descrete distributions

### Bernoulli
### Binomial
### Negative binomial distribution
### Poisson

## Mixture

`Mixture([w1,d1; w2,d2; ...])`, where w1, w2, ... - weights (real numbers) and d1, d2, ... - distributions.
The list must not be empty and sum of the weights must be equal to one.

*)
let chart_mixture =
    let d = Mixture([0.9,Normal(37.,9.); 0.1,Uniform(15.,20.)])
    chart d 10. 64.

(*** include-value:chart_mixture ***)

