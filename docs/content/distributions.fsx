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
# Supported probability distributions

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

`Uniform(lower_bound, upper_bound)` signifies a uniform probability distribution on [lower\_bound, upper\_bound) interval.
Upper bound must be greater than lower bound.

*)

let chart_uniform =
    let d = Uniform(-1.,2.)
    chart d -1.5 2.5

(*** include-value:chart_uniform ***)
(**

### Log-uniform distribution

`Uniform(lower_bound, upper_bound)`.
A uniform distribution of `log(x)` is a non-uniform for `x`.
Both bounds must be greater than zero. 

*)

let chart_loguniform =
    let d = LogUniform(0.5,1.5)
    chart d 0. 2.

(*** include-value:chart_loguniform ***)
(**

### Linear distribution

`Linear(lower_bound, upper_bound, density_at_lower_bound)`.
Density function of this distribution is linear on a [lower\_bound, upper\_bound) range and is 'improbable' outside of it.
Normalization condition gives us density value at upper bound: density\_at\_upper\_bound = density\_at\_lower\_bound + 2/(upper\_bound - lower\_bound).
Density must be positive on both sides which restricts their possible values.
If `density_at_lower_band` is outside of the permissible range, it is brought to the nearest permissible value.

This distribution is useful as a component of `Mixture` (see below).

*)

let chart_linear =
    let d = Linear(-1., 2., 0.2)
    chart d -1.5 2.5

(*** include-value:chart_linear ***)
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

`LogNormal(mean, standard_deviation_log)`. A normal distribution of `log(x)`.
The second parameter is a standard deviation of log(x), not a standard deviation of x. 

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
    let d = Exponential(5.7)
    chart d 0. 20.

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

For these distributions `draw` function always returns non-negative random values with zero fraction part.
Probability distribution function `log_pdf` truncates franction part of its argument.
The only exception is Bernoulli distribution in which case `log_pdf` treats all values of its argument `x > 0.5` as `1`,
and the rest argument valus are treated as `0`.

The following code presents mass distribution and a histogram of 
discrete probability distributions supported by the package.
This compares output of `draw` and `log_pdf` functions for the same distribution. 

*)

let discrete_chart d =
    let n, k, mt = 20, 20480, MT19937()
    let h = Array.create (n+1) 0
    seq {1..k} |> Seq.iter (fun _ ->
        let i = int(draw mt d) in if i<=n then h.[i] <- h.[i]+1)
    let xh = [|for i in 0..3*n+2 ->
                if i%3=2 then nan 
                else float(i/3)|]
    let yh = [|for i in 0..3*n+2 ->
                if i%3=0 then 0. 
                elif i%3=1 then float(h.[i/3])/float k 
                else nan |]
    let ypdf = Array.mapi (fun i x ->
        if i%3=0 then 0.0 else if i%3=2 then nan else exp(log_pdf d x)) xh
    Chart.ofList [Plot.line(xh, ypdf, thickness=7., stroke="lightgray")
                  Plot.line(xh, yh)]

(**

### Bernoulli

`Bernoulli(mean)` denotes distribution of a yes/no experiment (1 or 0) which yields success with probability `mean`.
Note that `log_pdf` function returns the same value for all 'x > 0.5'; 

*)
let chart_bernoulli =
    let d = Bernoulli(0.7)
    discrete_chart d

(*** include-value:chart_bernoulli ***)
(**
### Binomial

`Binomial(n, p)` is a number of successes
in a sequence of `n` independent yes/no experiments, each of which yields success with probability `p`.
*)
let chart_binomial =
    let d = Binomial(20, 0.7)
    discrete_chart d

(*** include-value:chart_binomial ***)
(**
### Negative binomial distribution

`NegativeBinomial(mean, r)` is a number of successes before a given number of failures `r`
in a sequence of yes/no experiments, each of which yields success with probability `p = mean/(mean+r)`.
*)
let chart_negative_binomial =
    let d = NegativeBinomial(5.7, 7.5)
    discrete_chart d

(*** include-value:chart_negative_binomial ***)
(**
### Poisson

`Poisson(mean)` is a number of events occuring in a fixed interval of time if these events occur with a known average rate = `mean`.

*)
let chart_poisson =
    let d = Poisson(5.7)
    discrete_chart d

(*** include-value:chart_poisson ***)
(**
## Mixture

`Mixture([w1,d1; w2,d2; ...])`, where w1, w2, ... - weights (real numbers) and d1, d2, ... - distributions.
The list must not be empty and sum of the weights must be equal to one.

*)
let chart_mixture =
    let d = Mixture([0.9,Normal(37.,9.); 0.1,Uniform(15.,20.)])
    chart d 10. 64.

(*** include-value:chart_mixture ***)

