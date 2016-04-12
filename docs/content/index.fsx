(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/Angara.Statistics"

(**
Angara.Statistics
======================

A collection of essential algorithms for Bayesian data constrained modelling.
Includes Mersenne twister random number generator, common probability distributions,
sampling statistics and quantiles, a kernel density estimator and
a resumable Metropolis-Hastings MCMC sampler.

Example
-------

Here is how you draw from normal distribution

*)
#r "Angara.Statistics.dll"
open Angara.Statistics

let generator = MT19937() // Mersenne twister with default seed
let distribution = Normal(37.0, 9.0)
draw generator distribution

(**
See [Probability distributions tutorial](tutorial.html) for more information.

Next snippet shows how to infer a parameter of a Poisson distribution data using MCMC sampler.
*)
(***define-output:poisson***)

let fake_data = [for _ in 1..1000 -> draw generator (Poisson 5.)]

open Angara.Filzbach
let log_likelihood (p:Parameters) =
    let lambda = p.GetValue("lambda")
    Seq.sumBy (fun d -> log_pdf (Poisson lambda) d) fake_data
let prior = Parameters.Empty.Add("lambda",LogUniform(0.001, 1000.0))
let posterior = Sampler.runmcmc(prior, log_likelihood, 1000, 100)
Sampler.print posterior

(** The last line of the above snippet prints credible intervals of the 
posterior distribution of the parameter:*)
(*** include-output:poisson ***)
(** 

See [Filzbach tutorial](tutorial.html) for more information.

Samples & documentation
-----------------------

Documentation includes tutorials automatically generated from `*.fsx` files in [the content folder][content]. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsprojects/Angara.Statistics/tree/master/docs/content
  [gh]: https://github.com/fsprojects/Angara.Statistics
  [issues]: https://github.com/fsprojects/Angara.Statistics/issues
  [readme]: https://github.com/fsprojects/Angara.Statistics/blob/master/README.md
  [license]: https://github.com/fsprojects/Angara.Statistics/blob/master/LICENSE.txt
*)
