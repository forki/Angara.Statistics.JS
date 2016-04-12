(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/Angara.Statistics"

(**
Introducing your project
========================

Say more

*)
(*** define-output:sample ***)
#r "Angara.Statistics.dll"
open Angara.Statistics

let distribution = Normal(37.0, 9.0)
let generator = MT19937()
let samples = [| for _ in 1..1000 -> draw generator distribution |]
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
Some more info
*)
