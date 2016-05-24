// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open MT

[<EntryPoint>]
let main argv = 
    let rng = MT19937(123.0)
    for i in 0..9 do
        printfn "%A" (rng.uniform_uint32())
    0 // return an integer exit code
