[<ReflectedDefinition>]
module Statistics

/// π
let pi = 3.14159265358979323846264338327950288

/// 2π
let pi2 = 6.283185307179586476925286

/// natural logarithm base
let e = 2.71828182845904523536028747135266250

/// The smallest positive normalized `float` value
let improbable = 2.2250738585072014E-308 // 2^(-1022)
/// Logarithm of `improbable`
let log_improbable = log(improbable) // -708

/// `1.0 - tolerance < 1.0 && 1.0 - 0.5*tolerance = 1.0`
let tolerance = 1.1102230246251565E-16 // 2^(-53)
/// Logarithm of `tolerance`
let log_tolerance = log(tolerance) // -36.7

#if JavaScript
type Complex(real:float, imaginary:float) =                    
    let re = real
    let im = imaginary
    member private self.im = 0.0
    member self.Real
        with get() =
            re
    member self.Imaginary
        with get() =
            im
    static member (+) (x: Complex,y: Complex) =
        Complex(x.Real+y.Real,x.Imaginary+y.Imaginary)
    static member (-) (x: Complex,y: Complex) =
        Complex(x.Real-y.Real,x.Imaginary-y.Imaginary)
    static member (*) (x: Complex,y: Complex) =
        Complex(x.Real*y.Real-x.Imaginary*y.Imaginary,x.Imaginary*y.Real+x.Real*y.Imaginary)
    static member ImgExp (x:float) = //e^Complex(0.0,x)
        Complex(cos(x),sin(x))
        
#else
open System.Numerics
#endif

let private Radix2Reorder(samples:'T[]) =
            let mutable j = 0
            for i in 0..samples.Length - 2 do
                if (i < j) then
                    let temp = samples.[i]
                    samples.[i] <- samples.[j]
                    samples.[j] <- temp

                let mutable m = samples.Length
                let mutable cont = true
                while cont do
                    m <- m >>> 1;
                    j <- j ^^^ m;
                    cont <- (j &&& m) = 0

let private Radix2Step(samples:Complex[], exponentSign:int, levelSize:int, k:int) =
            // Twiddle Factor
            let exponent = (float exponentSign*float k)*pi/float levelSize
            let w = Complex(cos(exponent), sin(exponent))
            let step = levelSize <<< 1
            for i in k..step..samples.Length-1 do
                let ai = samples.[i]
                let t = Complex.op_Multiply(w,samples.[i + levelSize])
                samples.[i] <- Complex.op_Addition(ai,t)
                samples.[i + levelSize] <- Complex.op_Subtraction(ai,t)

let private Radix2(samples:Complex[], exponentSign:int) =
            let rec is_power_two x p =
                if x = p then true
                elif x < p then false
                else is_power_two x (2*p)
            if not <| is_power_two samples.Length 1 then failwith "samples: The array length must be a power of 2." 

            Radix2Reorder(samples)
            let mutable levelSize = 1 
            while levelSize < samples.Length do
                for k = 0 to levelSize-1 do Radix2Step(samples, exponentSign, levelSize, k)
                levelSize <- levelSize * 2

let private fi x = float(x)

/// Inverse Fast Fourier Transform.
let ifft (xs:Complex[]) = 
    let samples = Array.copy xs
    Radix2(samples,1)
    let scalingFactor = 1.0/(float samples.Length)
    for i in 0..samples.Length-1 do
        let v = samples.[i]
        samples.[i] <- Complex(scalingFactor * v.Real, scalingFactor * v.Imaginary)
    samples

/// Fast Fourier transform. 
let fft (xs:Complex[]) = 
    let samples = Array.copy xs
    Radix2(samples,-1)
    samples

let private (|Even|Odd|) input = if input % 2 = 0 then Even else Odd

/// Descrete cosine transform.
let dct (rxs:float[]) = 
    let xs = Array.map (fun x -> Complex(x,0.0)) rxs
    let len = xs.Length
    let n = fi len
    let weights = 
        let myseq = Seq.init (len-1) (fun x -> Complex.op_Multiply(Complex(2.0, 0.0),Complex.ImgExp(fi (x+1) * pi / (2.0*n))))
        seq { yield Complex(2.0, 0.0); yield! myseq } |> Array.ofSeq
    let backpermute (arr:Complex[]) ind = ind |> Seq.map (fun i -> arr.[i])
    let interleaved = 
        let en = Seq.init ((len+1)/2) (fun i -> i * 2)
        let en2 = Seq.init (len/2) (fun i -> len - (i*2) - 1)
        backpermute xs (seq{ yield! en; yield! en2 }) |> Array.ofSeq

    Array.map2 (fun (a:Complex) (b:Complex) -> (Complex.op_Multiply(a,b)).Real) weights (fft interleaved)


/// Inverse discrete cosine transform.
// idct :: U.Vector CD -> U.Vector Double
// http://hackage.haskell.org/package/statistics-0.10.0.0/docs/src/Statistics-Transform.html
let idct (rxs:float[]) = 
    let xs = Array.map (fun x -> Complex(x,0.0)) rxs
    let len = rxs.Length
    let weights = 
        let n = fi len
        let coeff k = Complex.op_Multiply(Complex(2.0 * n, 0.0),Complex.ImgExp(fi (k+1) * pi / (2.0*n)))
        [| yield Complex(n,0.0); for i in 0..len-2 do yield coeff i |]
    let vals = (Array.map (fun (c:Complex) -> c.Real) << ifft) (Array.map2 (fun x y -> Complex.op_Multiply(x,y)) weights xs )
    let interleave z =
        let hz = z >>> 1
        match z with
            | Even _ -> vals.[hz]
            | Odd _ -> vals.[len - hz - 1]
    [| for i in 0..len-1 do yield interleave(i) |]

// http://hackage.haskell.org/package/statistics-0.10.5.0/docs/src/Statistics-Function.html#nextHighestPowerOfTwo
//
// Efficiently compute the next highest power of two for a
// non-negative integer.  If the given value is already a power of
// two, it is returned unchanged.  If negative, zero is returned.
let private nextHighestPowerOfTwo n =
  let   i0   = n - 1
  let   i1   = i0  ||| (i0 >>> 1)
  let   i2   = i1  ||| (i1 >>> 2)
  let   i4   = i2  ||| (i2 >>> 4)
  let   i8   = i4  ||| (i4 >>> 8)
  let   i16  = i8  ||| (i8 >>> 16)
  let _i32 = i16 ||| (i16 >>> 32)
  1 + _i32

let private isNan = System.Double.IsNaN
let private isInf = System.Double.IsInfinity

let histogram_ n xmin xmax xs =
    if n < 1 then failwith "n: must be > 0"
    let isNotFinite x = System.Double.IsNaN x || System.Double.IsInfinity x
    if  isNotFinite xmin then failwith (sprintf "xmin: is %e" xmin)
    if isNotFinite xmax then failwith (sprintf "xmax: is %e" xmax)
    if xmin >= xmax then failwith "xmin should be less than xmax"
    let h = Array.zeroCreate n
#if JavaScript
    Array.iteri (fun i _ -> h.[i] <- 0) h //zeroCreate does not put zeros if tranlated with WebSharper
#endif
    let step = (xmax - xmin) / float n
    let add x =
        if not(isNan x) && x >= xmin && x <= xmax then
            let idx = min (n-1) (int((x-xmin)/step))
            h.[idx] <- h.[idx] + 1
    xs |> Array.iter add
    h

#if JavaScript
let within (ulps:uint32) (a:float) b =
    System.Math.Abs(a-b)<tolerance
#else

/// Approximate comparison of two double values.
/// Tolerance `ulps` is in units of least precision.
let within (ulps:uint32) a b =
    // See e.g. "Comparing Floating Point Numbers, 2012 Edition" by  Bruce Dawson
    // https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
    let ai = System.BitConverter.DoubleToInt64Bits a
    let bi = System.BitConverter.DoubleToInt64Bits b
    let cmp ai bi = if ai<=bi then bi-ai <= int64 ulps else ai-bi <= int64 ulps
    if ai<0L && bi>=0L then cmp (System.Int64.MinValue-ai) bi
    elif ai>=0L && bi<0L then cmp ai (System.Int64.MinValue-bi)
    else cmp ai bi 

#endif

/// Root of a function using Ridders method.
//   Ridders, C.F.J. (1979) A new algorithm for computing a single
//   root of a real continuous function.
//   /IEEE Transactions on Circuits and Systems/ 26:979--980.
let ridders tolerance (lb, ub) (f : float->float) =
    // The function must have opposite signs when evaluated at the lower 
    // and upper bounds of the search (i.e. the root must be bracketed). 

    let rec iter a fa b fb i =
        if 100 <= i then None // Too many iterations performed. Fail
        else
        if within 1u a b then Some a // Root is bracketed within 1 ulp. No improvement could be made
        else
        let d = abs(b-a)
        let dm = (b-a) * 0.5
        let m = a + dm
        let fm = f m
        if 0.0 = fm then Some m else
        let dn = float(sign(fb - fa)) * dm * fm / sqrt(fm*fm - fa*fb)
        let n = m - float(sign dn) * min (abs dn) (abs dm - 0.5 * tolerance)
        if d < tolerance then Some n else
        if n=a || n=b then
            // Ridder's approximation coincide with one of old bounds. Revert to bisection 
            if 0 > sign fm * sign fa then iter a fa m fm (i+1)
            else iter m fm b fb (i+1)
        else
        let fn = f n
        if 0.0 = fn then Some n
        elif 0.0 > fn*fm then iter n fn m fm (i+1)
        elif 0.0 > fn*fa then iter a fa n fn (i+1)
        else iter n fn b fb (i+1)
                    
    if not (tolerance>=0.0) then failwith "tolerance must be greater than 0.0"
    let flb  = f lb
    if 0.0 = flb then Some lb else
    let fub = f ub
    if 0.0 = fub then Some ub
    elif 0.0 < fub*flb then None // root is not bracketed
    else iter lb flb ub fub 0


// from http://hackage.haskell.org/package/statistics-0.10.5.0/docs/src/Statistics-Sample-KernelDensity.html#kde
//
/// Gaussian kernel density estimator for one-dimensional data, using
/// the method of Botev et al.
//
// Botev. Z.I., Grotowski J.F., Kroese D.P. (2010). Kernel density estimation via diffusion. 
// /Annals of Statistics/ 38(5):2916-2957. <http://arxiv.org/pdf/1011.2602>
//
// The result is a pair of vectors, containing:
//
// * The coordinates of each mesh point.
//
// * Density estimates at each mesh point.
//
// n0 The number of mesh points to use in the uniform discretization
// of the interval @(min,max)@.  If this value is not a power of
// two, then it is rounded up to the next power of two.
//
// min Lower bound (@min@) of the mesh range.
// max Upper bound (@max@) of the mesh range.
// NaN in the sample are ignored.
let kde2 n0 min max (sample:float array) =
    // check kde2 arguments
    if sample = null then failwith "sample cannot be null"
    else if(n0 = 1) then failwith "n0 cannot be 1"
    else
        let xs = Array.filter (System.Double.IsNaN >> not) sample
        if Array.isEmpty xs then failwith "sample: doesn't contain numeric values"
        let m_sqrt_2_pi = sqrt (2.0*pi)
        let m_sqrt_pi = sqrt pi
        let r = max - min
        let len = fi xs.Length
        let ni = nextHighestPowerOfTwo n0    
        let n = fi ni
        let sqr a = a*a
        let mesh = 
            let d = r/(n - 1.0)
            Array.init ni (fun z -> min + d * fi z)

        let density =
            let a = 
                let h = Array.map (fun x -> float(x) / len) (histogram_ ni min max xs)
                let sh = Array.sum h
                (dct << Array.map (fun p -> p/sh)) h        
        
            let iv = [| for i in 1..ni-1 do yield sqr(fi i) |]
            let a2v = a |> Array.skip(1) |> Array.map (fun q -> sqr(q*0.5))
            let t_star =                         
                let rec f q t = 
                    let g i a2 =  i ** q * a2 * exp ((-i) * sqr(pi) * t)                
                    2.0 * pi ** (q*2.0) * Array.sum (Array.map2 g iv a2v)
                let rec go s h : float = 
                    let si = fi s
                    let k0 = 
                        let enum = seq{ for i in 1 .. s do yield 2*i - 1 } 
                        fi(Seq.fold (*) 1 enum) / m_sqrt_2_pi
                    let _const = (1.0 + 0.5 ** (si+0.5)) / 3.0
                    let time = (2.0 * _const * k0 / len / h) ** (2.0 / (3.0 + 2.0 * si))
                    if s=1 then h else go (s-1) (f si time) 
            
                let eq x = x - (len * (2.0 * m_sqrt_pi) * go 6 (f 7.0 x)) ** (-0.4)
                match ridders 1e-14 (0.0,0.1) eq with Some root -> root | None -> (0.28 * len ** (-0.4))
                
            let f2 b z = b * exp (sqr z * sqr pi * t_star * (-0.5)) 
            let a2 = Array.map2 f2 a [| for i in 0..ni-1 do yield fi i |]
            let a1 = idct a2
            let a0 = Array.map (fun x -> x / (2.0*r)) a1
            a0
        (mesh, density)

/// Gaussian kernel density estimator for one-dimensional data, using
/// the method of Botev et al.
//
// The result is a pair of vectors, containing:
//
// * The coordinates of each mesh point.  The mesh interval is chosen
//   to be 20% larger than the range of the sample.  (To specify the
//   mesh interval, use 'kde2'.)
//
// * Density estimates at each mesh point.
//
// n0 The number of mesh points to use in the uniform discretization
// of the interval @(min,max)@.  If this value is not a power of
// two, then it is rounded up to the next power of two.
let kde n0 (xs:float array) =
    if(xs = null) then failwith "sample: cannot be empty"
    else
        let mutable max = System.Double.MinValue
        let mutable min = System.Double.MaxValue
        let range =         
            if Array.isEmpty xs then 
                min <- 0.0
                max <- 0.0
                1.0  // unreasonable guess
            else
                xs |> Array.iter (fun xsi -> 
                  if max < (xsi) then 
                       max <- xsi
                  if min > (xsi) then 
                       min <- xsi)
                if min >= max then 1.0 else max - min
        kde2 n0 (min - range/10.0) (max + range/10.0) xs
