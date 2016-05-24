[<ReflectedDefinition>]
module MT

#if JavaScript
open WebSharper
#endif

[<Inline "$0">]
let inline int64 x = int64 x

[<Inline "$0">]
let inline int32 x = int32 x

[<Inline "$0">]
let inline uint32 x = uint32 x

[<Inline "$0">]
let inline uint64 x = uint64 x

let mod32 = 4294967296.0 //2^32

let coerceU32 x = (x+mod32)%mod32

// http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/CODES/mt19937ar.c
//
//   Adopted from a C-program for MT19937, with initialization improved 2002/1/26.
//   Coded by Takuji Nishimura and Makoto Matsumoto.
//
//   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
//   All rights reserved.                          
//
//   Redistribution and use in source and binary forms, with or without
//   modification, are permitted provided that the following conditions
//   are met:
//
//     1. Redistributions of source code must retain the above copyright
//        notice, this list of conditions and the following disclaimer.
//
//     2. Redistributions in binary form must reproduce the above copyright
//        notice, this list of conditions and the following disclaimer in the
//        documentation and/or other materials provided with the distribution.
//
//     3. The names of its contributors may not be used to endorse or promote 
//        products derived from this software without specific prior written 
//        permission.
//
//   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
//   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
//   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
//   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
//   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
//   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
//   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
//   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
//   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//
//   Any feedback is very welcome.
//   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
//   email: m-mat @ math.sci.hiroshima-u.ac.jp (remove space)

let N = 624
let M = 397
let MATRIX_A = float(0x9908b0dfu)   // constant vector a
let UPPER_MASK = float(0x80000000u) // most significant w-r bits
let LOWER_MASK = float(0x7fffffffu) // least significant r bits

[<Direct "$value >>> $shift">]
let shiftRightUint32 (value:float) (shift:int) =
    coerceU32(float((uint32(value) >>> shift)))

[<Direct "$left ^ $right">]
let xorUint32 (left:float) (right:float) =
    coerceU32(float(uint32(left) ^^^ uint32(right)))

let mulUint32 (left:float) (right:float) =
    coerceU32(float(uint32(left) * uint32(right)))

[<Direct "$value >>> $shift">]
let shiftRightInt32 (value:int32) shift =
    value >>> shift

// initializes mt[N] with a seed
let init_genrand (s:float) =
    let mt:float[] = Array.zeroCreate N // the array for the state vector
    mt.[0] <- float ((uint32 s) &&& 0xffffffffu)
    for mti = 1 to N-1 do
        let fmti = float mti
        let a = mulUint32 (float 1812433253u) (float(uint32(xorUint32 (mt.[mti-1]) (shiftRightUint32 (mt.[mti-1]) 30))))
        mt.[mti] <- coerceU32(a + fmti)
            // See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. 
            // In the previous versions, MSBs of the seed affect 
            // only MSBs of the array mt[].
            // 2002/01/09 modified by Makoto Matsumoto
        mt.[mti] <- coerceU32(float(uint32(mt.[mti]) &&& 0xffffffffu))
        // for >32 bit machines
    mt

let init_by_array (init_key:float[]) =
        let mt = init_genrand(float(19650218u))
        let mutable i = 1
        let mutable j = 0
        let key_length = Array.length init_key
        let len = max key_length N
        for k = 1 to len do
            let k = len - k + 1
            let a = (xorUint32 (mt.[i]) (coerceU32((xorUint32 (float(mt.[i-1])) (shiftRightUint32 (mt.[i-1]) 30)) * float(1664525u))))
            mt.[i] <- coerceU32(float(a) + init_key.[j] + float(j)) // non linear
            mt.[i] <- float(uint32(mt.[i]) &&& 0xffffffffu) // for WORDSIZE > 32 machines
            i <- i + 1
            j <- j + 1
            if i >= N then 
                mt.[0] <- mt.[N-1]
                i <- 1
            if j >= key_length then j <- 0
        let len = N-1
        for k = 1 to len do
            let k = len - k + 1
            let a = (uint32(mt.[i]) ^^^ ((uint32(mt.[i-1]) ^^^ (shiftRightUint32 (uint32(mt.[i-1])) 30)) * 1566083941u))
            mt.[i] <- coerceU32(float(a) - float(i)) // non linear
            mt.[i] <- float(uint32(mt.[i]) &&& 0xffffffffu) // for WORDSIZE > 32 machines
            i <- i + 1
            if i >= N then 
                mt.[0] <- mt.[N-1]
                i <- 1

        mt.[0] <- float 0x80000000u //* MSB is 1; assuring non-zero initial array */ 
        mt

type MT19937 private (
                        mt:float[], // the array for the state vector
                        idx:int      // index of the next word from the state (0..N)
                        ) =
    // Period parameters

    let mutable mti = idx // mti==N+1 means mt[N] is not initialized

    // generates a random number on [0,0xffffffff]-interval 
    let genrand_int32() : float =
        let mutable y = 0.0 //uint32
        let mag01 = [|float(0x0u); MATRIX_A|]
        // mag01[x] = x * MATRIX_A  for x=0,1

        if (mti >= N) then // generate N words at one time
            for  kk=0 to N-M-1 do
                y <- float((uint32(mt.[kk]) &&& uint32(UPPER_MASK)) ||| (uint32(mt.[kk+1]) &&& uint32(LOWER_MASK)))
                let a = uint32(mt.[kk+M])
                let b = (shiftRightUint32 (uint32(y)) 1)
                let c = uint32(mag01.[int(uint32(y) &&& 0x1u)])
                mt.[kk] <- float(a ^^^ b ^^^ c)
            for kk = N-M to N-2 do
                y <- float((uint32(mt.[kk]) &&& uint32(UPPER_MASK)) ||| (uint32(mt.[kk+1]) &&& uint32(LOWER_MASK)))
                mt.[kk] <- float(uint32(mt.[kk+(M-N)]) ^^^ (shiftRightUint32 (uint32(y)) 1) ^^^ uint32(mag01.[int(uint32(y) &&& 0x1u)]))
            y <- float((uint32(mt.[N-1]) &&& uint32(UPPER_MASK)) ||| (uint32(mt.[0]) &&& uint32(LOWER_MASK)))
            mt.[N-1] <- float(uint32(mt.[M-1]) ^^^ (shiftRightUint32 (uint32(y)) 1) ^^^ uint32(mag01.[int(uint32(y) &&& 0x1u)]));

            mti <- 0
  
        y <- mt.[mti]
        mti <- mti + 1

        // Tempering
        y <- float(uint32(y) ^^^ (shiftRightUint32 (uint32(y)) 11))
        y <- float(uint32(y) ^^^ ((uint32(y) <<< 7) &&& 0x9d2c5680u))
        y <- float(uint32(y) ^^^ ((uint32(y) <<< 15) &&& 0xefc60000u))
        y <- float(uint32(y) ^^^ (shiftRightUint32 (uint32(y)) 18))

        y

    // generates a random number on [0,1)-real-interval
    let genrand_float() =
        float(genrand_int32())*(1.0/4294967296.0) 
        // divided by 2^32

    // tables for ziggurat algorithm http://www.boost.org/doc/libs/1_60_0/boost/random/normal_distribution.hpp
    let table_x = [|
        3.7130862467403632609; 3.4426198558966521214; 3.2230849845786185446; 3.0832288582142137009;
        2.9786962526450169606; 2.8943440070186706210; 2.8231253505459664379; 2.7611693723841538514;
        2.7061135731187223371; 2.6564064112581924999; 2.6109722484286132035; 2.5690336259216391328;
        2.5300096723854666170; 2.4934545220919507609; 2.4590181774083500943; 2.4264206455302115930;
        2.3954342780074673425; 2.3658713701139875435; 2.3375752413355307354; 2.3104136836950021558;
        2.2842740596736568056; 2.2590595738653295251; 2.2346863955870569803; 2.2110814088747278106;
        2.1881804320720206093; 2.1659267937448407377; 2.1442701823562613518; 2.1231657086697899595;
        2.1025731351849988838; 2.0824562379877246441; 2.0627822745039633575; 2.0435215366506694976;
        2.0246469733729338782; 2.0061338699589668403; 1.9879595741230607243; 1.9701032608497132242;
        1.9525457295488889058; 1.9352692282919002011; 1.9182573008597320303; 1.9014946531003176140;
        1.8849670357028692380; 1.8686611409895420085; 1.8525645117230870617; 1.8366654602533840447;
        1.8209529965910050740; 1.8054167642140487420; 1.7900469825946189862; 1.7748343955807692457;
        1.7597702248942318749; 1.7448461281083765085; 1.7300541605582435350; 1.7153867407081165482;
        1.7008366185643009437; 1.6863968467734863258; 1.6720607540918522072; 1.6578219209482075462;
        1.6436741568569826489; 1.6296114794646783962; 1.6156280950371329644; 1.6017183802152770587;
        1.5878768648844007019; 1.5740982160167497219; 1.5603772223598406870; 1.5467087798535034608;
        1.5330878776675560787; 1.5195095847593707806; 1.5059690368565502602; 1.4924614237746154081;
        1.4789819769830978546; 1.4655259573357946276; 1.4520886428822164926; 1.4386653166774613138;
        1.4252512545068615734; 1.4118417124397602509; 1.3984319141236063517; 1.3850170377251486449;
        1.3715922024197322698; 1.3581524543224228739; 1.3446927517457130432; 1.3312079496576765017;
        1.3176927832013429910; 1.3041418501204215390; 1.2905495919178731508; 1.2769102735516997175;
        1.2632179614460282310; 1.2494664995643337480; 1.2356494832544811749; 1.2217602305309625678;
        1.2077917504067576028; 1.1937367078237721994; 1.1795873846544607035; 1.1653356361550469083;
        1.1509728421389760651; 1.1364898520030755352; 1.1218769225722540661; 1.1071236475235353980;
        1.0922188768965537614; 1.0771506248819376573; 1.0619059636836193998; 1.0464709007525802629;
        1.0308302360564555907; 1.0149673952392994716; 0.99886423348064351303; 0.98250080350276038481;
        0.96585507938813059489; 0.94890262549791195381; 0.93161619660135381056; 0.91396525100880177644;
        0.89591535256623852894; 0.87742742909771569142; 0.85845684317805086354; 0.83895221428120745572;
        0.81885390668331772331; 0.79809206062627480454; 0.77658398787614838598; 0.75423066443451007146;
        0.73091191062188128150; 0.70647961131360803456; 0.68074791864590421664; 0.65347863871504238702;
        0.62435859730908822111; 0.59296294244197797913; 0.55869217837551797140; 0.52065603872514491759;
        0.47743783725378787681; 0.42654798630330512490; 0.36287143102841830424; 0.27232086470466385065;
        0.
    |]

    let table_y = [|
        0.; 0.0026696290839025035092; 0.0055489952208164705392; 0.0086244844129304709682;
        0.011839478657982313715; 0.015167298010672042468; 0.018592102737165812650; 0.022103304616111592615;
        0.025693291936149616572; 0.029356317440253829618; 0.033087886146505155566; 0.036884388786968774128;
        0.040742868074790604632; 0.044660862200872429800; 0.048636295860284051878; 0.052667401903503169793;
        0.056752663481538584188; 0.060890770348566375972; 0.065080585213631873753; 0.069321117394180252601;
        0.073611501884754893389; 0.077950982514654714188; 0.082338898242957408243; 0.086774671895542968998;
        0.091257800827634710201; 0.09578784912257815216; 0.10036444102954554013; 0.10498725541035453978;
        0.10965602101581776100; 0.11437051244988827452; 0.11913054670871858767; 0.12393598020398174246;
        0.12878670619710396109; 0.13368265258464764118; 0.13862377998585103702; 0.14361008009193299469;
        0.14864157424369696566; 0.15371831220958657066; 0.15884037114093507813; 0.16400785468492774791;
        0.16922089223892475176; 0.17447963833240232295; 0.17978427212496211424; 0.18513499701071343216;
        0.19053204032091372112; 0.19597565311811041399; 0.20146611007620324118; 0.20700370944187380064;
        0.21258877307373610060; 0.21822164655637059599; 0.22390269938713388747; 0.22963232523430270355;
        0.23541094226572765600; 0.24123899354775131610; 0.24711694751469673582; 0.25304529850976585934;
        0.25902456739871074263; 0.26505530225816194029; 0.27113807914102527343; 0.27727350292189771153;
        0.28346220822601251779; 0.28970486044581049771; 0.29600215684985583659; 0.30235482778947976274;
        0.30876363800925192282; 0.31522938806815752222; 0.32175291587920862031; 0.32833509837615239609;
        0.33497685331697116147; 0.34167914123501368412; 0.34844296754987246935; 0.35526938485154714435;
        0.36215949537303321162; 0.36911445366827513952; 0.37613546951445442947; 0.38322381105988364587;
        0.39038080824138948916; 0.39760785649804255208; 0.40490642081148835099; 0.41227804010702462062;
        0.41972433205403823467; 0.42724699830956239880; 0.43484783025466189638; 0.44252871528024661483;
        0.45029164368692696086; 0.45813871627287196483; 0.46607215269457097924; 0.47409430069824960453;
        0.48220764633483869062; 0.49041482528932163741; 0.49871863547658432422; 0.50712205108130458951;
        0.51562823824987205196; 0.52424057267899279809; 0.53296265938998758838; 0.54179835503172412311;
        0.55075179312105527738; 0.55982741271069481791; 0.56902999107472161225; 0.57836468112670231279;
        0.58783705444182052571; 0.59745315095181228217; 0.60721953663260488551; 0.61714337082656248870;
        0.62723248525781456578; 0.63749547734314487428; 0.64794182111855080873; 0.65858200005865368016;
        0.66942766735770616891; 0.68049184100641433355; 0.69178914344603585279; 0.70333609902581741633;
        0.71515150742047704368; 0.72725691835450587793; 0.73967724368333814856; 0.75244155918570380145;
        0.76558417390923599480; 0.77914608594170316563; 0.79317701178385921053; 0.80773829469612111340;
        0.82290721139526200050; 0.83878360531064722379; 0.85550060788506428418; 0.87324304892685358879;
        0.89228165080230272301; 0.91304364799203805999; 0.93628268170837107547; 0.96359969315576759960;
        1.
    |]

    /// generates a sample from standard normal distribution N(0,1) using ziggurat algorithm.
    let znorm() =
        let tail() =
            let exponential() = -log(1.0-genrand_float())
            let tail_start = table_x.[1]
            let mutable r = System.Double.PositiveInfinity
            while System.Double.IsPositiveInfinity r do
                let x = exponential() / tail_start
                let y = exponential()
                if 2.0*y > x*x then r <- x+tail_start
            r

        let mutable r = System.Double.PositiveInfinity
        while System.Double.IsPositiveInfinity r do
            let digit = int(uint32(genrand_int32()) &&& 255u)
            let sign = if digit &&& 1 = 0 then -1.0 else 1.0 // float(int(digit &&& 1)*2-1)
            let i = shiftRightInt32 digit 1
            let x = genrand_float()*table_x.[i]
            if x<table_x.[i+1] then r <- x*sign
            elif i=0 then r <- tail()*sign
            else
                let y = table_y.[i] + genrand_float()*(table_y.[i+1]-table_y.[i])
                if y < exp(-0.5*x*x) then r <- x*sign 
        r


#if BOX_MULLER
    // Box-Muller is generally slower and requires additional state

    let mutable rnorm_phase = false
    let mutable rnorm_2 = 0.0
    let mutable rnorm_f = 0.0

    /// generates a sample from standard normal distribution N(0,1) using Box-Muller method.
    let rnorm () =
        if rnorm_phase then
            rnorm_phase <- false
            rnorm_2*rnorm_f
        else
            rnorm_phase <- true
            let mutable rnorm_1 = 0.0
            let mutable s = 1.0
            while (s>=1.0) do
                rnorm_1 <- genrand_float()*2.0-1.0
                rnorm_2 <- genrand_float()*2.0-1.0
                s <- rnorm_1*rnorm_1 + rnorm_2*rnorm_2
            rnorm_f <- sqrt(-2.0*log(s)/s)
            rnorm_1*rnorm_f
#endif

    do
        if mt.Length <> N then failwith (sprintf "State must be an array of length %d" N)

    new ()=
        MT19937(5489.0)

    new (seed:float) =
        let state = init_genrand seed
        MT19937(state, N)

    new (seed_arr:float[]) =
        if Array.length seed_arr = N+1 && ((uint32)seed_arr.[N]) < 2u + uint32 N then
            let state = Array.init N (fun i -> seed_arr.[i])
            let idx = int (seed_arr.[N])
            MT19937(state, idx)
        else
            let state = init_by_array(seed_arr)
            MT19937(state, N)
    member private x.getMt = Array.copy mt
    member private x.getIdx = mti
    new(copy:MT19937) =
        MT19937(copy.getMt, copy.getIdx)

    /// returns an array that allows to exactly restore the state of the generator.
    member x.get_seed() = [| yield! mt; yield float(uint32 mti)|]

    /// generates a random number on [0,0xffffffff]-interval 
    member __.uniform_uint32() = genrand_int32()

    /// generates a random number on [0,1)-real-interval
    member __.uniform_float64() = genrand_float()

    /// generates a random number on [0,max]-int-interval
    member __.uniform_int (max:int) =
        if max < 0 then failwith "max: The value cannot be negative"
        elif max = 0 then 0
        // if typeof<max> were uint32:
        //elif max = System.UInt32.MaxValue then x.genrand_int32()
        else
            let umax = coerceU32(float(max))
            let bucket_size = // (System.UInt32.MaxValue+1)/(max+1)
                let bs = floor((mod32-1.0) /(umax + 1.0))
                if (mod32-1.0) % (umax + 1.0) = umax then bs + 1.0 else bs
            // rejection algorithm
            let mutable r = floor(genrand_int32() / bucket_size)
            while r > umax do r <- floor(genrand_int32() / bucket_size)
            int r

    /// generates 'true' with probability 'p' or 'false' with probability '1-p'
    member __.bernoulli(p) =
        if p <= 0.0 then false
        elif p >= 1.0 then true
        else float(genrand_int32()) <= p*float(System.UInt32.MaxValue)

    /// generates a sample from standard normal distribution N(0,1) using ziggurat algorithm.
    member __.normal() = znorm()

#if BOX_MULLER
    /// generates a sample from standard normal distribution N(0,1) using Box-Muller algorithm.
    member __.normal_bm() = rnorm()
#endif
