// This solutions should solve the following:
// If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
//
// Find the sum of all the multiples of 3 or 5 below 1000.
open System.Diagnostics

let m1 = 3
let m2 = 5


// (sum of multiples of 3) + (sum of multiples of 5) - (sum of common terms) =
// (3 + 6 + ... + 999) + (5 + 10 + ... + 995) - (15 + 30 + ... + 990) =
// 3(1 + 2 + .. + 333) + 5(1 + 2 + ... + 119) - 15(1 + 2 + 66) =
// 3(333*(333+1))/2 + 5(119*(119+1))/2 - 15(66*(66+1))/2
let Smart (num : int) : int =
    let c = m1 * m2
    let numMultiplesm2 : int = (num-1)/m2
    let numMultiplesm1 : int = (num-1)/m1
    let numMultiplesc : int = (num-1)/(c)
    
    let summ2 = m2 * (numMultiplesm2 * (numMultiplesm2+1)) / 2
    let summ1 = m1 * (numMultiplesm1 * (numMultiplesm1+1)) / 2
    let sumc = c * (numMultiplesc * (numMultiplesc+1)) / 2

    summ2 + summ1 - sumc 

let Iteratively (num : int) : int =
    let mutable sum = 0
    for x in 1.. num-1 do
        if x % 3 = 0 || x % 5 = 0 then
            sum <- sum + x
    sum

let rec Recursively (num : int) : int =
    if num = 0 then
        0
    elif num % 5 = 0 || num % 3 = 0 then
        num + Recursively (num-1)
    else
        Recursively (num-1)


[<EntryPoint>]
let main argv =
    let num = 1000

    let stopWatch = Stopwatch.StartNew()
    let sol1 = Smart num
    stopWatch.Stop()
    printfn "the solution is(smart): %i" sol1
    printfn "speed of smart: %g" stopWatch.Elapsed.TotalMilliseconds

    stopWatch.Restart()
    let sol2 = Iteratively num
    stopWatch.Stop()
    printfn "the solution is(Iteratively): %i" sol2
    printfn "speed of Iteratively: %g" stopWatch.Elapsed.TotalMilliseconds

    stopWatch.Restart()
    let sol3 = Recursively (num-1)
    stopWatch.Stop()
    printfn "the solution is(recursively): %i" sol3
    printfn "speed of Recursively: %g" stopWatch.Elapsed.TotalMilliseconds
    0