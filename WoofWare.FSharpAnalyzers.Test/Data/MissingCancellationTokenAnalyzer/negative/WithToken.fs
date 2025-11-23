module MethodsWithToken

open System.Threading
open System.Threading.Tasks
open System.Net.Http

let delayWithToken (ct : CancellationToken) = Task.Delay (1000, ct)

let fetchWithToken (ct : CancellationToken) =
    let client = new HttpClient ()
    client.GetAsync ("https://example.com", ct)
