module MultipleCallsWithoutToken

open System.Threading.Tasks
open System.Net.Http

let multipleAsyncCalls () =
    task {
        let! _ = Task.Delay (100)
        let client = new HttpClient ()
        let! response = client.GetAsync ("https://example.com")
        return response
    }
