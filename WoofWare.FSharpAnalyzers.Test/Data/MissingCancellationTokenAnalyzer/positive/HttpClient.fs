module HttpClientWithoutToken

open System.Net.Http

let fetchWithoutToken () =
    let client = new HttpClient ()
    client.GetAsync ("https://example.com")
