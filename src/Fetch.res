type response

@val
external get: (string, {"signal": option<AbortController.signal>}) => Promise.t<response> = "fetch"
let get = (url, ~signal=?) => get(url, {"signal": signal})

@send external json: response => Promise.t<JSON.t> = "json"
