let parseCountries = json => {
  json
  ->JSON.Decode.array
  ->Option.flatMap(value => {
    value
    ->Array.map(value => {
      value
      ->JSON.Decode.object
      ->Option.flatMap(value => {
        switch (Dict.toArray(value)->Array.length == 2, Dict.get(value, "label")->Option.flatMap(JSON.Decode.string), Dict.get(value, "value")->Option.flatMap(JSON.Decode.string)) {
          | (true, Some(label), Some(value)) => Some({"label": label, "value": value})
          | _ => None
        }
      })
    })
    ->Extensions.Array.sequence
  })
}

type state =
  | Initial 
  | Loading
  | Loaded(array<{"label": string, "value": string}>)
  | Error

type action = 
  | StartLoading
  | Load(array<{"label": string, "value": string}>)
  | Error

let reducer = (state, action) => {
  switch (action, state) {
  | (StartLoading, Initial) => Loading
  | (Load(countries), Loading) => Loaded(countries)
  | (Error, Loading) => Error
  | (StartLoading | Load(_) | Error, Initial | Loading | Loaded(_) | Error) => state
  }
}

let initialState = Initial

@react.component
let make = (
  ~className: option<string>=?,
  ~country: option<string>,
  ~onChange: option<string> => unit,
) => {
  let (state, dispatch) = React.useReducer(reducer, initialState)

  React.useEffect0(() => {
    let controller = AbortController.make()

    dispatch(StartLoading)

    Fetch.get(
      "https://gist.githubusercontent.com/rusty-key/659db3f4566df459bd59c8a53dc9f71f/raw/4127f9550ef063121c564025f6d27dceeb279623/counties.json",
      ~signal=controller.signal,
    )
    ->Promise.then(Fetch.json)
    ->Promise.thenResolve(json => {
      switch(parseCountries(json)) {
        | None => dispatch(Error)
        | Some(countries) => dispatch(Load(countries))
      }
    })
    // TODO: add error handling
    // ->Promise.catch(_ => dispatch(Error)->Promise.resolve)
    ->Promise.done

    Some(() => AbortController.abort(controller))
  })

  switch state {
  | Initial => React.string("initial")
  | Loading => React.string("loading")
  | Loaded(countries) =>
    <div>
      {
        countries
        ->Array.map(country => {
          <div>
            <span className=`fi fi-${country["value"]}` />
            {React.string(" ")}
            {React.string(country["label"])}
          </div>
        })
        ->React.array
      } 
    </div>
  | Error => React.string("error")
  }
}
