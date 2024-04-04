%%css.module(let css = "./CountrySelect.module.css")

@get external scrollTop: Dom.element => int = "scrollTop"

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

type countryOption = {"label": string, "value": string}

type state =
  | Initial 
  | Loading
  | Loaded({countryOptions: array<countryOption>, searchInput: string})
  | Error

type action = 
  | StartLoading
  | Load(array<countryOption>)
  | Error
  | SetSearchInput(string)

let reducer = (state, action) => {
  switch (action, state) {
  | (StartLoading, Initial) => Loading
  | (Load(countryOptions), Loading) => Loaded({countryOptions, searchInput: ""})
  | (Error, Loading) => Error
  | (SetSearchInput(searchInput), Loaded(loadedState)) => Loaded({...loadedState, searchInput})
  | ((StartLoading | Load(_) | Error | SetSearchInput(_)),(Initial | Loading | Loaded(_) | Error)) => state
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

  let viewportRef = React.useRef(null)

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
  | Loaded({countryOptions, searchInput}) =>
    <div>
      <div>{country->Option.flatMap(country => Array.find(countryOptions, countryOption => countryOption["value"] == country))->Option.flatMap(countryOption => {
        <div>
          <span className=`fi fi-${countryOption["value"]}` />
          {React.string(" ")}
          {React.string(countryOption["label"])}
        </div>
        ->Some
      })->Option.getOr(React.null)}</div>
      <input value=searchInput onChange=(e => dispatch(SetSearchInput(ReactEvent.Form.target(e)["value"])))/>
      <div ref={ReactDOM.Ref.domRef(viewportRef)} onScroll={_ => {
        viewportRef.current->Nullable.toOption->Option.forEach(viewport => viewport->scrollTop->Console.log)
      }} className=css["dropdown"]>
        {
          countryOptions
          ->Array.filter(countryOption => countryOption["label"]->String.toLowerCase->String.includes(searchInput->String.trim->String.toLowerCase))
          ->Array.map(countryOption => {
            <div onClick={_ => onChange(countryOption["value"]->Some)}>
              <span className=`fi fi-${countryOption["value"]}` />
              {React.string(" ")}
              {React.string(countryOption["label"])}
            </div>
          })
          ->React.array
        }
      </div>
    </div>
  | Error => React.string("error")
  }
}

// Feature requirements and restrictions

//     Styles should be as close to the mockup as possible. Note that provided link is a Sketch-file, you can extract necessary styles from it.
//     You can use existing javascript packages such as react-select and flag-icon-css to implement CountrySelect: https://github.com/JedWatson/react-select/ https://github.com/lipis/flag-icon-css
//     You cannot use existing bindings; we expect you to write them by yourself.
//     Select uses this list of countries: https://gist.githubusercontent.com/rusty-key/659db3f4566df459bd59c8a53dc9f71f/raw/4127f9550ef063121c564025f6d27dceeb279623/counties.json
//     Countries should be fetched from the server during runtime, not embedded to your code.
//     The search filter is internal. It filters countries by name without case sensitivity.
//     Select renders only visible options; it is not slow on opening.
//     It supports keyboard (user can open and close dropdown, navigate and select options, cancel choice with keyboard).
