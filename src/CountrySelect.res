%%css.module(let css = "./CountrySelect.module.css")

let maxVisibleCountryOptions = 20
let renderOutOfViewport = 10
let countryOptionHeight = 27

let parseCountries = json => {
  json
  ->JSON.Decode.array
  ->Option.flatMap(value => {
    value
    ->Array.map(value => {
      value
      ->JSON.Decode.object
      ->Option.flatMap(
        value => {
          switch (
            Dict.toArray(value)->Array.length == 2,
            Dict.get(value, "label")->Option.flatMap(JSON.Decode.string),
            Dict.get(value, "value")->Option.flatMap(JSON.Decode.string),
          ) {
          | (true, Some(label), Some(value)) => Some({"label": label, "value": value})
          | _ => None
          }
        },
      )
    })
    ->Extensions.Array.sequence
  })
}

type countryOption = {"label": string, "value": string}

type state =
  | Initial
  | Loading
  | Loaded({
      countryOptions: array<countryOption>,
      searchInput: string,
      optionsViewportStart: int,
      optionsViewportEnd: int,
    })
  | Error

type action =
  | StartLoading
  | Load(array<countryOption>)
  | Error
  | SetSearchInput(string)
  | SetOptionsViewport({optionsViewportStart: int, optionsViewportEnd: int})

let reducer = (state, action) => {
  switch (action, state) {
  | (StartLoading, Initial) => Loading

  | (Load(countryOptions), Loading) =>
    // TODO: start optionsViewportEnd based on configuration
    Loaded({
      countryOptions,
      searchInput: "",
      optionsViewportStart: 0,
      optionsViewportEnd: maxVisibleCountryOptions - 1,
    })

  | (Error, Loading) => Error

  | (SetSearchInput(searchInput), Loaded(loadedState)) => Loaded({...loadedState, searchInput})

  | (SetOptionsViewport({optionsViewportStart, optionsViewportEnd}), Loaded(loadedState)) =>
    Loaded({...loadedState, optionsViewportStart, optionsViewportEnd})

  | (
      StartLoading | Load(_) | Error | SetSearchInput(_) | SetOptionsViewport(_),
      Initial | Loading | Loaded(_) | Error,
    ) => state
  }
}

let initialState = Initial

module CountryOption = {
  @react.component
  let make = (~label, ~value, ~onClick, ~top) => {
    Console.log(label)

    <div
      className={css["country-option"]}
      onClick
      style={ReactDOM.Style.make(~top, ~position="absolute", ~width="100%", ())}>
      <span className={`fi fi-${value}`} />
      {React.string(label)}
    </div>
  }
}

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
      Console.log(json)
      switch parseCountries(json) {
      | None => dispatch(Error)
      | Some(countries) => dispatch(Load(countries))
      }
    })
    // TODO: add error handling
    // ->Promise.catch(_ => dispatch(Error)->Promise.resolve)
    ->Promise.done

    Some(() => AbortController.abort(controller))
  })

  let onScroll = _ => {
    viewportRef.current
    ->Nullable.toOption
    ->Option.forEach(viewport => {
      let optionsViewportStart = viewport->Extensions.Dom.scrollTop / countryOptionHeight

      dispatch(
        SetOptionsViewport({
          optionsViewportStart,
          optionsViewportEnd: optionsViewportStart + maxVisibleCountryOptions - 1,
        }),
      )
    })
  }

  switch state {
  | Initial => React.string("initial")
  | Loading => React.string("loading")
  | Loaded({countryOptions, searchInput, optionsViewportStart, optionsViewportEnd}) =>
    let countryOptionsFiltered = {
      countryOptions->Array.filter(countryOption =>
        countryOption["label"]
        ->String.toLowerCase
        ->String.includes(searchInput->String.trim->String.toLowerCase)
      )
    }

    <div>
      <div>
        {country
        ->Option.flatMap(country =>
          Array.find(countryOptions, countryOption => countryOption["value"] == country)
        )
        ->Option.flatMap(countryOption => {
          <div>
            <span className={`fi fi-${countryOption["value"]}`} />
            {React.string(" ")}
            {React.string(countryOption["label"])}
          </div>->Some
        })
        ->Option.getOr(React.null)}
      </div>
      <input
        value=searchInput
        onChange={e => dispatch(SetSearchInput(ReactEvent.Form.target(e)["value"]))}
      />
      <div
        ref={ReactDOM.Ref.domRef(viewportRef)}
        className={css["dropdown"]}
        style={ReactDOM.Style.make(
          ~height=(Math.Int.min(countryOptionsFiltered->Array.length, maxVisibleCountryOptions) *
          countryOptionHeight)->Int.toString ++ "px",
          (),
        )}
        onScroll>
        <div
          style={ReactDOM.Style.make(
            ~position="absolute",
            ~width="100%",
            ~height=(countryOptionsFiltered->Array.length * countryOptionHeight)
              ->Int.toString ++ "px",
            (),
          )}>
          {countryOptionsFiltered
          ->Array.slice(
            ~start=Int.clamp(optionsViewportStart - renderOutOfViewport, ~min=0),
            ~end=optionsViewportEnd + 1 + renderOutOfViewport,
          )
          ->Array.mapWithIndex((countryOption, index) => {
            <CountryOption
              key={countryOption["value"]}
              label={countryOption["label"]}
              value={countryOption["value"]}
              onClick={_ => onChange(countryOption["value"]->Some)}
              top={Int.toString((optionsViewportStart + index) * countryOptionHeight) ++ "px"}
            />
          })
          ->React.array}
        </div>
      </div>
    </div>
  | Error => React.string("error")
  }
}
