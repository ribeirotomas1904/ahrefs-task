%%css.module(let css = "./CountrySelect.module.css")

let maxVisibleCountryOptions = 20
let countryOptionsOutsideViewport = 10

type countryOption = {label: string, value: string}

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
          | (true, Some(label), Some(value)) => Some({label, value})
          | _ => None
          }
        },
      )
    })
    ->Extensions.Array.sequence
  })
}

type state =
  | Initial
  | Loading
  | Loaded({
      countryOptions: array<countryOption>,
      searchInput: string,
      optionsViewportStart: int,
      optionsViewportEnd: int,
      isDropdownOpen: bool,
      countryOptionHeight: float,
    })
  | Error

type action =
  | StartLoading
  | Load(array<countryOption>)
  | Error
  | SetSearchInput(string)
  | SetOptionsViewport({optionsViewportStart: int, optionsViewportEnd: int})
  | SetIsDropdownOpen(bool)
  | SetCountryOptionHeight(float)

let reducer = (state, action) => {
  switch (action, state) {
  | (StartLoading, Initial) => Loading

  | (Load(countryOptions), Loading) =>
    Loaded({
      countryOptions,
      searchInput: "",
      optionsViewportStart: 0,
      optionsViewportEnd: maxVisibleCountryOptions - 1,
      isDropdownOpen: false,
      countryOptionHeight: 0.0,
    })

  | (Error, Loading) => Error

  | (SetSearchInput(searchInput), Loaded(loadedState)) => Loaded({...loadedState, searchInput})

  | (SetOptionsViewport({optionsViewportStart, optionsViewportEnd}), Loaded(loadedState)) =>
    Loaded({...loadedState, optionsViewportStart, optionsViewportEnd})

  | (SetIsDropdownOpen(isDropdownOpen), Loaded(loadedState)) =>
    Loaded({...loadedState, isDropdownOpen, optionsViewportStart: 0, optionsViewportEnd: maxVisibleCountryOptions - 1})

  | (SetCountryOptionHeight(countryOptionHeight), Loaded(loadedState)) =>
    Loaded({...loadedState, countryOptionHeight})

  | (
      StartLoading | Load(_) | Error | SetSearchInput(_) | SetOptionsViewport(_) | SetIsDropdownOpen(_) | SetCountryOptionHeight(_),
      Initial | Loading | Loaded(_) | Error,
    ) => state
  }
}

let initialState = Initial

module CountryOption = {
  @react.component
  let make = (~label, ~value, ~onClick, ~onKeyUp, ~onRef, ~top) => {
    <div
      tabIndex=0
      className={css["country-option"]}
      style={ReactDOM.Style.make(~top, ~position="absolute", ~width="100%", ())}
      onClick
      onKeyUp
      ref={ReactDOM.Ref.callbackDomRef(onRef)}
      >
      <span className={`fi fi-${value}`} />
      {React.string(label)}
    </div>
  }
}

@react.component
let make = (
  ~className: option<string>="",
  ~country: option<string>,
  ~onChange: option<string> => unit,
) => {
  let (state, dispatch) = React.useReducer(reducer, initialState)

  let viewportRef = React.useRef(null)
  let countryOptionsRef = React.useRef(Dict.make())

  React.useEffect0(() => {
    let controller = AbortController.make()

    dispatch(StartLoading)

    Fetch.get(
      "https://gist.githubusercontent.com/rusty-key/659db3f4566df459bd59c8a53dc9f71f/raw/4127f9550ef063121c564025f6d27dceeb279623/counties.json",
      ~signal=controller.signal,
    )
    ->Promise.then(Fetch.json)
    ->Promise.thenResolve(json => {
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

  let onScroll = countryOptionHeight => {
    viewportRef.current
    ->Nullable.toOption
    ->Option.forEach(viewport => {
      let optionsViewportStart = Math.trunc(viewport->Extensions.Dom.scrollTop->Int.toFloat /. countryOptionHeight)->Int.fromFloat

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
  | Loaded({countryOptions, searchInput, optionsViewportStart, optionsViewportEnd, isDropdownOpen, countryOptionHeight}) =>
    let countryOptionsFiltered = {
      countryOptions->Array.filter(countryOption =>
        countryOption.label
        ->String.toLowerCase
        ->String.includes(searchInput->String.trim->String.toLowerCase)
      )
    }

    <div className>
      // SELECTED COUNTRY 
      <div>
        {country
        ->Option.flatMap(country =>
          Array.find(countryOptions, countryOption => countryOption.value == country)
        )
        ->Option.flatMap(countryOption => {
          <div>
            <span className={`fi fi-${countryOption.value}`} />
            {React.string(" ")}
            {React.string(countryOption.label)}
          </div>->Some
        })
        ->Option.getOr(React.null)}
      </div>
      // SEARCH INPUT
      <input
        value=searchInput
        onChange={e => dispatch(SetSearchInput(ReactEvent.Form.target(e)["value"]))}
        onFocus=(_ => dispatch(SetIsDropdownOpen(true)))

        // TODO: onBlur cancels the click on a country option
        // onBlur=(_ => dispatch(SetIsDropdownOpen(false)))
      />
      // DROPDOWN
      { isDropdownOpen ? 
        <div
          ref={ReactDOM.Ref.domRef(viewportRef)}
          className={css["dropdown"]}
          style={ReactDOM.Style.make(
            ~height=(Math.Int.min(countryOptionsFiltered->Array.length, maxVisibleCountryOptions)->Int.toFloat *.
            countryOptionHeight)->Float.toString ++ "px",
            (),
          )}
          onScroll={_ => onScroll(countryOptionHeight)}>
          <div
            style={ReactDOM.Style.make(
              ~position="absolute",
              ~width="100%",
              ~height=(countryOptionsFiltered->Array.length->Int.toFloat *. countryOptionHeight)
                ->Float.toString ++ "px",
              (),
            )}>
            {countryOptionsFiltered
            ->Array.slice(
              ~start=Int.clamp(optionsViewportStart - countryOptionsOutsideViewport, ~min=0),
              ~end=optionsViewportEnd + 1 + countryOptionsOutsideViewport,
            )
            ->Array.mapWithIndex((countryOption, index) => {
              <CountryOption
                key={countryOption.value}
                label={countryOption.label}
                value={countryOption.value}
                onClick={_ => {
                  onChange(countryOption.value->Some)
                  dispatch(SetIsDropdownOpen(false))
                }}
                onKeyUp={e => {
                  // TODO: make it so that navigating on keyboard doesnt feel so weird by always moving the scrollbar
                  ReactEvent.Keyboard.preventDefault(e)

                  switch ReactEvent.Keyboard.code(e) {
                  | "Enter" => 
                      onChange(countryOption.value->Some)
                      dispatch(SetIsDropdownOpen(false))
                  | "Escape" =>
                      dispatch(SetIsDropdownOpen(false))
                  | "ArrowUp" =>
                      countryOptionsRef.current->Dict.get(Int.toString(index - 1))->Option.forEach(Extensions.Dom.focus)
                  | "ArrowDown" =>
                      countryOptionsRef.current->Dict.get(Int.toString(index + 1))->Option.forEach(Extensions.Dom.focus)
                  | _ => ()
                  } 
                }}
                onRef={domRef => {
                  domRef
                  ->Nullable.toOption
                  ->Option.forEach(domRef => {
                    countryOptionsRef.current->Dict.set(index->Int.toString, domRef)                    
                    if (Extensions.Dom.offsetHeight(domRef) != countryOptionHeight) {
                      dispatch(SetCountryOptionHeight(Extensions.Dom.offsetHeight(domRef)))
                    }
                  })
                }}
                top={Float.toString((optionsViewportStart + index)->Int.toFloat *. countryOptionHeight) ++ "px"}
              />
            })
            ->React.array}
          </div>
        </div> :
        React.null }

    </div>
  | Error => React.string("error")
  }
}
