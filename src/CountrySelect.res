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
      selectedCountryOption: int,
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
  | SetSelectedCountryOption(int)

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
      selectedCountryOption: 0,
    })

  | (Error, Loading) => Error

  | (SetSearchInput(searchInput), Loaded(loadedState)) => Loaded({...loadedState, searchInput})

  | (SetOptionsViewport({optionsViewportStart, optionsViewportEnd}), Loaded(loadedState)) =>
    Loaded({...loadedState, optionsViewportStart, optionsViewportEnd})

  | (SetIsDropdownOpen(isDropdownOpen), Loaded(loadedState)) =>
    Loaded({...loadedState, isDropdownOpen, searchInput: "", optionsViewportStart: 0, optionsViewportEnd: maxVisibleCountryOptions - 1, selectedCountryOption: 0})

  | (SetCountryOptionHeight(countryOptionHeight), Loaded(loadedState)) =>
    Loaded({...loadedState, countryOptionHeight})

  | (SetSelectedCountryOption(selectedCountryOption), Loaded(loadedState)) =>
    Loaded({...loadedState, selectedCountryOption})

  | (
      StartLoading | Load(_) | Error | SetSearchInput(_) | SetOptionsViewport(_) | SetIsDropdownOpen(_) | SetCountryOptionHeight(_) | SetSelectedCountryOption(_),
      Initial | Loading | Loaded(_) | Error,
    ) => state
  }
}

let initialState = Initial

module CountryOption = {
  @react.component
  let make = (~label, ~value, ~onClick, ~onMouseEnter, ~onRef, ~top, ~isSelected) => {
    <div
      className={css["country-option"] ++ " " ++ (isSelected ? css["country-option-selected"] : "")}
      style={ReactDOM.Style.make(~top, ~position="absolute", ~width="100%", ())}
      onClick
      onMouseEnter
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

  let searchInputRef = React.useRef(null)
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

  switch state {
  | Initial => React.string("initial")
  | Loading => React.string("loading")
  | Loaded({countryOptions, searchInput, optionsViewportStart, optionsViewportEnd, isDropdownOpen, countryOptionHeight, selectedCountryOption}) =>
    // TODO: deal with the case of countryOptionsFiltered having length 0
    let countryOptionsWithIndex = {
      countryOptions->Array.filter(countryOption =>
        countryOption.label
        ->String.toLowerCase
        ->String.includes(searchInput->String.trim->String.toLowerCase)
      )
    }

    let countryOptionsWithIndex = countryOptionsWithIndex->Array.mapWithIndex((countryOption, index) => (countryOption, index))

    let onChangeHandler = () => {
      Array.get(countryOptionsWithIndex, selectedCountryOption)->Option.forEach(((countryOption, _)) => onChange(Some(countryOption.value)))
      dispatch(SetIsDropdownOpen(false))
      searchInputRef.current
      ->Nullable.toOption
      ->Option.forEach(Extensions.Dom.blur)
    }

    let onScroll = () => {
      Console.log("ON SCROLL")
      viewportRef.current
      ->Nullable.toOption
      ->Option.forEach(viewport => {
        let optionsViewportStart = Math.trunc(viewport->Extensions.Dom.scrollTop->Int.toFloat /. countryOptionHeight)->Int.fromFloat

        dispatch(
          SetOptionsViewport({
            optionsViewportStart,
            optionsViewportEnd: optionsViewportStart + (maxVisibleCountryOptions - 1),
          }),
        )
      })

      searchInputRef.current->Nullable.toOption->Option.forEach(Extensions.Dom.focus)
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
        ref={ReactDOM.Ref.domRef(searchInputRef)}
        value=searchInput
        onChange={e => dispatch(SetSearchInput(ReactEvent.Form.target(e)["value"]))}
        onFocus=(_ => dispatch(SetIsDropdownOpen(true)))
        onKeyDown={e => {
          switch ReactEvent.Keyboard.code(e) {
          | "Enter" => 
              onChangeHandler()
          | "Escape" =>
              dispatch(SetIsDropdownOpen(false))
              searchInputRef.current
              ->Nullable.toOption
              ->Option.forEach(Extensions.Dom.blur)
          | "ArrowUp" =>
              ReactEvent.Keyboard.preventDefault(e)

              let newSelectedCountryOption = (selectedCountryOption - 1) < 0 ? countryOptionsWithIndex->Array.length - 1 : selectedCountryOption - 1
              dispatch(SetSelectedCountryOption(newSelectedCountryOption))

              if (newSelectedCountryOption == countryOptionsWithIndex->Array.length - 1) {
                viewportRef.current->Nullable.toOption->Option.forEach(viewport => Extensions.Dom.scrollTo(viewport, {"top": countryOptionsWithIndex->Array.length->Int.toFloat *. countryOptionHeight}))
              } else if (newSelectedCountryOption < optionsViewportStart) {
                viewportRef.current->Nullable.toOption->Option.forEach(viewport => Extensions.Dom.scrollTo(viewport, {"top": newSelectedCountryOption->Int.toFloat *. countryOptionHeight}))
              }
          | "ArrowDown" =>
              ReactEvent.Keyboard.preventDefault(e)

              let newSelectedCountryOption = (selectedCountryOption + 1) > (countryOptionsWithIndex->Array.length - 1) ? 0 : selectedCountryOption + 1
              dispatch(SetSelectedCountryOption(newSelectedCountryOption))

              if (newSelectedCountryOption == 0) {
                viewportRef.current->Nullable.toOption->Option.forEach(viewport => Extensions.Dom.scrollTo(viewport, {"top": 0.0}))
              } else if (newSelectedCountryOption > optionsViewportEnd) {
                viewportRef.current->Nullable.toOption->Option.forEach(viewport => Extensions.Dom.scrollTo(viewport, {"top": newSelectedCountryOption->Int.toFloat *. countryOptionHeight}))
              }
          | _ => ()
          } 
        }}
        onBlur=(e => {
          switch ReactEvent.Focus.relatedTarget(e) {
            | None => dispatch(SetIsDropdownOpen(false))
            | Some(relatedTarget) => 
              viewportRef.current
              ->Nullable.toOption
              ->Option.forEach(viewport => {
                switch Extensions.Dom.contains(viewport, Obj.magic(relatedTarget)) {
                | false => dispatch(SetIsDropdownOpen(false))
                | true => ()
                }
              })
          }
        })
      />
      // DROPDOWN
      { isDropdownOpen ? 
        <div
          ref={ReactDOM.Ref.domRef(viewportRef)}
          className={css["dropdown"]}
          style={ReactDOM.Style.make(
            ~height=(Math.Int.min(countryOptionsWithIndex->Array.length, maxVisibleCountryOptions)->Int.toFloat *.
            countryOptionHeight)->Float.toString ++ "px",
            (),
          )}
          onScroll={_ => onScroll()}
          tabIndex={-1}
          >
          <div
            style={ReactDOM.Style.make(
              ~position="absolute",
              ~width="100%",
              ~height=(countryOptionsWithIndex->Array.length->Int.toFloat *. countryOptionHeight)
                ->Float.toString ++ "px",
              (),
            )}>
            {countryOptionsWithIndex
            ->Array.slice(
              ~start=Math.Int.max(optionsViewportStart - countryOptionsOutsideViewport, 0),
              ~end=optionsViewportEnd + 1 + countryOptionsOutsideViewport,
            )
            ->Array.mapWithIndex(((countryOption, countryOptionIndex), mapIndex) => {
              <CountryOption
                isSelected={countryOptionIndex == selectedCountryOption}
                key={countryOption.value}
                label={countryOption.label}
                value={countryOption.value}
                onClick={_ => onChangeHandler()}
                onMouseEnter={_ => dispatch(SetSelectedCountryOption(countryOptionIndex))}
                onRef={domRef => {
                  domRef
                  ->Nullable.toOption
                  ->Option.forEach(domRef => {
                    countryOptionsRef.current->Dict.set(countryOptionIndex->Int.toString, domRef)                    
                    if (Extensions.Dom.offsetHeight(domRef) != countryOptionHeight) {
                      dispatch(SetCountryOptionHeight(Extensions.Dom.offsetHeight(domRef)))
                    }
                  })
                }}
                top={Float.toString((optionsViewportStart + mapIndex)->Int.toFloat *. countryOptionHeight) ++ "px"}
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
