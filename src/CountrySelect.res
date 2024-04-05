%%css.module(let css = "./CountrySelect.module.css")

let maxVisibleCountryOptions = 14

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
    if loadedState.isDropdownOpen == isDropdownOpen {
      Loaded(loadedState)
    } else {
      Loaded({
        ...loadedState,
        isDropdownOpen,
        searchInput: "",
        optionsViewportStart: 0,
        optionsViewportEnd: maxVisibleCountryOptions - 1,
        selectedCountryOption: 0,
      })
    }

  | (SetCountryOptionHeight(countryOptionHeight), Loaded(loadedState)) =>
    Loaded({...loadedState, countryOptionHeight})

  | (SetSelectedCountryOption(selectedCountryOption), Loaded(loadedState)) =>
    Loaded({...loadedState, selectedCountryOption})

  | (
      StartLoading
      | Load(_)
      | Error
      | SetSearchInput(_)
      | SetOptionsViewport(_)
      | SetIsDropdownOpen(_)
      | SetCountryOptionHeight(_)
      | SetSelectedCountryOption(_),
      Initial | Loading | Loaded(_) | Error,
    ) => state
  }
}

let initialState = Initial

module CountryOption = {
  @react.component
  let make = (
    ~label,
    ~value,
    ~onChange,
    ~onMouseEnter,
    ~onCancelSelect,
    ~onRef,
    ~top,
    ~isSelected,
  ) => {
    <div
      className={css["country-option"] ++ " " ++ (isSelected ? css["country-option-selected"] : "")}
      style={ReactDOM.Style.make(~top, ())}
      onClick={_ => onChange()}
      onTouchEnd={_ => onChange()}
      onMouseEnter={_ => onMouseEnter()}
      onTouchStart={_ => onMouseEnter()}
      onTouchMove={_ => onCancelSelect()}
      onTouchCancel={_ => onCancelSelect()}
      ref={ReactDOM.Ref.callbackDomRef(onRef)}>
      <span className={`fi fi-${value}`} />
      <span> {React.string(label)} </span>
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

  React.useEffect0(() => {
    let controller = AbortController.make()

    dispatch(StartLoading)

    // TODO: handle bad status code and network errors
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
    ->Promise.done

    Some(() => AbortController.abort(controller))
  })

  switch state {
  | Initial
  | Error
  | Loading =>
    <div className={css["country-select"] ++ " " ++ className}>
      // SELECTED COUNTRY
      <div className={css["selected-country"]}>
        <span> {React.string("Select country")} </span>
        <Icon.TriangleIcon />
      </div>
      <div className={css["wrapper"]}>
        // SEARCH INPUT
        <div className={css["search-input-container"]}>
          <Icon.SearchIcon className={css["search-icon"]} />
          <input
            disabled=true
            className={css["search-input"]}
            placeholder={"Search"}
            defaultValue={switch state {
            | Initial | Loaded(_) => ""
            | Loading => "Loading..."
            | Error => "Unexpected Error!"
            }}
          />
        </div>
      </div>
    </div>
  | Loaded({
      countryOptions,
      searchInput,
      optionsViewportStart,
      optionsViewportEnd,
      isDropdownOpen,
      countryOptionHeight,
      selectedCountryOption,
    }) =>
    let countryOptionsWithIndex = {
      countryOptions->Array.filter(countryOption =>
        countryOption.label
        ->String.toLowerCase
        ->String.includes(searchInput->String.trim->String.toLowerCase)
      )
    }

    let countryOptionsWithIndex =
      countryOptionsWithIndex->Array.mapWithIndex((countryOption, index) => (countryOption, index))

    let onChangeHandler = () => {
      countryOptionsWithIndex[selectedCountryOption]->Option.forEach(((countryOption, _)) => {
        onChange(Some(countryOption.value))

        dispatch(SetIsDropdownOpen(false))

        searchInputRef.current
        ->Nullable.toOption
        ->Option.forEach(Extensions.Dom.blur)
      })
    }

    let onScroll = () => {
      viewportRef.current
      ->Nullable.toOption
      ->Option.forEach(viewport => {
        let optionsViewportStart =
          Math.round(
            viewport->Extensions.Dom.scrollTop->Int.toFloat /. countryOptionHeight,
          )->Int.fromFloat

        dispatch(
          SetOptionsViewport({
            optionsViewportStart,
            optionsViewportEnd: optionsViewportStart + (maxVisibleCountryOptions - 1),
          }),
        )
      })

      searchInputRef.current->Nullable.toOption->Option.forEach(Extensions.Dom.focus)
    }

    <div className={css["country-select"] ++ " " ++ className}>
      // SELECTED COUNTRY
      <div
        className={css["selected-country"]}
        onClick={_ =>
          searchInputRef.current->Nullable.toOption->Option.forEach(Extensions.Dom.focus)}>
        {country
        ->Option.flatMap(country =>
          Array.find(countryOptions, countryOption => countryOption.value == country)
        )
        ->Option.flatMap(countryOption => {
          <>
            <span className={`fi fi-${countryOption.value}`} />
            <span> {React.string(countryOption.label)} </span>
            <Icon.TriangleIcon />
          </>->Some
        })
        ->Option.getOr({
          <>
            <span> {React.string("Select country")} </span>
            <Icon.TriangleIcon />
          </>
        })}
      </div>
      <div className={css["wrapper"]}>
        // SEARCH INPUT
        <div className={css["search-input-container"]}>
          <Icon.SearchIcon className={css["search-icon"]} />
          <input
            ref={ReactDOM.Ref.domRef(searchInputRef)}
            className={css["search-input"]}
            placeholder={"Search"}
            value=searchInput
            onChange={e => dispatch(SetSearchInput(ReactEvent.Form.target(e)["value"]))}
            onFocus={_ => dispatch(SetIsDropdownOpen(true))}
            onKeyDown={e => {
              switch ReactEvent.Keyboard.code(e) {
              | "Enter" => onChangeHandler()
              | "Escape" =>
                dispatch(SetIsDropdownOpen(false))
                searchInputRef.current
                ->Nullable.toOption
                ->Option.forEach(Extensions.Dom.blur)
              | "ArrowUp" =>
                ReactEvent.Keyboard.preventDefault(e)

                let newSelectedCountryOption =
                  selectedCountryOption - 1 < 0
                    ? countryOptionsWithIndex->Array.length - 1
                    : selectedCountryOption - 1

                if newSelectedCountryOption == countryOptionsWithIndex->Array.length - 1 {
                  viewportRef.current
                  ->Nullable.toOption
                  ->Option.forEach(viewport => {
                    Extensions.Dom.scrollTo(
                      viewport,
                      {
                        "top": countryOptionsWithIndex->Array.length->Int.toFloat *.
                          countryOptionHeight,
                        "behavior": "instant",
                      },
                    )
                    setTimeout(() => {
                      Extensions.Dom.scrollTo(
                        viewport,
                        {
                          "top": countryOptionsWithIndex->Array.length->Int.toFloat *.
                            countryOptionHeight,
                          "behavior": "instant",
                        },
                      )
                    }, 10)->ignore
                  })
                } else if (
                  newSelectedCountryOption < optionsViewportStart ||
                    newSelectedCountryOption > optionsViewportEnd
                ) {
                  viewportRef.current
                  ->Nullable.toOption
                  ->Option.forEach(viewport =>
                    Extensions.Dom.scrollTo(
                      viewport,
                      {
                        "top": newSelectedCountryOption->Int.toFloat *. countryOptionHeight,
                        "behavior": "instant",
                      },
                    )
                  )
                }

                dispatch(SetSelectedCountryOption(newSelectedCountryOption))

              | "ArrowDown" =>
                ReactEvent.Keyboard.preventDefault(e)

                let newSelectedCountryOption =
                  selectedCountryOption + 1 > countryOptionsWithIndex->Array.length - 1
                    ? 0
                    : selectedCountryOption + 1

                if newSelectedCountryOption == 0 {
                  viewportRef.current
                  ->Nullable.toOption
                  ->Option.forEach(viewport =>
                    Extensions.Dom.scrollTo(viewport, {"top": 0.0, "behavior": "instant"})
                  )
                } else if (
                  newSelectedCountryOption < optionsViewportStart ||
                    newSelectedCountryOption > optionsViewportEnd
                ) {
                  viewportRef.current
                  ->Nullable.toOption
                  ->Option.forEach(viewport => {
                    Extensions.Dom.scrollTo(
                      viewport,
                      {
                        "top": (newSelectedCountryOption - (maxVisibleCountryOptions - 1))
                          ->Int.toFloat *. countryOptionHeight,
                        "behavior": "auto",
                      },
                    )
                  })
                }

                dispatch(SetSelectedCountryOption(newSelectedCountryOption))

              | _ => ()
              }
            }}
            onBlur={e => {
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
            }}
          />
        </div>
        // DROPDOWN
        {switch (isDropdownOpen, countryOptionsWithIndex->Array.length > 0) {
        | (false, _) => React.null
        | (true, false) =>
          <div className={css["dropdown"]} tabIndex={-1}>
            <div className={css["no-options"]}> {React.string("No options")} </div>
          </div>

        | (true, true) =>
          <div
            ref={ReactDOM.Ref.domRef(viewportRef)}
            className={css["dropdown"]}
            style={ReactDOM.Style.make(
              ~height=(Math.Int.min(
                countryOptionsWithIndex->Array.length,
                maxVisibleCountryOptions,
              )->Int.toFloat *. countryOptionHeight +. 5.0)->Float.toString ++ "px",
              (),
            )}
            onScroll={_ => onScroll()}
            tabIndex={-1}>
            <div
              className={css["country-options-container"]}
              style={ReactDOM.Style.make(
                ~height=(countryOptionsWithIndex->Array.length->Int.toFloat *. countryOptionHeight)
                  ->Float.toString ++ "px",
                (),
              )}>
              {countryOptionsWithIndex
              ->Array.slice(~start=optionsViewportStart, ~end=optionsViewportEnd + 1)
              ->Array.map(((countryOption, countryOptionIndex)) => {
                <CountryOption
                  isSelected={countryOptionIndex == selectedCountryOption}
                  key={countryOption.value}
                  label={countryOption.label}
                  value={countryOption.value}
                  onChange={onChangeHandler}
                  onCancelSelect={_ => dispatch(SetSelectedCountryOption(-1))}
                  onMouseEnter={_ => dispatch(SetSelectedCountryOption(countryOptionIndex))}
                  onRef={domRef => {
                    domRef
                    ->Nullable.toOption
                    ->Option.forEach(domRef => {
                      if Extensions.Dom.offsetHeight(domRef) != countryOptionHeight {
                        dispatch(SetCountryOptionHeight(Extensions.Dom.offsetHeight(domRef)))
                      }
                    })
                  }}
                  top={Float.toString(
                    countryOptionIndex->Int.toFloat *. countryOptionHeight,
                  ) ++ "px"}
                />
              })
              ->React.array}
            </div>
          </div>
        }}
      </div>
    </div>
  }
}
