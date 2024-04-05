%%css.module(let css = "./CountrySelect.module.css")

let maxVisibleCountryOptions = 14
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
    Loaded({
      ...loadedState,
      isDropdownOpen,
      searchInput: "",
      optionsViewportStart: 0,
      optionsViewportEnd: maxVisibleCountryOptions - 1,
      selectedCountryOption: 0,
    })

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
  let make = (~label, ~value, ~onClick, ~onMouseEnter, ~onRef, ~top, ~isSelected) => {
    <div
      className={css["country-option"] ++ " " ++ (isSelected ? css["country-option-selected"] : "")}
      style={ReactDOM.Style.make(~top, ())}
      onClick
      onMouseEnter
      ref={ReactDOM.Ref.callbackDomRef(onRef)}>
      <span className={`fi fi-${value}`} />
      <span> {React.string(label)} </span>
    </div>
  }
}

module SearchIcon = {
  @react.component
  let make = (~className) => {
    <svg className width="14" height="14" viewBox="0 0 14 14" xmlns="http://www.w3.org/2000/svg">
      <path
        fillRule="evenodd"
        clipRule="evenodd"
        d="M6 11C7.01929 11 7.96734 10.695 8.75787 10.1713L12.06 13.47L13.47 12.06L10.1713 8.75783C10.695 7.96731 11 7.01927 11 6C11 3.23858 8.76142 1 6 1C3.23858 1 1 3.23858 1 6C1 8.76142 3.23858 11 6 11ZM9.2 6C9.2 7.76731 7.76731 9.2 6 9.2C4.23269 9.2 2.8 7.76731 2.8 6C2.8 4.23269 4.23269 2.8 6 2.8C7.76731 2.8 9.2 4.23269 9.2 6Z"
        fill="#333333"
      />
    </svg>
  }
}

module TriangleIcon = {
  @react.component
  let make = (~className) => {
    <svg width="8" height="8" viewBox="0 0 8 8" xmlns="http://www.w3.org/2000/svg">
      <path fillRule="evenodd" clipRule="evenodd" d="M0 2H8L4 7L0 2Z" fill="#333333" />
    </svg>
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
  | Loaded({
      countryOptions,
      searchInput,
      optionsViewportStart,
      optionsViewportEnd,
      isDropdownOpen,
      countryOptionHeight,
      selectedCountryOption,
    }) =>
    // TODO: deal with the case of countryOptionsFiltered having length 0
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
      countryOptionsWithIndex[selectedCountryOption]->Option.forEach(((countryOption, _)) =>
        onChange(Some(countryOption.value))
      )
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
        let optionsViewportStart =
          Math.trunc(
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
            <TriangleIcon className="" />
          </>->Some
        })
        ->Option.getOr({
          <>
            <span> {React.string("Select Country")} </span>
            <TriangleIcon className="" />
          </>
        })}
      </div>
      <div className={css["wrapper"]}>
        // SEARCH INPUT
        <div className={css["search-input-container"]}>
          <SearchIcon className={css["search-icon"]} />
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
                } else if newSelectedCountryOption < optionsViewportStart || true {
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

                Console.log(newSelectedCountryOption)
                Console.log(optionsViewportEnd)

                if newSelectedCountryOption == 0 {
                  viewportRef.current
                  ->Nullable.toOption
                  ->Option.forEach(viewport =>
                    Extensions.Dom.scrollTo(viewport, {"top": 0.0, "behavior": "instant"})
                  )
                } else if newSelectedCountryOption > optionsViewportEnd || true {
                  viewportRef.current
                  ->Nullable.toOption
                  ->Option.forEach(viewport => {
                    Extensions.Dom.scrollTo(
                      viewport,
                      {
                        "top": newSelectedCountryOption->Int.toFloat *. countryOptionHeight,
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
        {isDropdownOpen
          ? <div
              ref={ReactDOM.Ref.domRef(viewportRef)}
              className={css["dropdown"]}
              style={ReactDOM.Style.make(
                ~height=(Math.Int.min(
                  countryOptionsWithIndex->Array.length,
                  maxVisibleCountryOptions,
                )->Int.toFloat *. countryOptionHeight)->Float.toString ++ "px",
                (),
              )}
              onScroll={_ => onScroll()}
              tabIndex={-1}>
              <div
                className={css["country-options-container"]}
                style={ReactDOM.Style.make(
                  ~height=(countryOptionsWithIndex->Array.length->Int.toFloat *.
                    countryOptionHeight)->Float.toString ++ "px",
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
                        if Extensions.Dom.offsetHeight(domRef) != countryOptionHeight {
                          dispatch(SetCountryOptionHeight(Extensions.Dom.offsetHeight(domRef)))
                        }
                      })
                    }}
                    top={Float.toString(
                      (optionsViewportStart + mapIndex)->Int.toFloat *. countryOptionHeight,
                    ) ++ "px"}
                  />
                })
                ->React.array}
              </div>
            </div>
          : React.null}
      </div>
    </div>
  | Error => React.string("error")
  }
}
