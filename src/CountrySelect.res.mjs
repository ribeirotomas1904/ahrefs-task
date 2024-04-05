// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fetch from "./Fetch.res.mjs";
import * as React from "react";
import * as Core__JSON from "@rescript/core/src/Core__JSON.res.mjs";
import * as Extensions from "./Extensions.res.mjs";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Core__Option from "@rescript/core/src/Core__Option.res.mjs";
import * as JsxRuntime from "react/jsx-runtime";
import CountrySelectModuleCss from "./CountrySelect.module.css";

var css = CountrySelectModuleCss;

function parseCountries(json) {
  return Core__Option.flatMap(Core__JSON.Decode.array(json), (function (value) {
                return Extensions.$$Array.sequence(value.map(function (value) {
                                return Core__Option.flatMap(Core__JSON.Decode.object(value), (function (value) {
                                              var match = Object.entries(value).length === 2;
                                              var match$1 = Core__Option.flatMap(value["label"], Core__JSON.Decode.string);
                                              var match$2 = Core__Option.flatMap(value["value"], Core__JSON.Decode.string);
                                              if (match && match$1 !== undefined && match$2 !== undefined) {
                                                return {
                                                        label: match$1,
                                                        value: match$2
                                                      };
                                              }
                                              
                                            }));
                              }));
              }));
}

function reducer(state, action) {
  if (typeof action !== "object") {
    if (action === "StartLoading") {
      if (typeof state !== "object" && state === "Initial") {
        return "Loading";
      } else {
        return state;
      }
    } else if (typeof state !== "object" && state === "Loading") {
      return "Error";
    } else {
      return state;
    }
  }
  switch (action.TAG) {
    case "Load" :
        if (typeof state !== "object" && state === "Loading") {
          return {
                  TAG: "Loaded",
                  countryOptions: action._0,
                  searchInput: "",
                  optionsViewportStart: 0,
                  optionsViewportEnd: 13,
                  isDropdownOpen: false,
                  countryOptionHeight: 0.0,
                  selectedCountryOption: 0
                };
        } else {
          return state;
        }
    case "SetSearchInput" :
        if (typeof state !== "object") {
          return state;
        } else {
          return {
                  TAG: "Loaded",
                  countryOptions: state.countryOptions,
                  searchInput: action._0,
                  optionsViewportStart: state.optionsViewportStart,
                  optionsViewportEnd: state.optionsViewportEnd,
                  isDropdownOpen: state.isDropdownOpen,
                  countryOptionHeight: state.countryOptionHeight,
                  selectedCountryOption: state.selectedCountryOption
                };
        }
    case "SetOptionsViewport" :
        if (typeof state !== "object") {
          return state;
        } else {
          return {
                  TAG: "Loaded",
                  countryOptions: state.countryOptions,
                  searchInput: state.searchInput,
                  optionsViewportStart: action.optionsViewportStart,
                  optionsViewportEnd: action.optionsViewportEnd,
                  isDropdownOpen: state.isDropdownOpen,
                  countryOptionHeight: state.countryOptionHeight,
                  selectedCountryOption: state.selectedCountryOption
                };
        }
    case "SetIsDropdownOpen" :
        var isDropdownOpen = action._0;
        if (typeof state !== "object" || state.isDropdownOpen === isDropdownOpen) {
          return state;
        } else {
          return {
                  TAG: "Loaded",
                  countryOptions: state.countryOptions,
                  searchInput: "",
                  optionsViewportStart: 0,
                  optionsViewportEnd: 13,
                  isDropdownOpen: isDropdownOpen,
                  countryOptionHeight: state.countryOptionHeight,
                  selectedCountryOption: 0
                };
        }
    case "SetCountryOptionHeight" :
        if (typeof state !== "object") {
          return state;
        } else {
          return {
                  TAG: "Loaded",
                  countryOptions: state.countryOptions,
                  searchInput: state.searchInput,
                  optionsViewportStart: state.optionsViewportStart,
                  optionsViewportEnd: state.optionsViewportEnd,
                  isDropdownOpen: state.isDropdownOpen,
                  countryOptionHeight: action._0,
                  selectedCountryOption: state.selectedCountryOption
                };
        }
    case "SetSelectedCountryOption" :
        if (typeof state !== "object") {
          return state;
        } else {
          return {
                  TAG: "Loaded",
                  countryOptions: state.countryOptions,
                  searchInput: state.searchInput,
                  optionsViewportStart: state.optionsViewportStart,
                  optionsViewportEnd: state.optionsViewportEnd,
                  isDropdownOpen: state.isDropdownOpen,
                  countryOptionHeight: state.countryOptionHeight,
                  selectedCountryOption: action._0
                };
        }
    
  }
}

function CountrySelect$CountryOption(props) {
  var onCancelSelect = props.onCancelSelect;
  var onMouseEnter = props.onMouseEnter;
  var onChange = props.onChange;
  return JsxRuntime.jsxs("div", {
              children: [
                JsxRuntime.jsx("span", {
                      className: "fi fi-" + props.value
                    }),
                JsxRuntime.jsx("span", {
                      children: props.label
                    })
              ],
              ref: Caml_option.some(props.onRef),
              className: css["country-option"] + " " + (
                props.isSelected ? css["country-option-selected"] : ""
              ),
              style: {
                top: props.top
              },
              onClick: (function (param) {
                  onChange();
                }),
              onMouseEnter: (function (param) {
                  onMouseEnter();
                }),
              onTouchCancel: (function (param) {
                  onCancelSelect();
                }),
              onTouchEnd: (function (param) {
                  onChange();
                }),
              onTouchMove: (function (param) {
                  onCancelSelect();
                }),
              onTouchStart: (function (param) {
                  onMouseEnter();
                })
            });
}

var CountryOption = {
  make: CountrySelect$CountryOption
};

function CountrySelect$SearchIcon(props) {
  return JsxRuntime.jsx("svg", {
              children: JsxRuntime.jsx("path", {
                    clipRule: "evenodd",
                    d: "M6 11C7.01929 11 7.96734 10.695 8.75787 10.1713L12.06 13.47L13.47 12.06L10.1713 8.75783C10.695 7.96731 11 7.01927 11 6C11 3.23858 8.76142 1 6 1C3.23858 1 1 3.23858 1 6C1 8.76142 3.23858 11 6 11ZM9.2 6C9.2 7.76731 7.76731 9.2 6 9.2C4.23269 9.2 2.8 7.76731 2.8 6C2.8 4.23269 4.23269 2.8 6 2.8C7.76731 2.8 9.2 4.23269 9.2 6Z",
                    fill: "#333333",
                    fillRule: "evenodd"
                  }),
              className: props.className,
              height: "14",
              width: "14",
              viewBox: "0 0 14 14",
              xmlns: "http://www.w3.org/2000/svg"
            });
}

var SearchIcon = {
  make: CountrySelect$SearchIcon
};

function CountrySelect$TriangleIcon(props) {
  return JsxRuntime.jsx("svg", {
              children: JsxRuntime.jsx("path", {
                    clipRule: "evenodd",
                    d: "M0 2H8L4 7L0 2Z",
                    fill: "#333333",
                    fillRule: "evenodd"
                  }),
              height: "8",
              width: "8",
              viewBox: "0 0 8 8",
              xmlns: "http://www.w3.org/2000/svg"
            });
}

var TriangleIcon = {
  make: CountrySelect$TriangleIcon
};

function CountrySelect(props) {
  var onChange = props.onChange;
  var __className = props.className;
  var className = __className !== undefined ? __className : "";
  var match = React.useReducer(reducer, "Initial");
  var dispatch = match[1];
  var state = match[0];
  var searchInputRef = React.useRef(null);
  var viewportRef = React.useRef(null);
  React.useEffect((function () {
          var controller = new AbortController();
          dispatch("StartLoading");
          Fetch.get("https://gist.githubusercontent.com/rusty-key/659db3f4566df459bd59c8a53dc9f71f/raw/4127f9550ef063121c564025f6d27dceeb279623/counties.json", Caml_option.some(controller.signal)).then(function (prim) {
                  return prim.json();
                }).then(function (json) {
                var countries = parseCountries(json);
                if (countries !== undefined) {
                  return dispatch({
                              TAG: "Load",
                              _0: countries
                            });
                } else {
                  return dispatch("Error");
                }
              });
          return (function () {
                    controller.abort();
                  });
        }), []);
  if (typeof state !== "object") {
    switch (state) {
      case "Initial" :
      case "Loading" :
      case "Error" :
          return "TODO";
      
    }
  } else {
    var selectedCountryOption = state.selectedCountryOption;
    var countryOptionHeight = state.countryOptionHeight;
    var optionsViewportEnd = state.optionsViewportEnd;
    var optionsViewportStart = state.optionsViewportStart;
    var searchInput = state.searchInput;
    var countryOptions = state.countryOptions;
    var countryOptionsWithIndex = countryOptions.filter(function (countryOption) {
          return countryOption.label.toLowerCase().includes(searchInput.trim().toLowerCase());
        });
    var countryOptionsWithIndex$1 = countryOptionsWithIndex.map(function (countryOption, index) {
          return [
                  countryOption,
                  index
                ];
        });
    var onChangeHandler = function () {
      Core__Option.forEach(countryOptionsWithIndex$1[selectedCountryOption], (function (param) {
              onChange(param[0].value);
              dispatch({
                    TAG: "SetIsDropdownOpen",
                    _0: false
                  });
              Core__Option.forEach(Caml_option.nullable_to_opt(searchInputRef.current), (function (prim) {
                      prim.blur();
                    }));
            }));
    };
    var onScroll = function () {
      Core__Option.forEach(Caml_option.nullable_to_opt(viewportRef.current), (function (viewport) {
              var optionsViewportStart = Math.round(viewport.scrollTop / countryOptionHeight) | 0;
              dispatch({
                    TAG: "SetOptionsViewport",
                    optionsViewportStart: optionsViewportStart,
                    optionsViewportEnd: optionsViewportStart + 13 | 0
                  });
            }));
      Core__Option.forEach(Caml_option.nullable_to_opt(searchInputRef.current), (function (prim) {
              prim.focus();
            }));
    };
    var match$1 = countryOptionsWithIndex$1.length > 0;
    return JsxRuntime.jsxs("div", {
                children: [
                  JsxRuntime.jsx("div", {
                        children: Core__Option.getOr(Core__Option.flatMap(Core__Option.flatMap(props.country, (function (country) {
                                        return countryOptions.find(function (countryOption) {
                                                    return countryOption.value === country;
                                                  });
                                      })), (function (countryOption) {
                                    return Caml_option.some(JsxRuntime.jsxs(JsxRuntime.Fragment, {
                                                    children: [
                                                      JsxRuntime.jsx("span", {
                                                            className: "fi fi-" + countryOption.value
                                                          }),
                                                      JsxRuntime.jsx("span", {
                                                            children: countryOption.label
                                                          }),
                                                      JsxRuntime.jsx(CountrySelect$TriangleIcon, {})
                                                    ]
                                                  }));
                                  })), JsxRuntime.jsxs(JsxRuntime.Fragment, {
                                  children: [
                                    JsxRuntime.jsx("span", {
                                          children: "Select country"
                                        }),
                                    JsxRuntime.jsx(CountrySelect$TriangleIcon, {})
                                  ]
                                })),
                        className: css["selected-country"],
                        onClick: (function (param) {
                            Core__Option.forEach(Caml_option.nullable_to_opt(searchInputRef.current), (function (prim) {
                                    prim.focus();
                                  }));
                          })
                      }),
                  JsxRuntime.jsxs("div", {
                        children: [
                          JsxRuntime.jsxs("div", {
                                children: [
                                  JsxRuntime.jsx(CountrySelect$SearchIcon, {
                                        className: css["search-icon"]
                                      }),
                                  JsxRuntime.jsx("input", {
                                        ref: Caml_option.some(searchInputRef),
                                        className: css["search-input"],
                                        placeholder: "Search",
                                        value: searchInput,
                                        onKeyDown: (function (e) {
                                            var match = e.code;
                                            switch (match) {
                                              case "ArrowDown" :
                                                  e.preventDefault();
                                                  var newSelectedCountryOption = (selectedCountryOption + 1 | 0) > (countryOptionsWithIndex$1.length - 1 | 0) ? 0 : selectedCountryOption + 1 | 0;
                                                  if (newSelectedCountryOption === 0) {
                                                    Core__Option.forEach(Caml_option.nullable_to_opt(viewportRef.current), (function (viewport) {
                                                            viewport.scrollTo({
                                                                  top: 0.0,
                                                                  behavior: "instant"
                                                                });
                                                          }));
                                                  } else if (newSelectedCountryOption < optionsViewportStart || newSelectedCountryOption > optionsViewportEnd) {
                                                    Core__Option.forEach(Caml_option.nullable_to_opt(viewportRef.current), (function (viewport) {
                                                            viewport.scrollTo({
                                                                  top: (newSelectedCountryOption - 13 | 0) * countryOptionHeight,
                                                                  behavior: "auto"
                                                                });
                                                          }));
                                                  }
                                                  return dispatch({
                                                              TAG: "SetSelectedCountryOption",
                                                              _0: newSelectedCountryOption
                                                            });
                                              case "ArrowUp" :
                                                  e.preventDefault();
                                                  var newSelectedCountryOption$1 = (selectedCountryOption - 1 | 0) < 0 ? countryOptionsWithIndex$1.length - 1 | 0 : selectedCountryOption - 1 | 0;
                                                  if (newSelectedCountryOption$1 === (countryOptionsWithIndex$1.length - 1 | 0)) {
                                                    Core__Option.forEach(Caml_option.nullable_to_opt(viewportRef.current), (function (viewport) {
                                                            viewport.scrollTo({
                                                                  top: countryOptionsWithIndex$1.length * countryOptionHeight,
                                                                  behavior: "instant"
                                                                });
                                                            setTimeout((function () {
                                                                    viewport.scrollTo({
                                                                          top: countryOptionsWithIndex$1.length * countryOptionHeight,
                                                                          behavior: "instant"
                                                                        });
                                                                  }), 10);
                                                          }));
                                                  } else if (newSelectedCountryOption$1 < optionsViewportStart || newSelectedCountryOption$1 > optionsViewportEnd) {
                                                    Core__Option.forEach(Caml_option.nullable_to_opt(viewportRef.current), (function (viewport) {
                                                            viewport.scrollTo({
                                                                  top: newSelectedCountryOption$1 * countryOptionHeight,
                                                                  behavior: "instant"
                                                                });
                                                          }));
                                                  }
                                                  return dispatch({
                                                              TAG: "SetSelectedCountryOption",
                                                              _0: newSelectedCountryOption$1
                                                            });
                                              case "Enter" :
                                                  return onChangeHandler();
                                              case "Escape" :
                                                  dispatch({
                                                        TAG: "SetIsDropdownOpen",
                                                        _0: false
                                                      });
                                                  return Core__Option.forEach(Caml_option.nullable_to_opt(searchInputRef.current), (function (prim) {
                                                                prim.blur();
                                                              }));
                                              default:
                                                return ;
                                            }
                                          }),
                                        onFocus: (function (param) {
                                            dispatch({
                                                  TAG: "SetIsDropdownOpen",
                                                  _0: true
                                                });
                                          }),
                                        onBlur: (function (e) {
                                            var relatedTarget = e.relatedTarget;
                                            if (relatedTarget == null) {
                                              return dispatch({
                                                          TAG: "SetIsDropdownOpen",
                                                          _0: false
                                                        });
                                            } else {
                                              return Core__Option.forEach(Caml_option.nullable_to_opt(viewportRef.current), (function (viewport) {
                                                            if (viewport.contains(relatedTarget)) {
                                                              return ;
                                                            } else {
                                                              return dispatch({
                                                                          TAG: "SetIsDropdownOpen",
                                                                          _0: false
                                                                        });
                                                            }
                                                          }));
                                            }
                                          }),
                                        onChange: (function (e) {
                                            dispatch({
                                                  TAG: "SetSearchInput",
                                                  _0: e.target.value
                                                });
                                          })
                                      })
                                ],
                                className: css["search-input-container"]
                              }),
                          state.isDropdownOpen ? (
                              match$1 ? JsxRuntime.jsx("div", {
                                      children: JsxRuntime.jsx("div", {
                                            children: countryOptionsWithIndex$1.slice(optionsViewportStart, optionsViewportEnd + 1 | 0).map(function (param) {
                                                  var countryOptionIndex = param[1];
                                                  var countryOption = param[0];
                                                  return JsxRuntime.jsx(CountrySelect$CountryOption, {
                                                              label: countryOption.label,
                                                              value: countryOption.value,
                                                              onChange: onChangeHandler,
                                                              onMouseEnter: (function () {
                                                                  dispatch({
                                                                        TAG: "SetSelectedCountryOption",
                                                                        _0: countryOptionIndex
                                                                      });
                                                                }),
                                                              onCancelSelect: (function () {
                                                                  dispatch({
                                                                        TAG: "SetSelectedCountryOption",
                                                                        _0: -1
                                                                      });
                                                                }),
                                                              onRef: (function (domRef) {
                                                                  Core__Option.forEach((domRef == null) ? undefined : Caml_option.some(domRef), (function (domRef) {
                                                                          if (domRef.offsetHeight !== countryOptionHeight) {
                                                                            return dispatch({
                                                                                        TAG: "SetCountryOptionHeight",
                                                                                        _0: domRef.offsetHeight
                                                                                      });
                                                                          }
                                                                          
                                                                        }));
                                                                }),
                                                              top: (countryOptionIndex * countryOptionHeight).toString() + "px",
                                                              isSelected: countryOptionIndex === selectedCountryOption
                                                            }, countryOption.value);
                                                }),
                                            className: css["country-options-container"],
                                            style: {
                                              height: (countryOptionsWithIndex$1.length * countryOptionHeight).toString() + "px"
                                            }
                                          }),
                                      ref: Caml_option.some(viewportRef),
                                      className: css.dropdown,
                                      style: {
                                        height: (Math.min(countryOptionsWithIndex$1.length, 14) * countryOptionHeight + 5.0).toString() + "px"
                                      },
                                      tabIndex: -1,
                                      onScroll: (function (param) {
                                          onScroll();
                                        })
                                    }) : JsxRuntime.jsx("div", {
                                      children: JsxRuntime.jsx("div", {
                                            children: "No options",
                                            className: css["no-options"]
                                          }),
                                      className: css.dropdown,
                                      tabIndex: -1
                                    })
                            ) : null
                        ],
                        className: css.wrapper
                      })
                ],
                className: css["country-select"] + " " + className
              });
  }
}

var maxVisibleCountryOptions = 14;

var initialState = "Initial";

var make = CountrySelect;

export {
  css ,
  maxVisibleCountryOptions ,
  parseCountries ,
  reducer ,
  initialState ,
  CountryOption ,
  SearchIcon ,
  TriangleIcon ,
  make ,
}
/* css Not a pure module */
