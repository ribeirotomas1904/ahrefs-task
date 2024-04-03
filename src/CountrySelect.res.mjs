// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fetch from "./Fetch.res.mjs";
import * as React from "react";
import * as Core__JSON from "@rescript/core/src/Core__JSON.res.mjs";
import * as Extensions from "./Extensions.res.mjs";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Core__Option from "@rescript/core/src/Core__Option.res.mjs";
import * as JsxRuntime from "react/jsx-runtime";

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
  } else if (typeof state !== "object" && state === "Loading") {
    return {
            TAG: "Loaded",
            _0: action._0
          };
  } else {
    return state;
  }
}

function CountrySelect(props) {
  var match = React.useReducer(reducer, "Initial");
  var dispatch = match[1];
  var state = match[0];
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
  if (typeof state === "object") {
    return JsxRuntime.jsx("div", {
                children: state._0.map(function (country) {
                      return JsxRuntime.jsxs("div", {
                                  children: [
                                    JsxRuntime.jsx("span", {
                                          className: "fi fi-" + country.value
                                        }),
                                    " ",
                                    country.label
                                  ]
                                });
                    })
              });
  }
  switch (state) {
    case "Initial" :
        return "initial";
    case "Loading" :
        return "loading";
    case "Error" :
        return "error";
    
  }
}

var initialState = "Initial";

var make = CountrySelect;

export {
  parseCountries ,
  reducer ,
  initialState ,
  make ,
}
/* react Not a pure module */
