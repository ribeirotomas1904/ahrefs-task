// Generated by ReScript, PLEASE EDIT WITH CARE

import * as React from "react";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as CountrySelect from "./CountrySelect.res.mjs";
import * as JsxRuntime from "react/jsx-runtime";

function App(props) {
  var match = React.useState(function () {
        
      });
  var setCountry = match[1];
  return JsxRuntime.jsx(JsxRuntime.Fragment, {
              children: Caml_option.some(JsxRuntime.jsx(CountrySelect.make, {
                        className: "custom-class",
                        country: match[0],
                        onChange: (function (country) {
                            setCountry(function (param) {
                                  return country;
                                });
                            console.log(country);
                          })
                      }))
            });
}

var make = App;

export {
  make ,
}
/* react Not a pure module */
