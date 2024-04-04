// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Core__Array from "@rescript/core/src/Core__Array.res.mjs";

function sequence(array) {
  return Core__Array.reduce(array, [], (function (accumulatorOpt, currentElementOpt) {
                if (accumulatorOpt !== undefined && currentElementOpt !== undefined) {
                  return accumulatorOpt.concat([Caml_option.valFromOption(currentElementOpt)]);
                }
                
              }));
}

var $$Array = {
  sequence: sequence
};

var Dom = {};

export {
  $$Array ,
  Dom ,
}
/* No side effect */
