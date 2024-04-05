module Array = {
  let sequence = array => {
    Array.reduce(array, Some([]), (accumulatorOpt, currentElementOpt) => {
      switch (accumulatorOpt, currentElementOpt) {
      | (Some(accumulator), Some(currentElement)) =>
        Array.concat(accumulator, [currentElement])->Some
      | _ => None
      }
    })
  }
}

module Dom = {
  @get external scrollTop: Dom.element => int = "scrollTop"
  @send external focus: Dom.element => unit = "focus"
  @send external blur: Dom.element => unit = "blur"
  @get external offsetHeight: Dom.element => float = "offsetHeight"
  @send external contains: (Dom.element, Dom.element) => bool = "contains"
  @send external scrollTo: (Dom.element, {"top": float, "behavior": string}) => unit = "scrollTo"
}
