module Array = {
    
    let sequence = (array) => {
        Array.reduce(array, Some([]), (accumulatorOpt, currentElementOpt) => {
            switch (accumulatorOpt, currentElementOpt) {
            | (Some(accumulator), Some(currentElement)) => Array.concat(accumulator, [currentElement])->Some
            | _ => None
            }
        })
    }
    
}
