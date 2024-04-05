@react.component
let make = () => {
  let (country, setCountry) = React.useState(() => None)

  <div style={ReactDOM.Style.make(~padding="10px", ())}>
    <CountrySelect
      className="custom-class"
      country
      onChange={country => {
        setCountry(_ => country)
        Console.log2("[App.res] Selected country:", country)
      }}
    />
  </div>
}
