@react.component
let make = () => {
  let (country, setCountry) = React.useState(() => None)

  <div style={ReactDOM.Style.make(~padding="100px", ())}>
    <CountrySelect
      className="custom-class"
      country
      onChange={country => {
        setCountry(_ => country)
        Js.log(country)
      }}
    />
  </div>
}
