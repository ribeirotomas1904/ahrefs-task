@react.component
let make = () => {
  let (country, setCountry) = React.useState(() => None)

  <>
    <CountrySelect
      className="custom-class"
      country
      onChange={country => {
        setCountry(_ => country)
        Js.log(country)
      }}
    />
  </>
}
