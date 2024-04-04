%%raw(`import "/node_modules/flag-icons/css/flag-icons.min.css";`)
%%raw(`import "./globalStyles.css";`)

switch ReactDOM.querySelector("#root") {
| Some(domElement) =>
  ReactDOM.Client.createRoot(domElement)->ReactDOM.Client.Root.render(
    <React.StrictMode>
      <App />
    </React.StrictMode>,
  )
| None => ()
}
