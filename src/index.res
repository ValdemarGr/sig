switch ReactDOM.querySelector("#root") {
| Some(elem) =>
  ReactDOM.render(
    <React.StrictMode> <div> {`Holla`->React.string} </div> </React.StrictMode>,
    elem,
  )
| None => ()
}
