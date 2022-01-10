module TestComponent = {
  @react.component
  let make = Sig.component(() => {
    let state = Ref.use(() => 2)

    let s2 = state->Sig.map(x => x * 2)

    let text = Ref.use(() => "")

    s2->Sig.useEffect(x => {
      text->Ref.set(x->Js.Int.toString)
      None
    })

    <div> 
      <br/>
      {state->Sig.get->Js.Int.toString->React.string} 
      <br/>
      <button onClick={_ => state->Ref.set(state->Sig.get + 1)}>
        {`+`->React.string}
      </button>
      <br/>
      {text->Sig.get->React.string}
    </div>
  })
}

switch ReactDOM.querySelector("#root") {
| Some(elem) =>
  ReactDOM.render(
    <React.StrictMode> <div> {`Holla`->React.string} <TestComponent /> </div> </React.StrictMode>,
    elem,
  )
| None => ()
}
