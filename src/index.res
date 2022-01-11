module TestComponent = {
  type nested = {field: Ref.t<int>}

  @react.component
  let make = Sig.component(() => {
    let nest = Ref.use(() => {field: Ref.make(2)})

    let state = Ref.use(() => 2)

    let imapped = state->Ref.imap(Js.Int.toString, x => x->Belt.Int.fromString->Belt.Option.getExn + 2)

    let s2 = state->Sig.useMap(x => x * 2)

    let text = Ref.use(() => "")

    s2->Sig.useEffect(x => {
      text->Ref.set(x->Js.Int.toString)
      None
    })

    let view = nest->Sig.useFlatMap(x => x.field)

    let sum = Sig.map2(state, view, (x, y) => x + y)

    <div>
      <br />
      {sum->Sig.get->Js.Int.toString->React.string}
      <br />
      {view->Sig.get->Js.Int.toString->React.string}
      <br />
      {state->Sig.get->Js.Int.toString->React.string}
      <br />
      <button onClick={_ => state->Ref.set(state->Sig.get + 1)}> {`+`->React.string} </button>
      <br />
      <button onClick={_ => view->Ref.set(view->Sig.get + 1)}> {`+`->React.string} </button>
      <br />
      <button onClick={_ => nest->Ref.set({field: Ref.make(0)})}> {`+`->React.string} </button>
      <br />
      {text->Sig.get->React.string}
      <br />
      <button onClick={_ => imapped->Ref.set(view->Sig.get->Js.Int.toString)}> {`imap`->React.string} </button>
      <br />
      {imapped->Sig.get->React.string}
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
