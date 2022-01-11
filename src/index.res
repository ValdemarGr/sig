module ShowAt = {
  @react.component
  let make = Sig.component((~state: Arr.t<string>, ~idx: int) => {
    let at = state->Sig.map(xs => xs[idx])

    <> <br /> {`State is ${at->Sig.get}`->React.string} </>
  })
}

module TestComponent = {
  type nested = {field: Ref.t<int>}

  @react.component
  let make = Sig.component(() => {
    let nest = Ref.use(() => {field: Ref.make(2)})

    let state = Ref.use(() => 2)

    let imapped =
      state->Ref.imap(Js.Int.toString, x => x->Belt.Int.fromString->Belt.Option.getExn + 2)

    let s2 = state->Sig.useMap(x => x * 2)

    let text = Ref.use(() => "")

    s2->Sig.useEffect(x => {
      text->Ref.set(x->Js.Int.toString)
      None
    })

    let view = nest->Sig.useFlatMap(x => x.field)

    let sum = Sig.map2(state, view, (x, y) => x + y)

    let arrState = Arr.use(() => Belt.Array.range(0, 30)->Belt.Array.map(Belt.Int.toString))

    let selections = Arr.use(() => [])

    let field: Ref.t<option<int>> = Ref.use(() => None)

    <div>
      <br />
      <input onChange={e => field->Ref.set(Some(ReactEvent.Form.target(e)["value"]))} />
      <br />
      <button
        onClick={_ =>
          field
          ->Sig.get
          ->Belt.Option.forEach(x => selections->Arr.update(xs => xs->Js.Array2.push(x)->ignore))}>
        {`Add`->React.string}
      </button>
      <br />
      <button
        onClick={_ =>
          selections->Arr.update(xs =>
            switch field->Sig.get {
            | None => ()
            | Some(x) =>
              xs->Js.Array2.removeFromInPlace(~pos=xs->Js.Array2.findIndex(y => y == x))->ignore
            }
          )}>
        {`Remove`->React.string}
      </button>
      <br />
      {selections->Sig.get->Js.Array2.map(idx => <ShowAt state={arrState} idx />)->React.array}
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
      <button onClick={_ => imapped->Ref.set(view->Sig.get->Js.Int.toString)}>
        {`imap`->React.string}
      </button>
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
