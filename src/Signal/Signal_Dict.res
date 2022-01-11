type t<'a> = Signal_Sig.sig<Js.Dict.t<'a>, Js.Dict.t<'a>>

let make = (initial: Js.Dict.t<'a>): t<'a> => {
  let o = Signal_MobX.observable(initial)
  () => (o, o)
}

let use = (f: unit => Js.Dict.t<'a>) => Signal_Internal.use(() => make(f()))

let update: (t<'a>, Js.Dict.t<'a> => unit) => unit = (fa, f) =>
  Signal_Sig.transaction(() => f(snd(fa())))
