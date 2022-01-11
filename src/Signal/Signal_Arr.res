type t<'a> = Signal_Sig.sig<array<'a>, array<'a>>

let make = (initial: array<'a>): t<'a> => {
  let o = Signal_MobX.observable(initial)
  () => (o, o)
}

let use = (f: unit => array<'a>) => Signal_Internal.use(() => make(f()))

let update: (t<'a>, array<'a> => unit) => unit = (fa, f) => Signal_Sig.transaction(() => f(snd(fa())))
