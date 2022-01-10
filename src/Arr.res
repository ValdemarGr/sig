type t<'a> = Sig.sig<array<'a>, array<'a>>

let make = (initial: array<'a>): t<'a> => {
  () => (initial, initial)
}

let use = (f: unit => array<'a>) => Internal.use(() => make(f()))

let update: (t<'a>, array<'a> => unit) => unit = (fa, f) => f(snd(fa()))
