type t<'a> = Signal_Sig.sig<'a => unit, 'a>

let make = (initial: 'a): t<'a> => {
  let r = Signal_MobX.observable(ref(initial))
  () => (x => r := x, r.contents)
}

let use = (f: unit => 'a) => Signal_Internal.use(() => make(f()))

let set: (t<'a>, 'a) => unit = (fa, a) => Signal_Sig.transaction(() => fst(fa())(a))

let imap = (fa: t<'a>, f: 'a => 'b, g: 'b => 'a): t<'b> =>
  Signal_Sig.suspend(() => {
    let (set, x) = fa()
    (b => set(g(b)), f(x))
  })

let useImap = (fa, f, g) => Signal_Internal.use(() => imap(fa, f, g))
