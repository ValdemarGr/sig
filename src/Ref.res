type t<'a> = Sig.sig<'a => unit, 'a>

let make = (initial: 'a): t<'a> => {
  let r = MobX.observable(ref(initial))
  () => (x => r := x, r.contents)
}

let use = (f: unit => 'a) => Internal.use(() => make(f()))

let set: (t<'a>, 'a) => unit = (fa, a) => Sig.transaction(() => fst(fa())(a))

let imap = (fa: t<'a>, f: 'a => 'b, g: 'b => 'a): t<'b> =>
  Sig.suspend(() => {
    let (set, x) = fa()
    (b => set(g(b)), f(x))
  })

let useImap = (fa, f, g) => Internal.use(() => imap(fa, f, g))
