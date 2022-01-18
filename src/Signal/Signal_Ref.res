type t<'a> = Signal_Sig.sig<'a => unit, 'a>

let fromRef = (r: ref<'a>): t<'a> => {
  let r = Signal_MobX.observable(r)
  (() => x => r := x, () => r.contents)
}

let make = (initial: 'a): t<'a> => fromRef(ref(initial))

let use = (f: unit => 'a) => Signal_Internal.use(() => make(f()))

let set: (t<'a>, 'a) => unit = (fa, a) => Signal_Sig.transaction(() => fst(fa)()(a))

let imap = (fa: t<'a>, f: 'a => 'b, g: 'b => 'a): t<'b> => {
  let (set, x) = fa
  let s = () => b => set()(g(b))
  let v = Signal_Sig.suspend(() => f(x()))
  (s, v)
}

let useImap = (fa, f, g) => Signal_Internal.use(() => imap(fa, f, g))
