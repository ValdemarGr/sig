type t<'a> = Signal_Sig.sig<'a => unit, 'a>

let fromRef = (r: ref<'a>): t<'a> => {
  let r = Signal_MobX.observable(r)
  let set = x => r := x
  (() => set, () => r.contents)
}

let make = (initial: 'a): t<'a> => fromRef(ref(initial))

let use = (f: unit => 'a) => Signal_Internal.use(() => make(f()))

let set: (t<'a>, 'a) => unit = (fa, a) => Signal_Sig.transaction(() => {
  let g = fst(fa)()
  g(a)
})

let modify = (fa: t<'a>, f: 'a => ('a, 'b)): 'b =>
  Signal_Sig.transaction(() => {
    let cur = snd(fa)()
    let (a, b) = f(cur)
    fa->set(a)
    b
  })

let update = (fa: t<'a>, f: 'a => 'a): unit => modify(fa, a => (f(a), ()))

let imap = (fa: t<'a>, f: 'a => 'b, g: 'b => 'a): t<'b> => {
  let (set, x) = fa
  let s = () => b => set()(g(b))
  let v = Signal_Sig.suspend(() => f(x()))
  (s, v)
}

let useImap = (fa, f, g) => Signal_Internal.use(() => imap(fa, f, g))
