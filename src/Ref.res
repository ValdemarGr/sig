type t<'a> = Sig.sig<ref<'a>, 'a>

let make = (initial: 'a): t<'a> => {
  let r = MobX.observable(ref(initial))
  () => (r, r.contents)
}

let use = (f: unit => 'a) => Internal.use(() => make(f()))

let set: (t<'a>, 'a) => unit = (fa, a) => fst(fa()) := a
