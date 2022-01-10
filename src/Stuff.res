type interface<'a, 'b> = unit => ('a, 'b)

module Internal = {
  let use = (f: unit => 'a): 'a => {
    let (s, _) = React.useState(f)
    s
  }
}

module Sig = {
  type t<'a> = interface<'a, 'a>

  let get: interface<_, 'a> => 'a = fa => snd(fa())

  let const = (x: 'a): t<'a> => () => (x, x)

  let suspend = (f: unit => 'a): (unit => 'a) => MobX.computed(f)

  let make = (f: unit => 'a): t<'a> =>
    suspend(() => {
      let x = f()
      (x, x)
    })

  let use = f => Internal.use(() => make(f))

  let map = (fa: interface<_, 'a>, f: 'a => 'b): t<'b> => make(() => f(snd(fa())))

  let useMap = (fa, f) => Internal.use(() => map(fa, f))

  let map2 = (fa: interface<_, 'a>, fb: interface<_, 'b>, f: ('a, 'b) => 'c): t<'c> =>
    make(() => f(snd(fa()), snd(fb())))

  let useMap2 = (fa, fb, f) => Internal.use(() => map(fa, fb, f))

  let flatMap: (interface<_, 'a>, 'a => interface<'w, 'b>) => interface<'w, 'b> = (fa, f) =>
    suspend(() => f(snd(fa()))())

  let useFlatMap = (fa, f) => Internal.use(() => flatMap(fa, f))
}

module Ref = {
  type t<'a> = interface<ref<'a>, 'a>

  let make = (initial: 'a): t<'a> => {
    let r = MobX.observable(ref(initial))
    () => (r, r.contents)
  }

  let use = (f: unit => 'a) => Internal.use(() => make(f()))

  let set: (t<'a>, 'a) => unit = (fa, a) => fst(fa()) := a
}

module Arr = {
  type t<'a> = interface<array<'a>, array<'a>>

  let make = (initial: array<'a>): t<'a> => {
    () => (initial, initial)
  }

  let use = (f: unit => array<'a>) => Internal.use(() => make(f()))

  let update: (t<'a>, array<'a> => unit) => unit = (fa, f) => f(snd(fa()))
}

let s1 = Sig.const(2)
let s2 = s1->Sig.map(x => x + 1)
let s3 = Ref.make(9)
let s4 = Arr.make([1, 2, 3])
let s5 = s4->Arr.update(xs => xs->Js.Array2.push(4)->ignore)
let s6 = Sig.map2(s3, s2, (x, y) => x + y)

type structure = {nested: Ref.t<int>}

let s = Ref.make({nested: Ref.make(0)})

let flattened = s->Sig.flatMap(x => x.nested)

flattened->Ref.set(9)
