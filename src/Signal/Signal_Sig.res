type sig<'a, 'b> = unit => ('a, 'b)

type t<'a> = sig<'a, 'a>

let get: sig<_, 'a> => 'a = fa => snd(fa())

let const = (x: 'a): t<'a> => () => (x, x)

let suspend = (f: unit => 'a): (unit => 'a) => Signal_MobX.computed(f)

let make = (f: unit => 'a): t<'a> =>
  suspend(() => {
    let x = f()
    (x, x)
  })

let use = f => Signal_Internal.use(() => make(f))

let map = (fa: sig<_, 'a>, f: 'a => 'b): t<'b> => make(() => f(snd(fa())))

let useMap = (fa, f) => Signal_Internal.use(() => map(fa, f))

let map2 = (fa: sig<_, 'a>, fb: sig<_, 'b>, f: ('a, 'b) => 'c): t<'c> =>
  make(() => f(snd(fa()), snd(fb())))

let useMap2 = (fa, fb, f) => Signal_Internal.use(() => map(fa, fb, f))

let flatMap: (sig<_, 'a>, 'a => sig<'w, 'b>) => sig<'w, 'b> = (fa, f) =>
  suspend(() => f(snd(fa()))())

let useFlatMap = (fa, f) => Signal_Internal.use(() => flatMap(fa, f))

let component = Signal_MobX.observer

let transaction = Signal_MobX.runInAction

let effect = (fa: sig<_, 'a>, f: 'a => option<unit => unit>): (unit => unit) => {
  let lastState = ref(() => ())
  let clean = Signal_MobX.autorun(() => {
    lastState.contents()
    lastState := (() => ())
    let x = snd(fa())
    let c = f(x)->Belt.Option.getWithDefault(() => ())
    lastState := c
  })
  () => {
    lastState.contents()
    clean(.)
  }
}

let useEffect = (fa: sig<_, 'a>, f: 'a => option<unit => unit>): unit =>
  React.useEffect0(() => {
    let c = effect(fa, f)
    Some(c)
  })
