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

let toSig = (fa: sig<_, 'a>): t<'a> => make(() => get(fa))

let useToSig = fa => Signal_Internal.use(() => toSig(fa))

let use = f => Signal_Internal.use(() => make(f))

let tuple = (fa, fb) => make(() => (get(fa), get(fb)))

let useTuple = (fa, fb) => Signal_Internal.use(() => tuple(fa, fb))

let map = (fa: sig<_, 'a>, f: 'a => 'b): t<'b> => make(() => f(snd(fa())))

let useMap = (fa, f) => Signal_Internal.use(() => map(fa, f))

let flatMap: (sig<_, 'a>, 'a => sig<'w, 'b>) => sig<'w, 'b> = (fa, f) =>
  suspend(() => f(snd(fa()))())

let useFlatMap = (fa, f) => Signal_Internal.use(() => flatMap(fa, f))

let ifM = (fp, ft, ff) =>
  fp->flatMap(x =>
    if x {
      ft
    } else {
      ff
    }
  )

let useIfM = (fp, ft, ff) => Signal_Internal.use(() => ifM(fp, ft, ff))

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

// Tuple boilerplate
let tuple3 = (fa, fb, fc) => make(() => (get(fa), get(fb), get(fc)))

let useTuple3 = (fa, fb, fc) => Signal_Internal.use(() => tuple3(fa, fb, fc))

let tuple4 = (fa, fb, fc, fd) => make(() => (get(fa), get(fb), get(fc), get(fd)))

let useTuple4 = (fa, fb, fc, fd) => Signal_Internal.use(() => tuple4(fa, fb, fc, fd))

let tuple5 = (fa, fb, fc, fd, fe) => make(() => (get(fa), get(fb), get(fc), get(fd), get(fe)))

let useTuple5 = (fa, fb, fc, fd, fe) => Signal_Internal.use(() => tuple5(fa, fb, fc, fd, fe))

let tuple6 = (fa, fb, fc, fd, fe, ff) =>
  make(() => (get(fa), get(fb), get(fc), get(fd), get(fe), get(ff)))

let useTuple6 = (fa, fb, fc, fd, fe, ff) =>
  Signal_Internal.use(() => tuple6(fa, fb, fc, fd, fe, ff))

let tuple7 = (fa, fb, fc, fd, fe, ff, fg) =>
  make(() => (get(fa), get(fb), get(fc), get(fd), get(fe), get(ff), get(fg)))

let useTuple7 = (fa, fb, fc, fd, fe, ff, fg) =>
  Signal_Internal.use(() => tuple7(fa, fb, fc, fd, fe, ff, fg))

// Map boilerplate
let map2 = (fa: sig<_, 'a>, fb: sig<_, 'b>, f: ('a, 'b) => 'c): t<'c> =>
  make(() => f(snd(fa()), snd(fb())))

let useMap2 = (fa, fb, f) => Signal_Internal.use(() => map(fa, fb, f))

let map3 = (fa, fb, fc, f) => make(() => f(get(fa()), get(fb()), get(fc())))

let useMap3 = (fa, fb, fc, f) => Signal_Internal.use(() => map3(fa, fb, fc, f))

let map4 = (fa, fb, fc, fd, f) => make(() => f(get(fa()), get(fb()), get(fc()), get(fd())))

let useMap4 = (fa, fb, fc, fd, f) => Signal_Internal.use(() => map4(fa, fb, fc, fd, f))

let map5 = (fa, fb, fc, fd, fe, f) =>
  make(() => f(get(fa()), get(fb()), get(fc()), get(fd()), get(fe())))

let useMap5 = (fa, fb, fc, fd, fe, f) => Signal_Internal.use(() => map5(fa, fb, fc, fd, fe, f))

let map6 = (fa, fb, fc, fd, fe, ff, f) =>
  make(() => f(get(fa()), get(fb()), get(fc()), get(fd()), get(fe()), get(ff())))

let useMap6 = (fa, fb, fc, fd, fe, ff, f) =>
  Signal_Internal.use(() => map6(fa, fb, fc, fd, fe, ff, f))

let map7 = (fa, fb, fc, fd, fe, ff, fg, f) =>
  make(() => f(get(fa()), get(fb()), get(fc()), get(fd()), get(fe()), get(ff()), get(fg())))

let useMap7 = (fa, fb, fc, fd, fe, ff, fg, f) =>
  Signal_Internal.use(() => map7(fa, fb, fc, fd, fe, ff, fg, f))

// Effect boilerplate
let effect2 = (fa, fb, f) => tuple(fa, fb)->effect(((a, b)) => f(a, b))

let useEffect2 = (fa, fb, f) => useEffect(useTuple(fa, fb), ((a, b)) => f(a, b))

let effect3 = (fa, fb, fc, f) => tuple3(fa, fb, fc)->effect(((a, b, c)) => f(a, b, c))

let useEffect3 = (fa, fb, fc, f) => useEffect(useTuple3(fa, fb, fc), ((a, b, c)) => f(a, b, c))

let effect4 = (fa, fb, fc, fd, f) => tuple4(fa, fb, fc, fd)->effect(((a, b, c, d)) => f(a, b, c, d))

let useEffect4 = (fa, fb, fc, fd, f) =>
  useEffect(useTuple4(fa, fb, fc, fd), ((a, b, c, d)) => f(a, b, c, d))

let effect5 = (fa, fb, fc, fd, fe, f) =>
  tuple5(fa, fb, fc, fd, fe)->effect(((a, b, c, d, e)) => f(a, b, c, d, e))

let useEffect5 = (fa, fb, fc, fd, fe, f) =>
  useEffect(useTuple5(fa, fb, fc, fd, fe), ((a, b, c, d, e)) => f(a, b, c, d, e))

let effect6 = (fa, fb, fc, fd, fe, ff, f) =>
  tuple6(fa, fb, fc, fd, fe, ff)->effect(((a, b, c, d, e, h)) => f(a, b, c, d, e, h))

let useEffect6 = (fa, fb, fc, fd, fe, ff, f) =>
  useEffect(useTuple6(fa, fb, fc, fd, fe, ff), ((a, b, c, d, e, h)) => f(a, b, c, d, e, h))

let effect7 = (fa, fb, fc, fd, fe, ff, fg, f) =>
  tuple7(fa, fb, fc, fd, fe, ff, fg)->effect(((a, b, c, d, e, h, g)) => f(a, b, c, d, e, h, g))

let useEffect7 = (fa, fb, fc, fd, fe, ff, fg, f) =>
  useEffect(useTuple7(fa, fb, fc, fd, fe, ff, fg), ((a, b, c, d, e, h, g)) =>
    f(a, b, c, d, e, h, g)
  )
