type sig<'a, 'b> = (unit => 'a, unit => 'b)

type t<'a> = sig<'a, 'a>

/*
 * This escapes the safe land of sig's suspended combinators.
 * Call this at the "end of the world", e.g a react component's render body,
 * or as an alternative you can also use the effect combinator which can be used to interact with the
 * world outside of sig to maybe set a variable's value.
 *
 * More technically, this will tell all upstream signals that they should evaluate on changes
 * such that the resulting value, for more info visit the official mobx documentation.
 */
let get: sig<_, 'a> => 'a = fa => snd(fa)()

let const = (x: 'a): t<'a> => (() => x, () => x)

let suspend = (f: unit => 'a): (unit => 'a) => Signal_MobX.computed(f)

let make = (f: unit => 'a): t<'a> => {
  let o = suspend(() => f())
  (o, o)
}

let toSig = (fa: sig<_, 'a>): t<'a> => make(() => get(fa))

let useToSig = fa => Signal_Internal.use(() => toSig(fa))

let use = f => Signal_Internal.use(() => make(f))

let tuple = (fa, fb) => make(() => (get(fa), get(fb)))

let useTuple = (fa, fb) => Signal_Internal.use(() => tuple(fa, fb))

let map = (fa: sig<_, 'a>, f: 'a => 'b): t<'b> => make(() => f(get(fa)))

let useMap = (fa, f) => Signal_Internal.use(() => map(fa, f))

let flatMap = (fa: sig<_, 'a>, f: 'a => sig<'w, 'b>): sig<'w, 'b> => {
  let base = suspend(() => f(get(fa)))
  let l = suspend(() => fst(base())())
  let r = suspend(() => snd(base())())
  (l, r)
}

let useFlatMap = (fa, f) => Signal_Internal.use(() => flatMap(fa, f))

let flatten = fa => fa->flatMap(x => x)

let useFlatten = fa => Signal_Internal.use(() => flatten(fa))

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
    let x = get(fa)
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

let arr = (xs: array<t<'a>>): t<array<'a>> => make(() => xs->Js.Array2.map(get))

let useArr = xs => Signal_Internal.use(() => arr(xs))

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
  make(() => f(get(fa), get(fb)))

let useMap2 = (fa, fb, f) => Signal_Internal.use(() => map2(fa, fb, f))

let map3 = (fa, fb, fc, f) => make(() => f(get(fa), get(fb), get(fc)))

let useMap3 = (fa, fb, fc, f) => Signal_Internal.use(() => map3(fa, fb, fc, f))

let map4 = (fa, fb, fc, fd, f) => make(() => f(get(fa), get(fb), get(fc), get(fd)))

let useMap4 = (fa, fb, fc, fd, f) => Signal_Internal.use(() => map4(fa, fb, fc, fd, f))

let map5 = (fa, fb, fc, fd, fe, f) => make(() => f(get(fa), get(fb), get(fc), get(fd), get(fe)))

let useMap5 = (fa, fb, fc, fd, fe, f) => Signal_Internal.use(() => map5(fa, fb, fc, fd, fe, f))

let map6 = (fa, fb, fc, fd, fe, ff, f) =>
  make(() => f(get(fa), get(fb), get(fc), get(fd), get(fe), get(ff)))

let useMap6 = (fa, fb, fc, fd, fe, ff, f) =>
  Signal_Internal.use(() => map6(fa, fb, fc, fd, fe, ff, f))

let map7 = (fa, fb, fc, fd, fe, ff, fg, f) =>
  make(() => f(get(fa), get(fb), get(fc), get(fd), get(fe), get(ff), get(fg)))

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

module Option = {
  let fold = (fa: sig<_, option<'a>>, def: 'b, f: 'a => 'b) =>
    map(fa, Belt.Option.mapWithDefault(_, def, f))

  let useFold = (fa, def, f) => useMap(fa, Belt.Option.mapWithDefault(_, def, f))

  let semiflatMap = (fa: sig<_, option<'a>>, f: 'a => t<'b>): t<option<'b>> =>
    flatMap(fa, Belt.Option.mapWithDefault(_, const(None), x => map(f(x), x => Some(x))))

  let useSemiflatMap = (fa, f) => Signal_Internal.use(() => semiflatMap(fa, f))

  let subflatMap = (fa: sig<_, option<'a>>, f: 'a => option<'b>): t<option<'b>> =>
    map(fa, Belt.Option.flatMap(_, f))

  let useSubflatMap = (fa, f) => Signal_Internal.use(() => subflatMap(fa, f))

  let flatMapF = (fa: sig<_, option<'a>>, f: 'a => t<option<'b>>) =>
    flatMap(fa, Belt.Option.mapWithDefault(_, const(None), f))

  let useFlatMapF = (fa, f) => Signal_Internal.use(() => flatMapF(fa, f))

  let map = (fa: sig<_, option<'a>>, f: 'a => 'b): t<option<'b>> => map(fa, Belt.Option.map(_, f))

  let useMap = (fa: sig<_, option<'a>>, f) => useMap(fa, Belt.Option.map(_, f))
}

module Result = {
  let fold = (fa: sig<_, Belt.Result.t<'a, _>>, def: 'b, f: 'a => 'b) =>
    map(fa, Belt.Result.mapWithDefault(_, def, f))

  let useFold = (fa, def, f) => useMap(fa, Belt.Result.mapWithDefault(_, def, f))

  let semiflatMap = (fa: sig<_, Belt.Result.t<'a, 'e>>, f: 'a => t<'b>): t<Belt.Result.t<'b, 'e>> =>
    flatMap(fa, x =>
      switch x {
      | Belt.Result.Ok(x) => f(x)->map(x => Belt.Result.Ok(x))
      | e => const(e)
      }
    )

  let useSemiflatMap = (fa, f) => Signal_Internal.use(() => semiflatMap(fa, f))

  let subflatMap = (fa: sig<_, Belt.Result.t<'a, 'e>>, f: 'a => Belt.Result.t<'b, 'e>): t<
    Belt.Result.t<'b, 'e>,
  > => map(fa, Belt.Result.flatMap(_, f))

  let useSubflatMap = (fa, f) => Signal_Internal.use(() => subflatMap(fa, f))

  let flatMapF = (fa: sig<_, Belt.Result.t<'a, 'e>>, f: 'a => t<Belt.Result.t<'b, 'e>>) =>
    flatMap(fa, x =>
      switch x {
      | Belt.Result.Ok(x) => f(x)
      | e => const(e)
      }
    )

  let useFlatMapF = (fa, f) => Signal_Internal.use(() => flatMapF(fa, f))

  let map = (fa: sig<_, Belt.Result.t<'a, 'e>>, f: 'a => 'b): t<Belt.Result.t<'b, 'e>> =>
    map(fa, Belt.Result.map(_, f))

  let useMap = (fa, f) => useMap(fa, Belt.Result.map(_, f))
}
