open Belt

type t<'a> = Signal_Sig.sig<array<ref<'a>>, array<'a>>

let make = (initial: array<'a>): t<'a> => {
  let o = Signal_MobX.observable(initial->Array.map(ref))
  (() => o, () => o->Array.map(x => x.contents))
}

let use = (f: unit => array<'a>) => Signal_Internal.use(() => make(f()))

let update: (t<'a>, array<ref<'a>> => 'b) => 'b = (fa, f) =>
  Signal_Sig.transaction(() => f(fst(fa)()))

let pushMany = (fa: t<'a>, ys: array<'a>) =>
  fa->update(xs => {
    let news = Signal_MobX.observable(ys->Array.map(ref))
    xs->Js.Array2.pushMany(news)
  })

let push = (fa, x) => pushMany(fa, [x])

let unsiftMany = (fa: t<'a>, ys: array<'a>) =>
  fa->update(xs => {
    let news = Signal_MobX.observable(ys->Array.map(ref))
    xs->Js.Array2.unshiftMany(news)
  })

let unshift = (fa, x) => unsiftMany(fa, [x])

let shift = (fa: t<'a>) =>
  Signal_Sig.make(() => fa->update(xs => xs->Js.Array2.shift)->Belt.Option.map(x => x.contents))

let set = (fa: t<'a>, i, a): bool =>
  fa->update(xs => xs->Array.get(i)->Option.map(x => x := a)->Option.isSome)

let get = (fa: t<'a>, i) => fa->Signal_Sig.map(xs => xs->Array.get(i))

let getUnsafe = (fa: t<'a>, i) =>
  get(fa, i)->Signal_Sig.map(x => {
    switch x {
    | None =>
      Js.Exn.raiseError(
        `getUnsafe: index ${i->Int.toString} out of bounds of ${Signal_Sig.transaction(() =>
            fa->Signal_Sig.get->Array.length->Int.toString
          )}`,
      )
    | Some(x) => x
    }
  })

let useGet = (fa: t<'a>, i) => Signal_Internal.use(() => get(fa, i))

let useGetUnsafe = (fa: t<'a>, i) => Signal_Internal.use(() => getUnsafe(fa, i))

let map = (fa: t<'a>, f) => fa->Signal_Sig.map(xs => xs->Array.map(f))

let useMap = (fa: t<'a>, f) => Signal_Internal.use(() => map(fa, f))

let filter = (fa: t<'a>, f) => fa->Signal_Sig.map(xs => xs->Array.keep(f))

let useFilter = (fa: t<'a>, f) => Signal_Internal.use(() => filter(fa, f))

let fold = (fa: t<'a>, a, f) => fa->Signal_Sig.map(xs => xs->Array.reduce(a, f))
