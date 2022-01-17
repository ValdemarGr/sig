open Belt

type t<'a> = Signal_Sig.sig<array<'a>, array<'a>>

let make = (initial: array<'a>): t<'a> => {
  let o = Signal_MobX.observable(initial)
  () => (o, o)
}

let use = (f: unit => array<'a>) => Signal_Internal.use(() => make(f()))

let update: (t<'a>, array<'a> => 'b) => 'b = (fa, f) => Signal_Sig.transaction(() => f(fst(fa())))

let set = (fa: t<'a>, i, a): bool =>
  fa->update(xs => xs->Array.get(i)->Option.map(x => x := a)->Option.isSome)

// updates whenever the array's structure changes in any way, that is, xs[i] updates whenever xs[0] ... xs[len(xs)-1] updates
// if you wish to only listen to updates at position i, use either a Dict or consider a type of Arr.t<Ref.t<'a>>
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
