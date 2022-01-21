type t<'a> = Signal_Sig.sig<Js.Dict.t<'a>, Js.Dict.t<'a>>

let make = (initial: Js.Dict.t<'a>): t<'a> => {
  let o = Signal_MobX.observable(initial)
  (() => o, () => o)
}

let use = (f: unit => Js.Dict.t<'a>) => Signal_Internal.use(() => make(f()))

let update: (t<'a>, Js.Dict.t<'a> => unit) => unit = (fa, f) =>
  Signal_Sig.transaction(() => f(snd(fa)()))

let set = (fa, k, v) => fa->update(d => d->Js.Dict.set(k, v))

let get = (fa, k) => fa->Signal_Sig.map(d => d->Js.Dict.get(k))

let useGet = (fa, k) => Signal_Internal.use(() => get(fa, k))

module Internal = {
  external unsafeCastDictValues: Js.Dict.t<'a> => Js.Dict.t<string> = "%identity"
}

let remove = (fa: t<'a>, k: string) => {
  fa->update(d => Js.Dict.unsafeDeleteKey(. d->Internal.unsafeCastDictValues, k))
}
