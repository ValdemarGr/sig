module type Sync = {
  type t<'a>

  let return: unit => t<'a>

  let resolve: 'a => t<'a>

  let fail: exn => t<'a>
}

module PromiseIntegration = (PromiseImplementation: Sync) => {
  type p<'a> = PromiseImplementation.t<'a>

  let fromPromiseWhen = (fa: Signal_Ref.t<p<'a>>, p: Promise.t<'a>, f: 'a => bool) =>
    p
    ->Promise.thenResolve(x =>
      if f(x) {
        fa->Signal_Ref.set(PromiseImplementation.resolve(x))
      } else {
        ()
      }
    )
    ->Promise.catch(e => {
      fa->Signal_Ref.set(PromiseImplementation.fail(e))
      Promise.reject(e)
    })
    ->ignore

  let fromPromise = (fa: Signal_Ref.t<p<'a>>, p: Promise.t<'a>) => fromPromiseWhen(fa, p, _ => true)

  let useFromCancellablePromise = (fa: Signal_Sig.t<Promise.t<'a>>): Signal_Sig.t<p<'a>> => {
    let state = Signal_Ref.use(() => PromiseImplementation.return())

    fa->Signal_Sig.useEffect(p => {
      let cancelled = ref(false)

      state->fromPromiseWhen(p, _ => !cancelled.contents)

      Some(() => cancelled := true)
    })

    state->Signal_Sig.map(x => x)
  }

  let useAsync = (fa, f) => fa->Signal_Sig.useMap(f)->useFromCancellablePromise

  let manageEffect = (fa, f) => {
    let p = f()
    fa->Signal_Ref.set(PromiseImplementation.return())
    fromPromise(fa, p)
  }
}
