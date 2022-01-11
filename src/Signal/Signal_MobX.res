@module("mobx-react-lite")
external observer: React.component<'props> => React.component<'props> = "observer"

@module("mobx") 
external observable: 'a => 'a = "observable"

@module("mobx")
external runInAction: (unit => 'a) => 'a = "runInAction"

@module("mobx")
external autorun: (unit => unit, . unit) => unit = "autorun"

type computedInterface<'a> = {get: (. unit) => 'a}

@module("mobx")
external _computed: (@uncurry (unit => 'a)) => computedInterface<'a> = "computed"

let computed = (f: unit => 'a): (unit => 'a) => {
  let c = _computed(() => f())
  () => c.get(.)
}
