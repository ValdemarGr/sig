let use = (f: unit => 'a): 'a => {
  let (s, _) = React.useState(f)
  s
}
