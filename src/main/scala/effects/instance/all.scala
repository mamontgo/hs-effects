package effects.instance


trait All  extends ListInstances
  with FunctionInstances
  with OptionInstances
  with IOInstances
  with Console
  with EitherInstances
  with ThreadIO
  with TryInstances


object All
  extends All
