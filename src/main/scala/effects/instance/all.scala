package effects.instance


trait All  extends ListInstances
  with FunctionInstances
  with OptionInstances
  with IOInstances
  with EitherInstances
  with TryInstances


object All
  extends All
