package effects.instance


trait All  extends ListInstances
  with FunctionInstances
  with OptionInstances
  with IOInstances
  with Console
  with BaseUtils
  with EitherInstances


object All
  extends All
