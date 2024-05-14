package effects.instance


trait All  extends ListInstances
  with FunctionInstances
  with OptionInstances
  with IOInstances
  with Console
  with BaseUtils
  with EitherInstances
  with ThreadIO


object All
  extends All
