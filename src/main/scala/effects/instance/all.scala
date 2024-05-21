package effects.instance

import scala.concurrent.ExecutionContext.Implicits.global


trait All extends ListInstances
  with FunctionInstances
  with FutureInstances
  with OptionInstances
  with IOInstances
  with EitherInstances
  with TryInstances
  with WriterInstances


object All
  extends All
