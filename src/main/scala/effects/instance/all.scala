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
  with TupleInstances
  with ConstInstances
  with IdentityInstances


object All
  extends All
