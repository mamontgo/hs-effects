package effects.utils

import effects.instance.{All, Console, ThreadIO}

trait All extends ThreadIO
  with Console


object All
  extends All
