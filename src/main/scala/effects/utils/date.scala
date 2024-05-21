package effects.utils

import effects.instance.IO

import java.util.Date


object DateIO {
  def date: IO[Date] = IO.create(Date())

}