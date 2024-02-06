package src.main.wacc

// Error message typeclass
trait ErrMsg {
  // Adds context to an error message if it is not empty
  def withContext[A](ctx: A): String
}

object ErrMsg {
  // Allows the use of the withContext method on any string
  implicit class stringErrMsg(err: String) {
    def withContext[A](ctx: A): String = if (err.nonEmpty) s"$err  in $ctx\n" else ""
  }

  // Error message with mismatching type, requires a context
  def typeErrorMsg(situation: String, context: String, expected: String, got: String): String =
    s"Type mismatch error in $situation\n" +
      s"  Expected '$expected', but got '$got'\n" withContext context
}
