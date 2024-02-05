package src.main.wacc

trait ErrMsg {
  def withContext[A](ctx: A): String
}

object ErrMsg {
  implicit class stringErrMsg(err: String) {
    def withContext[A](ctx: A): String = if (err.nonEmpty) s"$err  in $ctx\n" else ""
  }

  def typeErrorMsg(situation: String, context: String, expected: String, got: String): String =
    s"Type mismatch error in $situation\n" +
      s"  Expected '$expected', but got '$got'\n" withContext context
}
