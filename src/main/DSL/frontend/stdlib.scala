package DSL.frontend

object stdlib {
  val source: String = 
    """
    |func explodeN(x, n) {
    |    if v = ~x; n == 0 {
    |       0
    |    } elif v == 6 {
    |        explodeN(x, n - 1) + v
    |    } else {
    |        v
    |    }
    |}
    |
    |func explode(x) { explodeN(x, 10) }
    |""".stripMargin.trim
}