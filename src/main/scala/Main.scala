
object Main {

  def main(args: Array[String]): Unit = {

    val A = Candidate("A")
    val B = Candidate("B")
    val C = Candidate("C")
    val D = Candidate("D")

    val votes = List(
      List(
        Set(A),
        Set(B),
        Set(C),
        Set(D)
      ),
      List(
        Set(A),
        Set(B),
        Set(C),
        Set(D)
      ),
      List(
        Set(A),
        Set(B),
        Set(C),
        Set(D)
      ),
      List(
        Set(D),
        Set(A),
        Set(B),
        Set(C)
      ),
      List(
        Set(D),
        Set(A),
        Set(B),
        Set(C)
      ),
      List(
        Set(D),
        Set(B),
        Set(C),
        Set(A)
      ),
      List(
        Set(D),
        Set(B),
        Set(C),
        Set(A)
      ),
      List(
        Set(C),
        Set(B),
        Set(D),
        Set(A)
      ),
      List(
        Set(C),
        Set(B),
        Set(D),
        Set(A)
      )
    )

    val schulze = new SchulzeMethod(votes, Set(A, B, C, D))

    println( schulze.getPotentialWinners() )
  }

}

