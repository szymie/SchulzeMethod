class SchulzeMethod(votes: List[List[Set[Candidate]]], candidates: Set[Candidate]) {

  val candidatesSize = candidates.size
  val defeats = defeatsMatrix()
  val paths = updatedPathMatrix(initialPathsMatrix(defeats))

  private def defeatsMatrix(): Map[(Candidate, Candidate), Int] = {

    votes.foldLeft(candidatesMatrix((_, _) => 0)) {
      (acc: Map[(Candidate, Candidate), Int], vote: List[Set[Candidate]]) =>

        val (newAcc, _) = vote.foldLeft((acc, Set[Candidate]())) {
          case ((acc: Map[(Candidate, Candidate), Int], dominantCandidates: Set[Candidate]), votePosition: Set[Candidate]) =>

            val newDominantCandidates = dominantCandidates ++ votePosition

            val newAcc = votePosition.foldLeft(acc) {
              (acc: Map[(Candidate, Candidate), Int], candidate: Candidate) =>

                val dominatedCandidates = candidates -- newDominantCandidates

                dominatedCandidates.foldLeft(acc) {
                  (acc: Map[(Candidate, Candidate), Int], dominatedCandidate: Candidate) =>
                    val newDefeatValue = acc.getOrElse((candidate, dominatedCandidate), 0) + 1
                    acc + ((candidate, dominatedCandidate) -> newDefeatValue)
                }
            }

            (newAcc, newDominantCandidates)
        }

        newAcc
    }
  }

  private def candidatesMatrix(initialValue: (Candidate, Candidate) => Int): Map[(Candidate, Candidate), Int] = {

    candidates.foldLeft(Map[(Candidate, Candidate), Int]()) {
      (acc: Map[(Candidate, Candidate), Int], leftCandidate: Candidate) =>
        candidates.foldLeft(acc) {
          (acc: Map[(Candidate, Candidate), Int], rightCandidate: Candidate) =>
            if(leftCandidate != rightCandidate) acc + ((leftCandidate, rightCandidate) -> initialValue(leftCandidate, rightCandidate))
            else acc
        }
    }
  }

  private def initialPathsMatrix(defeats: Map[(Candidate, Candidate), Int]): Map[(Candidate, Candidate), Int] = {

    candidatesMatrix {
      (leftCandidate: Candidate, rightCandidate: Candidate) =>
        (defeats.get((leftCandidate, rightCandidate)), defeats.get((rightCandidate, leftCandidate))) match {
          case (Some(leftDefeat), Some(rightDefeat)) =>
            if(leftDefeat > rightDefeat) leftDefeat
            else 0
          case _ => 0
        }
    }
  }

  private def updatedPathMatrix(paths: Map[(Candidate, Candidate), Int]): Map[(Candidate, Candidate), Int] = {

    paths.foldLeft(paths) {
      case (acc, ((i, j), _)) =>
        (candidates - i - j).foldLeft(acc) {
          (acc, k) =>
            acc + ((j, k) -> Math.max(acc((j, k)), Math.min(acc((j, i)), acc((i, k)))))
        }
    }
  }

  def getPotentialWinners(): Set[Candidate] = {

    candidates.filter {
      (candidate: Candidate) =>

        val candidatesToBeat = candidates - candidate

        !candidatesToBeat.exists {
          (candidateToBeat: Candidate) =>
            paths((candidateToBeat, candidate)) > paths((candidate, candidateToBeat))
        }
    }
  }
}
