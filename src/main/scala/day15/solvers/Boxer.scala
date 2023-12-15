package day15.solvers

import day15.model.Lens
import utils.ProblemSolver

object Boxer extends ProblemSolver[String, Map[Int, List[Lens]]] {
  override def solve(input: String): Map[Int, List[Lens]] = {
    input.split(",").foldLeft(Map.empty[Int, List[Lens]]) { case (boxes, operation) =>
      if (operation.contains('=')) {
        val parts = operation.split("=", -1)
        val label = parts(0)
        val focalLength = parts(1).toInt
        val index = Hasher.solve(label)

        boxes.updatedWith(index) {
          case None => Some(List(Lens(label, focalLength)))
          case Some(existingBoxContent) =>
            if (existingBoxContent.exists(_.label == label))
              Some(existingBoxContent.map(lens => if (lens.label == label) lens.copy(focalLength = focalLength) else lens))
            else
              Some(existingBoxContent :+ Lens(label, focalLength))
        }
      } else {
        val label = operation.stripSuffix("-")
        val index = Hasher.solve(label)

        boxes.updatedWith(index) {
          case None => None
          case Some(existingBoxContent) => Some(existingBoxContent.filterNot(_.label == label))
        }
      }
    }.filterNot(_._2.isEmpty)
  }
}
