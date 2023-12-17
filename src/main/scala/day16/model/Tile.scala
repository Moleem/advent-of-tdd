package day16.model

sealed trait Tile {
  def handleLight(coordinates: (Int, Int), fromDirection: Direction): List[((Int, Int), Direction)]
}

case object EmptyTile extends Tile {
  override def handleLight(coordinates: (Int, Int), fromDirection: Direction): List[((Int, Int), Direction)] =
    fromDirection match {
      case Right => List(
        ((coordinates._1, coordinates._2+1), Right)
      )
      case Left => List(
        ((coordinates._1, coordinates._2-1), Left)
      )
      case Up => List(
        ((coordinates._1-1, coordinates._2), Up)
      )
      case Down => List(
        ((coordinates._1+1, coordinates._2), Down)
      )
    }
}

case object HorizontalSplitter extends Tile {
  override def handleLight(coordinates: (Int, Int), fromDirection: Direction): List[((Int, Int), Direction)] =
    fromDirection match {
      case Right => List(
        ((coordinates._1, coordinates._2+1), Right)
      )
      case Left => List(
        ((coordinates._1, coordinates._2-1), Left)
      )
      case Up | Down => List(
        ((coordinates._1, coordinates._2+1), Right),
        ((coordinates._1, coordinates._2-1), Left)
      )
    }
}

case object VerticalSplitter extends Tile {
  override def handleLight(coordinates: (Int, Int), fromDirection: Direction): List[((Int, Int), Direction)] =
    fromDirection match {
      case Right | Left => List(
        ((coordinates._1+1, coordinates._2), Down),
        ((coordinates._1-1, coordinates._2), Up)
      )
      case Up => List(
        ((coordinates._1 - 1, coordinates._2), Up)
      )
      case Down => List(
        ((coordinates._1 + 1, coordinates._2), Down)
      )
    }
}

case object DownLeaningMirror extends Tile {
  override def handleLight(coordinates: (Int, Int), fromDirection: Direction): List[((Int, Int), Direction)] =
    fromDirection match {
      case Right => List(
        ((coordinates._1+1, coordinates._2), Down)
      )
      case Left => List(
        ((coordinates._1-1, coordinates._2), Up)
      )
      case Up => List(
        ((coordinates._1, coordinates._2-1), Left)
      )
      case Down => List(
        ((coordinates._1, coordinates._2+1), Right)
      )
    }
}

case object UpLeaningMirror extends Tile {
  override def handleLight(coordinates: (Int, Int), fromDirection: Direction): List[((Int, Int), Direction)] =
    fromDirection match {
      case Right => List(
        ((coordinates._1 - 1, coordinates._2), Up)
      )
      case Left => List(
        ((coordinates._1+1, coordinates._2), Down)
      )
      case Up => List(
        ((coordinates._1, coordinates._2 + 1), Right)
      )
      case Down => List(
        ((coordinates._1, coordinates._2-1), Left)
      )
    }
}

