package day07.model

sealed trait JokerAwareCard {
  val strength: Int
  def compare(other: JokerAwareCard): Int = Integer.compare(this.strength, other.strength)
}

object JokerAwareCard {
  case object C_A extends JokerAwareCard { override val strength: Int = 13 }
  case object C_K extends JokerAwareCard { override val strength: Int = 12 }
  case object C_Q extends JokerAwareCard { override val strength: Int = 11 }
  case object C_T extends JokerAwareCard { override val strength: Int = 10 }
  case object C_9 extends JokerAwareCard { override val strength: Int = 9 }
  case object C_8 extends JokerAwareCard { override val strength: Int = 8 }
  case object C_7 extends JokerAwareCard { override val strength: Int = 7 }
  case object C_6 extends JokerAwareCard { override val strength: Int = 6 }
  case object C_5 extends JokerAwareCard { override val strength: Int = 5 }
  case object C_4 extends JokerAwareCard { override val strength: Int = 4 }
  case object C_3 extends JokerAwareCard { override val strength: Int = 3 }
  case object C_2 extends JokerAwareCard { override val strength: Int = 2 }
  case object C_J extends JokerAwareCard { override val strength: Int = 1 }
}