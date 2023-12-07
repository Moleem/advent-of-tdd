package day07.model

sealed trait Card {
  val strength: Int
  def compare(other: Card): Int = Integer.compare(this.strength, other.strength)
}

case object C_A extends Card {override val strength: Int = 13 }
case object C_K extends Card { override val strength: Int = 12 }
case object C_Q extends Card { override val strength: Int = 11 }
case object C_J extends Card { override val strength: Int = 10 }
case object C_T extends Card { override val strength: Int = 9 }
case object C_9 extends Card { override val strength: Int = 8 }
case object C_8 extends Card { override val strength: Int = 7 }
case object C_7 extends Card { override val strength: Int = 6 }
case object C_6 extends Card { override val strength: Int = 5 }
case object C_5 extends Card { override val strength: Int = 4 }
case object C_4 extends Card { override val strength: Int = 3 }
case object C_3 extends Card { override val strength: Int = 2 }
case object C_2 extends Card { override val strength: Int = 1 }
