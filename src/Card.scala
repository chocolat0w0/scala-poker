case class Card(str: String) {
  val suit:Suit = Suit.create(str.split("-")(0))
  val rank = "数字"
}

sealed abstract case class Suit(label: String, strength: Int)
object Suit {
  /**
    * 指定されたマーク記号に対応するスートクラスを生成します。
    * 異常なマーク記号の場合は、一番弱いスートを返しておきます。
    * @param str マーク記号
    * @return スートクラス
    */
  def create(str: String): Suit = {
    str match {
      case "C" => Club
      case "D" => Diamond
      case "H" => Heart
      case "S" => Spade
      case _ => Club
    }
  }

  object Club extends Suit("C", 1)
  object Diamond	extends Suit("D", 2)
  object Heart	extends Suit("H", 3)
  object Spade	extends Suit("S", 4)
}

