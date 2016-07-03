case class Card(str: String)  extends Ordered[Card] {
  val suit:Suit = Suit.create(str.split("-")(0))
  val rank = Rank.create(str.split("-")(1))

  /**
    * カードの強さを比較する。強さの比較はrankが優先する。
    * @param that 比較対象のカード
    * @return 負の数:自分が強い/正の数:相手が強い
    */
  override def compare(that: Card): Int = {
    (rank.strength - that.rank.strength, suit.strength - that.suit.strength) match {
      case (r, _) if r < 0 => r
      case (r, s) if r == 0 => s
      case (r, _) => r
    }
  }
}

sealed abstract case class Suit(label: String, strength: Int)
object Suit {
  /**
    * 指定されたマーク記号に対応するスートを生成します。
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

sealed abstract case class Rank(label: String, strength: Int)
object Rank {
  /**
    * 指定された数字記号に対応するランクを生成します。
    * 異常な数字記号の場合は、一番弱いランクを返しておきます。
    * @param str 数字記号
    * @return ランククラス
    */
  def create(str: String): Rank = {
    str match {
      case "2" => Deuce
      case "3" => Trey
      case "4" => Four
      case "5" => Five
      case "6" => Six
      case "7" => Seven
      case "8" => Eight
      case "9" => Nine
      case "10" => Ten
      case "J" => Jack
      case "Q" => Queen
      case "K" => King
      case "A" => Ace
      case _ => Deuce
    }
  }

  object Deuce extends Rank("2", 2)
  object Trey extends Rank("3", 3)
  object Four extends Rank("4", 4)
  object Five extends Rank("5", 5)
  object Six extends Rank("6", 6)
  object Seven extends Rank("7", 7)
  object Eight extends Rank("8", 8)
  object Nine extends Rank("9", 9)
  object Ten extends Rank("10", 10)
  object Jack extends Rank("J", 11)
  object Queen extends Rank("Q", 12)
  object King extends Rank("K", 13)
  object Ace extends Rank("A", 14)
}
