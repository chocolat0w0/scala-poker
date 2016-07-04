trait PokerHandValidator {
  /**
    * 指定されたカードが役を満たすか判定する
    *
    * @param cards カード
    * @return 役を満たす場合:(役名, 役の中で一番強いカード)
    */
  def isMatch(cards: Seq[Card]): Option[WinningHand]
}

abstract sealed case class WinningHand(name: String, strength: Int, card: Card)
class Straight(card: Card) extends WinningHand("Straight", 5, card)
class Flush(card: Card) extends WinningHand("Flush", 6, card)

object Straight extends PokerHandValidator {
  override def isMatch(cards: Seq[Card]): Option[WinningHand] = {
    if (cards.zip(cards.tail).forall { case(x, y) => x.rank.strength  + 1 == y.rank.strength }) Option(new Straight(cards.last))
    else None
  }
}

object Flush extends PokerHandValidator {
  override def isMatch(cards: Seq[Card]): Option[WinningHand] = {
    if (cards.forall(_.suit.equals(cards.head.suit))) Option(new Flush(cards.last))
    else None
  }
}
