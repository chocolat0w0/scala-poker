trait PokerHandValidator {
  /**
    * 指定されたカードが役を満たすか判定する
    *
    * @param cards カード
    * @return 役を満たす場合:(役名, 役の中で一番強いカード)
    */
  def isValid(cards: Seq[Card]): Option[WinningHand]
}

sealed case class WinningHand(name: String, strength: Int, card: Card) extends Ordered[WinningHand] {
  override def compare(that: WinningHand): Int = strength - that.strength
}
class Straight(card: Card) extends WinningHand("Straight", 5, card)
class Flush(card: Card) extends WinningHand("Flush", 6, card)

object Straight extends PokerHandValidator {
  override def isValid(cards: Seq[Card]): Option[WinningHand] = {
    if (cards.zip(cards.tail).forall { case(x, y) => x.rank.strength  + 1 == y.rank.strength }) Option(new Straight(cards.last))
    else None
  }
}

object Flush extends PokerHandValidator {
  override def isValid(cards: Seq[Card]): Option[WinningHand] = {
    if (cards.forall(_.suit.equals(cards.head.suit))) Option(new Flush(cards.last))
    else None
  }
}
