trait PokerHandValidator {
  /**
    * 指定されたカードが役を満たすか判定する
    *
    * @param cards カード
    * @return 役を満たす場合:(役名, 役の中で一番強いカード)
    */
  def isValid(cards: Seq[Card]): Option[WinningHand]
}

object OnePair extends PokerHandValidator {
  override def isValid(cards: Seq[Card]): Option[WinningHand] = {
    val pair = cards.zip(cards.tail).filter {case (x, y) => x.rank.equals(y.rank)}
    if (pair.length == 1) Option(new OnePair(pair.head._2))
    else None
  }
}

object Straight extends PokerHandValidator {
  override def isValid(cards: Seq[Card]): Option[WinningHand] = {
    if (cards.zip(cards.tail).forall { case(x, y) => x.rank.strength  + 1 == y.rank.strength }) Option(new Straight(cards.last))
    else None
  }
}

object Flush extends PokerHandValidator {
  override def isValid(cards: Seq[Card]): Option[WinningHand] = {
    if (cards.zip(cards.tail).forall {case(x, y) => x.suit.equals(y.suit)}) Option(new Flush(cards.last))
    else None
  }
}

object StraightFlush extends PokerHandValidator {
  override def isValid(cards: Seq[Card]): Option[WinningHand] = {
    (Straight.isValid(cards), Flush.isValid(cards)) match {
      case (Some(_), Some(_)) => Option(new StraightFlush(cards.last))
      case _ => None
    }
  }
}

sealed case class WinningHand(name: String, strength: Int, card: Card) extends Ordered[WinningHand] {
  override def compare(that: WinningHand): Int = strength - that.strength
}
class OnePair(card: Card) extends WinningHand("One Pair", 2, card)
class Straight(card: Card) extends WinningHand("Straight", 5, card)
class Flush(card: Card) extends WinningHand("Flush", 6, card)
class StraightFlush(card: Card) extends WinningHand("Straight Flush", 9, card)

