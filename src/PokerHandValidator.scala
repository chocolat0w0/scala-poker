trait PokerHandValidator {
  /**
    * 指定されたカードが役を満たすか判定する
    *
    * @param cards カード
    * @return 役を満たす場合:(役名, 役の中で一番強いカード)
    */
  def isValid(cards: Seq[Card]): Option[WinningHand]
}

object HighCard extends PokerHandValidator {
  override def isValid(cards: Seq[Card]): Option[WinningHand] = {
    val pair = cards.zip(cards.tail).filter {case (x, y) => x.rank.equals(y.rank)}
    (pair.length, Straight.isValid(cards), Flush.isValid(cards)) match {
      case (0, None, None) => Option(new HighCard(cards.last))
      case _ => None
    }
  }
}

object OnePair extends PokerHandValidator {
  override def isValid(cards: Seq[Card]): Option[WinningHand] = {
    val pair = cards.zip(cards.tail).filter {case (x, y) => x.rank.equals(y.rank)}
    if (pair.length == 1) Option(new OnePair(pair.head._2))
    else None
  }
}

object TwoPair extends PokerHandValidator {
  override def isValid(cards: Seq[Card]): Option[WinningHand] = {
    val pair = cards.zip(cards.tail).filter {case (x, y) => x.rank.equals(y.rank)}
    if (pair.length == 2 && !pair.head._1.rank.equals(pair.last._2.rank)) Option(new TwoPair(pair.last._2))
    else None
  }
}

object ThreeOfAKind extends PokerHandValidator {
  override def isValid(cards: Seq[Card]): Option[WinningHand] = {
    val pair = cards.zip(cards.tail).filter {case (x, y) => x.rank.equals(y.rank)}
    if (pair.length == 2 && pair.head._1.rank.equals(pair.last._2.rank)) Option(new ThreeOfAKind(pair.last._2))
    else None
  }
}

object FourOfAKind extends PokerHandValidator {
  override def isValid(cards: Seq[Card]): Option[WinningHand] = {
    val pair = cards.zip(cards.tail).filter {case (x, y) => x.rank.equals(y.rank)}
    if (pair.length == 3 && pair.head._1.rank.equals(pair.last._2.rank)) Option(new FourOfAKind(pair.last._2))
    else None
  }
}

object FullHouse extends PokerHandValidator {
  override def isValid(cards: Seq[Card]): Option[WinningHand] = {
    val pair = cards.zip(cards.tail).filter {case (x, y) => x.rank.equals(y.rank)}
    if (pair.length == 3 && !pair.head._1.rank.equals(pair.last._2.rank)) Option(new FullHouse(pair.last._2))
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

object RoyalStraightFlush extends PokerHandValidator {
  override def isValid(cards: Seq[Card]): Option[WinningHand] = {
    (Straight.isValid(cards), Flush.isValid(cards)) match {
      case (Some(s), Some(_)) if s.card.rank.equals(Rank.Ace) => Option(new RoyalStraightFlush(s.card))
      case _ => None
    }
  }
}

sealed case class WinningHand(name: String, strength: Int, card: Card) extends Ordered[WinningHand] {
  override def compare(that: WinningHand): Int = strength - that.strength
}
class HighCard(card: Card) extends WinningHand("High Card", 1, card)
class OnePair(card: Card) extends WinningHand("One Pair", 2, card)
class TwoPair(card: Card) extends WinningHand("Two Pair", 3, card)
class ThreeOfAKind(card: Card) extends WinningHand("Three Of A Kind", 4, card)
class Straight(card: Card) extends WinningHand("Straight", 5, card)
class Flush(card: Card) extends WinningHand("Flush", 6, card)
class FullHouse(card: Card) extends WinningHand("Full House", 7, card)
class FourOfAKind(card: Card) extends WinningHand("Four Of A Kind", 8, card)
class StraightFlush(card: Card) extends WinningHand("Straight Flush", 9, card)
class RoyalStraightFlush(card: Card) extends WinningHand("Royal Straight Flush", 10, card)

