import Rank._

trait PokerWinningHand {
  /**
    * 指定されたカードが役を満たすか判定する
    *
    * @param cards カード
    * @return 役を満たす場合:(役名, 役の中で一番強いカード)
    */
  def isMatch(cards: Array[Card]): Option[(String, Card)]
}

object Straight extends PokerWinningHand {
  override def isMatch(cards: Array[Card]): Option[(String, Card)] = {
    if (cards.zip(cards.tail).forall { case(x, y) => x.rank.strength  + 1 == y.rank.strength }) Option("Straight", cards.last)
    else None
  }
}

