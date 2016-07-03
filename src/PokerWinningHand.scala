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

object StraightFlush extends PokerWinningHand {
  override def isMatch(cards: Array[Card]): Option[(String, Card)] = {
    cards.map(_.rank) match {
      case Array(Ten, Jack, Queen, King, Ace) => Option(("Strait Flush", cards.last))
      case _ => None
    }
  }
}
