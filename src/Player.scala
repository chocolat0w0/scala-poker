case class Player(cardsStr: String) {
  val hands: Seq[Card] = cardsStr.split(" ").map(Card(_)).sorted

  /**
    * プレイヤーが所持する手札で成立する、最も強い役を返す
    * @return (役名、役を構成するカードで最も強いもの)
    */
  def judge: (String, String) = {
    val validator = Seq(OnePair, TwoPair, ThreeOfAKind, Straight, Flush, StraightFlush)
    val winningHand = validator.flatMap(_.isValid(hands)).max
    (winningHand.name, winningHand.card.str)
  }
}
