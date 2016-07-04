case class Player(cardsStr: String) {
  val hands: Seq[Card] = cardsStr.split(" ").map(Card(_)).sorted

  def judge: (String, String) = {
    val winningHand = Straight.isMatch(hands).get
    (winningHand.name, winningHand.card.str)
  }
}
