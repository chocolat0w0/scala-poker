case class Player(cardsStr: String) {
  val hands: Array[Card] = cardsStr.split(" ").map(Card(_)).sorted

  def judge: Unit = {
    println( StraightFlush.isMatch(hands))
  }
}
