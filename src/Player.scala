case class Player(cardsStr: String) {
  val hands:Array[Card] = cardsStr.split(" ").map(Card(_))

}
