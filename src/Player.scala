case class Player(cardsStr: String) {
  val hands: Array[Card] = cardsStr.split(" ").map(Card(_))
  hands.foreach(h => println(h))
  println("-----ordered-----")
  hands.sorted.foreach(h => println(h))
}
