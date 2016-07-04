case class Player(cardsStr: String) {
  val hands: Seq[Card] = cardsStr.split(" ").map(Card(_)).sorted

  /**
    * プレイヤーが所持する手札で成立する、最も強い役を返す
    * @return (役名、役を構成するカードで最も強いもの)
    */
  def judge: (String, String) = {
    val validator = Seq(Straight, Flush)
//    TODO: 強い方から判定して、成立したら以降の判定はしないようにできないかな？
    val winningHand = validator.flatMap(_.isMatch(hands)).last
    (winningHand.name, winningHand.card.str)
  }
}
