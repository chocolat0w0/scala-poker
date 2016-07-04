object Main {
  def main(args: Array[String]) {
//    test("S-4 S-Q H-2 H-J H-3", "High Card", "S-Q")
//
    test("S-4 S-Q H-Q H-J H-3", "One Pair", "S-Q")
//
    test("S-4 S-J H-J H-Q H-4", "Two Pair", "S-J")
//
    test("S-4 S-Q H-4 H-J H-4", "Three Of A Kind", "S-4")

    test("H-2 D-3 S-4 C-5 S-6", "Straight", "S-6")

    test("H-4 H-Q H-2 H-J H-3", "Flush", "H-Q")
//
//    test("H-4 H-Q S-Q D-Q H-4", "Full House", "S-Q")
//
//    test("H-4 H-Q S-Q D-Q C-Q", "Four Of A Kind", "S-Q")
//
    test("S-5 S-3 S-6 S-2 S-4", "Straight Flush", "S-6")
//
//    test("S-J S-Q S-A S-K S-10", "Royal Straight Flush", "S-A")
  }

  def test(input: String, winningHand: String, winningCard: String): Unit = {
    val actual = Player(input).judge
    assert(winningHand == actual._1 && winningCard == actual._2, "input: %s, expected: %s, actual: %s".format(input, (winningHand, winningCard), (actual._1, actual._2)))
  }
}
