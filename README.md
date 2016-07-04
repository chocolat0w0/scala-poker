# ルール
手札は5枚。

一番強い役と、それを構成する一番強いカードで戦う。

# 役
下に行くほど強い。

役名(和名)                                             | 条件                         | 例
-------------------------------------------------------|------------------------------|----------
High Card(ハイ・カード)                                | 何も成立しない               | C-A D-2 H-3 S-4 S-5
One Pair(ワンペア)                                     | 2枚同じ数字が1セット         | C-A D-A H-3 S-4 S-5
Two Pair(ツーペア)                                     | 2枚同じ数字が2セット         | C-A D-A H-3 S-3 S-5
Three Of A Kind(スリーカード)                          | 3枚同じ数字                  | C-A D-A H-A S-4 S-5
Straight(ストレート)                                   | 数字が階段で5枚              | C-2 D-3 H-4 S-5 S-6
Flush(フラッシュ)                                      | 5枚同じマーク                | C-A C-2 C-3 C-4 C-5
Full House(フルハウス)                                 | 2枚同じ数字＋3枚同じ数字     | C-A D-A H-3 S-3 S-3
Four Of A Kind(フォーカード)                           | 4枚同じ数字                  | C-A D-A H-A S-A S-5
Straight Flush(ストレート・フラッシュ)                 | 同じマークで階段5枚          | C-2 C-3 C-4 C-5 C-6
Royal Straight Flush(ロイヤル・ストレート・フラッシュ) | Aが入って同じマークで階段5枚 | C-10 C-J C-Q C-K C-A

# カード
## スート
カードのマーク。

下に行くほど強い。

意味	| 入出力
--------|--------
Club	| C
Diamond	| D
Heart	| H
Spade	| S

## 数字
下に行くほど強い。

意味	| 入出力
--------|--------
2 - 10	| 2 - 10
11	    | J
12	    | Q
13	    | K
14	    | A

## 一番強いカード
役が同じ場合、役を構成するカードのうち、一番強いカードの比較になる。
数字＞スートの順に強い判定をする。

C-9 vs S-2
→ 数字の比較で9の勝ち

S-9 vs C-9
→ 数字が同じなのでスートの比較になり、スペードの勝ち



