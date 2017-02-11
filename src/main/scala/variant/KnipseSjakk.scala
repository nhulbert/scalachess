package chess
package variant

case object KnipseSjakk extends Variant(
  id = 11,
  key = "flickChess",
  name = "Knipse-Sjakk",
  shortName = "Std",
  title = "Flick your pieces in order to eliminate your opponent's king",
  standardInitialPosition = true)
