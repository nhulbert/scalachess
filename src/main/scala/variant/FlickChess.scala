package chess
package variant

case object FlickChess extends Variant(
  id = 11,
  key = "flickchess",
  name = "Flick-Chess",
  shortName = "Flick",
  title = "Flick your pieces in order to eliminate your opponent's king",
  standardInitialPosition = true)
