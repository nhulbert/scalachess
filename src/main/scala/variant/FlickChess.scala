package chess
package variant

case object FlickChess extends Variant(
  id = 11,
  key = "flickchess",
  name = "Flick-Chess",
  shortName = "Flick",
  title = "Flick your pieces in order to eliminate your opponent's king",
  standardInitialPosition = false) {

  override def allowsCastling = false

  override val castles = Castles.none

  override val initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1"

  override def insufficientWinningMaterial(board: Board) = false
  override def insufficientWinningMaterial(board: Board, color: Color) = false

  // In Flick-Chess, the king can't be put into check so we always return false
  override def kingThreatened(board: Board, color: Color, to: Pos, filter: Piece => Boolean = _ => true) = false

  override def valid(board: Board, strict: Boolean) =
    board.kingPos.size >= 1

  // In Flick-Chess, there is no checkmate condition, and the winner is the player whose opponent's king is captured
  override def winner(situation: Situation): Option[Color] = if (specialEnd(situation)) Some(situation.color) else None

  /** Flick-Chess has a special end where a king has been captured */
  override def specialEnd(situation: Situation) = situation.board.kingPos.size != 2

  // Stalemate is not possible in Flick-Chess
  override def staleMate(situation: Situation) = false
}
