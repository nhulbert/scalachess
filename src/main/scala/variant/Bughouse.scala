package chess
package variant

import format.Uci

import scalaz.Validation.FlatMap._

case object Bughouse extends Variant(
  id = 11,
  key = "bughouse",
  name = "Bughouse",
  shortName = "bug",
  title = "A four player, two board variant where each piece you capture is added to your partner's collection, which they may take a piece from to place on their board as their turn.",
  standardInitialPosition = true
) {

  def pieces = Standard.pieces

  override val initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/ w KQkq - 0 1"

  override def valid(board: Board, strict: Boolean) = {
    val pieces = board.pieces.values
    (Color.all forall validSide(board, false)_) &&
      (!strict || (pieces.count(_ is Pawn) <= 16 && pieces.size <= 32))
  }

  private def canDropPawnOn(pos: Pos) = (pos.y != 1 && pos.y != 8)

  override def drop(situation: Situation, role: Role, pos: Pos): Valid[Drop] = for {
    d1 <- situation.board.crazyData toValid "Board has no crazyhouse data"
    _ <- d1.validIf(role != Pawn || canDropPawnOn(pos), s"Can't drop $role on $pos")
    piece = Piece(situation.color, role)
    d2 <- d1.drop(piece) toValid s"No $piece to drop"
    board1 <- situation.board.place(piece, pos) toValid s"Can't drop $role on $pos, it's occupied"
    _ <- board1.validIf(!board1.check(situation.color), s"Dropping $role on $pos doesn't uncheck the king")
  } yield Drop(
    piece = piece,
    pos = pos,
    before = situation.board,
    after = board1 withCrazyData d2
  )

  override def updatePositionHashes(board: Board, move: Move, hash: chess.PositionHash) =
    updateHashes(hash, board, !move.piece.color)

  override def updatePositionHashes(board: Board, drop: Drop, hash: chess.PositionHash) =
    updateHashes(hash, board, !drop.piece.color)

  // don't clear the hash on pawn move or promotion, to preserve threefold repetition
  // but disable 50-moves by truncating the hash at 99
  private def updateHashes(hash: PositionHash, board: Board, color: Color) = {
    val newHash = Hash(Situation(board, color)) ++ hash
    if (newHash.size > 99 * Hash.size) newHash take 99 * Hash.size else newHash
  }

  override def finalizeBoard(board: Board, uci: Uci, capture: Option[Piece]): Board = uci match {
    case Uci.Move(orig, dest, promOption) =>
      board.crazyData.fold(board) { data =>
        val d1 = capture.fold(data) { _ => data.removePromoted(dest) }
        val d2 = promOption.fold(d1.move(orig, dest)) { _ => d1 promote dest }
        board withCrazyData d2
      }
    case _ => board
  }

  def addToPocket(data: Crazyhouse.Data, piece: Piece): Crazyhouse.Data =
    data.storePiece(piece)

  private def canDropStuff(situation: Situation) = // Different than Crazyhouse: any piece has the possibility of being
    situation.board.crazyData.fold(false) { (data: Crazyhouse.Data) => // dropped on any turn, TODO: add detection of
      possibleDrops(situation).fold(true) { squares => // checkmate situation that requires evaluation of both boards
        squares.nonEmpty // (without this clock will need to time out before game is forced to end)
      }
    }

  override def staleMate(situation: Situation) =
    super.staleMate(situation) && !canDropStuff(situation)

  override def checkmate(situation: Situation) =
    super.checkmate(situation) && !canDropStuff(situation)

  def possibleDrops(situation: Situation): Option[List[Pos]] =
    if (!situation.check) None
    else situation.kingPos.map { blockades(situation, _) }

  private def blockades(situation: Situation, kingPos: Pos): List[Pos] = {
    def attacker(piece: Piece) = piece.role.projection && piece.color != situation.color
    def forward(p: Pos, dir: Direction, squares: List[Pos]): List[Pos] = dir(p) match {
      case None => Nil
      case Some(next) if situation.board(next).exists(attacker) => next :: squares
      case Some(next) if situation.board(next).isDefined => Nil
      case Some(next) => forward(next, dir, next :: squares)
    }
    Queen.dirs flatMap { forward(kingPos, _, Nil) } filter { square =>
      situation.board.place(Piece(situation.color, Knight), square) exists { defended =>
        !defended.check(situation.color)
      }
    }
  }

  case class PieceAdd(piece: Piece, halfMove: Int)
}
