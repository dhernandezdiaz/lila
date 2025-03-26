package lila.tree

import munit.FunSuite
import chess.variant.Standard
import chess.{ Check, Ply, Color }
import chess.format.{ Fen, Uci, UciCharPair, UciPath }
import chess.format.pgn.{ Glyph, Glyphs }
import chess.bitboard.Bitboard
import lila.tree.Node.*
import lila.tree.Branch

class RootTest extends FunSuite:

  val variant = Standard
  val root    = Root.default(variant)

  val move           = Uci("e2e4").get
  val pair           = UciCharPair(move)
  val path_to_branch = UciPath.fromId(pair)

  val ply = Ply(1)
  val branch = Branch(
    id = UciCharPair(move),
    ply = ply,
    move = chess.format.Uci.WithSan(move, chess.format.pgn.SanStr("e4")),
    fen = Fen.Full("some-fen"),
    check = Check.No,
    crazyData = None
  )

  test("create default Root") {
    assertEquals(root.ply, Ply.initial)
    assertEquals(root.fen, variant.initialFen)
    assert(root.children.isEmpty)
  }

  test("add and find child node") {
    val updated = root.addChild(branch)
    assertEquals(updated.children.first.map(_.id), Some(branch.id))
    assertEquals(updated.nodeAt(path_to_branch).map(_.ply), Some(ply))
  }

  test("set and retrieve comment") {
    val comment = Comment(Comment.Id.make, Comment.Text("Hello!"), Comment.Author.Lichess)
    // val updated     = root.addChild(branch)
    val withComment = root.setCommentAt(comment, UciPath.root).get
    assertEquals(withComment.comments.value.headOption.map(_.text.value), Some("Hello!"))
  }

  test("clear annotations recursively") {
    val withAnnotations = root.copy(
      comments = Comments(List(Comment(Comment.Id.make, Comment.Text("Hi"), Comment.Author.Lichess))),
      shapes = Shapes(List(Shape.Circle("green", chess.Square.E4)))
    )
    val cleared = withAnnotations.clearAnnotationsRecursively
    assertEquals(cleared.comments.value.isEmpty, true)
    assertEquals(cleared.shapes.value.isEmpty, true)
  }

  test("merge roots should combine glyphs and comments") {
    val comment = Comment(Comment.Id.make, Comment.Text("Merged"), Comment.Author.Lichess)

    val r1 = Root
      .default(variant)
      .copy(glyphs = Glyphs.fromList(List(Glyph.MoveAssessment.brilliant)))

    val r2 = Root
      .default(variant)
      .copy(comments = Comments(List(comment)))

    val merged = r1.merge(r2)

    assert(merged.glyphs.nonEmpty.isDefined)
    assertEquals(merged.comments.value.length, 1)
    assertEquals(merged.comments.value.headOption.map(_.text.value), Some("Merged"))
    assertEquals(merged.comments.value.head.by, Comment.Author.Lichess)
  }
