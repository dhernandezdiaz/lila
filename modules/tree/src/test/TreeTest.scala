package lila.tree

import munit.FunSuite
import chess.variant.Standard
import chess.{ Check, Ply, Color }
import chess.format.{ Fen, Uci, UciCharPair, UciPath, FullFen }
import chess.format.pgn.{ Glyph, Glyphs, SanStr }
import chess.bitboard.Bitboard
import lila.tree.Node.*
import lila.tree.Branch
import play.api.libs.json.*

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

  test("Root nodes should never be hidden") {
    val r = Root.default(Standard)
    assertEquals(r.hidden, false)
  }

class BranchTest extends FunSuite:

  val move = Uci("e2e4").get
  val pair = UciCharPair(move)
  val ply  = Ply(1)
  val fen  = FullFen("some-fen")
  // val fen     = Fen.Full("some-fen")
  val withSan = chess.format.Uci.WithSan(move, SanStr("e4"))

  def makeBranch(hidden: Boolean = false): Branch =
    Branch(
      id = pair,
      ply = ply,
      move = withSan,
      fen = fen,
      check = Check.No,
      crazyData = None,
      hidden = hidden
    )

  test("branch instantiation and defaults") {
    val b = makeBranch()
    assertEquals(b.ply, ply)
    assertEquals(b.fen, fen)
    assertEquals(b.move.uci, move)
    assertEquals(b.children.isEmpty, true)
    assertEquals(b.hidden, false)
  }

  test("branch can be marked as hidden") {
    val b = makeBranch(hidden = true)
    assertEquals(b.hidden, true)
  }

  test("add and retrieve children") {
    val child  = makeBranch().copy(ply = Ply(2))
    val parent = makeBranch().addChild(child)
    assertEquals(parent.children.first.map(_.ply), Some(Ply(2)))
  }

  test("prepend and drop first child") {
    val a       = makeBranch().copy(ply = Ply(2))
    val b       = makeBranch().copy(ply = Ply(3))
    val parent  = makeBranch().prependChild(a).addChild(b)
    val dropped = parent.dropFirstChild
    assertEquals(dropped.children.first.map(_.ply), Some(Ply(3)))
  }

  test("set and delete comment") {
    val author  = Comment.Author.Lichess
    val comment = Comment(Comment.Id.make, Comment.Text("Comment"), author)
    val b       = makeBranch().setComment(comment)
    assertEquals(b.comments.value.headOption.map(_.text.value), Some("Comment"))
    // assertEquals(b.comments.value.head.text.value, "Comment")

    val deleted = b.deleteComment(comment.id)
    assertEquals(deleted.comments.value.exists(_.id == comment.id), false)
  }

  test("toggle glyphs") {
    val b = makeBranch().toggleGlyph(Glyph.MoveAssessment.brilliant)
    assert(!b.glyphs.isEmpty)
  }

  test("clear annotations") {
    val b = makeBranch().copy(
      comments = Comments(List(Comment(Comment.Id.make, Comment.Text("X"), Comment.Author.Lichess))),
      shapes = Shapes(List(Shape.Circle("green", chess.Square.E4))),
      glyphs = Glyphs.fromList(List(Glyph.MoveAssessment.brilliant))
    )
    val cleared = b.clearAnnotations
    assert(cleared.comments.value.isEmpty)
    assert(cleared.shapes.value.isEmpty)
    assertEquals(cleared.glyphs.nonEmpty, None)
  }

  test("merge branches should combine children and annotations") {
    val child = makeBranch().copy(ply = Ply(2))
    val a = makeBranch().copy(
      shapes = Shapes(List(Shape.Circle("red", chess.Square.E2))),
      children = Branches(List(child))
    )
    val b = makeBranch().copy(
      comments = Comments(List(Comment(Comment.Id.make, Comment.Text("Yo"), Comment.Author.Lichess)))
    )
    val merged = a.merge(b)
    assertEquals(merged.comments.value.length, 1)
    assertEquals(merged.shapes.value.length, 1)
    assertEquals(merged.children.countRecursive, 1)
  }

  test("updateMainlineLast applies update to deepest child") {
    val leaf = makeBranch().copy(ply = Ply(3))
    val mid  = makeBranch().copy(ply = Ply(2), children = Branches(List(leaf)))
    val root = makeBranch().copy(ply = Ply(1), children = Branches(List(mid)))

    val updated = root.updateMainlineLast(_.setComp)
    assertEquals(updated.children.first.get.children.first.get.comp, true)
  }

  test("json serialization includes hidden when true") {
    val branch = makeBranch().copy(hidden = true)
    val json   = Json.toJson(branch)(Node.defaultNodeJsonWriter)
    assert(json.as[JsObject].value("hidden") == JsBoolean(true))
  }
