package u02

import BTrees.Tree.*
<<<<<<< HEAD:test/u02/BTreesTest.scala
import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertTrue}
import org.junit.jupiter.api.Test


=======
import junit.framework.TestCase.assertTrue
import org.junit.Assert.{assertEquals, assertFalse}
import org.junit.Test
>>>>>>> b6e3de5eda6a909e6e63c3737cf1b9f8349b17a8:src/test/scala/u02/BTreesTest.scala

class BTreesTest:

  val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(1))

  @Test
  def testCount() =
    assertEquals(2, count(tree, 1))
    assertEquals(1, count(tree, 2))
    assertEquals(0, count(tree, 3))

  @Test
  def testSize() =
    assertEquals(3, size(tree))

  @Test
  def testFind() =
    assertTrue(find(tree, 2))
    assertFalse(find(tree, 4))
