package patmat

import org.scalatest.{FunSuite, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite with Matchers {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)

    val t2 = Fork(
      left = Fork(
        Leaf('a',2),
        Leaf('b',3), List('a','b'), 5),
      right = Leaf('d',4), chars = List('a','b','d'), weight = 9
    )

    val l2 = List('a','a','b','b','b','d','d','d','d')
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until function") {
    val list = List(
      Leaf('b',3),
      Leaf('a',2),
      Leaf('d',4)
    )
    new TestTrees {
      until(singleton, combine)(list) equals t2
    }
  }

//  test("createCodeTree function") {
//    new  TestTrees {
//      val orderedLeafList1_expected = List(Leaf('a',2), Leaf('b',3), Leaf('d',4))
//      val orderedLeafList1_actual = makeOrderedLeafList(times(l2))
//
//      val combined1_expected  = List(
//        Leaf('d',4),
//        Fork(Leaf('a',2), Leaf('b',3), List('a', 'b'), 5)
//      )
//      val combined1_actual = combine(orderedLeafList1_actual)
//
//      val combined2_expected = List(
//        Fork(
//          Leaf('d',4),
//          Fork(Leaf('a',2), Leaf('b',3), List('a', 'b'), 5),
//          List('d', 'a', 'b'),
//          9
//        )
//      )
//      val combined2_actual = combine(combined1_expected)
//
//
//      createCodeTree(l2) shouldEqual
//    }
//  }

  test("decode small tree") {
    new TestTrees {
      decode(t2, List(
        0,0, // a
        0,0, // a
        0,1, // b
        0,1, // b
        0,1, // b
        1, // d
        1, // d
        1, // d
        1  // d
      )) shouldEqual l2

      decode(t2, List(1,1)) shouldEqual List('d','d')

      decode(t2, List(1)) shouldEqual List('d')
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val encoded: List[Bit] = encode(t1)("ab".toList)
      val decoded: List[Char] = decode(t1, encoded)
      println(decoded)

      assert( decoded === "ab".toList)
    }
  }

  test("decode and quickEncode a very short text should be identity") {
    new TestTrees {
      val encoded: List[Bit] = quickEncode(t1)("ab".toList)
      val decoded: List[Char] = decode(t1, encoded)
      println(decoded)
      assert( decoded === "ab".toList)

      quickEncode(frenchCode)(decodedSecret) shouldEqual secret
    }
  }
}

