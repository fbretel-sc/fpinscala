package fpinscala.datastructures

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

/**
 * @author martin.lehmann@gmx.de
 */
class Exercise3Tests {

//  @Test
//  def testExercise_3_01() {		// match
//	  assertEquals(3, List.resultOfMatchExpression)
//  }

  // ------------------ List ------------------------------------------------------------------------------------------------

  @Test 
  def testExercise_3_02() {		// tail
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List(2,3,4,5), List.tail(l1))

	  val l2 = List()
	  try {
		  val t = List.tail(l2)
		  fail("tail of empty list - test failed")
	  }
	  catch {
	  	case ex: RuntimeException => // expected
	  	case _: Throwable => fail("tail of empty list - test failed")
	  }

	  val l3 = List()
	  try {
		  val t = List.tail(l3)
		  fail("tail of empty list - test failed")
	  }
	  catch {
	  	case ex: RuntimeException => // expected
	  	case _: Throwable => fail("tail of empty list - test failed")
	  }
  }

  @Test 
  def testExercise_3_03() {		// setHead
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List(99, 2,3,4,5), List.setHead(l1, 99))

	  val l2 = List()
	  try {
		  val t = List.tail(l2)
		  fail("setHead of empty list - test failed")
	  }
	  catch {
	  	case ex: RuntimeException => // expected
	  	case _: Throwable => fail("setHead of empty list - test failed")
	  }
	  
	  val l3 = Nil
	  try {
		  val t = List.tail(l3)
		  fail("setHead of empty list - test failed")
	  }
	  catch {
	  	case ex: RuntimeException => // expected
	  	case _: Throwable => fail("setHead of empty list - test failed")
	  }
  }

  @Test 
  def testExercise_3_04() {		// drop
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List(1,2,3,4,5), List.drop(l1, 0))

  	  val l2 = List(1,2,3,4,5)
	  assertEquals(List(2,3,4,5), List.drop(l2, 1))

  	  val l3 = List(1,2,3,4,5)
	  assertEquals(Nil, List.drop(l3, 5))

  	  val l4 = List(1,2,3,4,5)
	  assertEquals(Nil, List.drop(l4, 6))
	  
  	  val l5 = Nil
	  assertEquals(Nil, List.drop(l5, 99))
  }

  @Test 
  def testExercise_3_05() {		// dropWhile
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List(4,5), List.dropWhile(l1, ((x:Int) => x<4)))

  	  val l2 = List(1,2,3,4,5)
	  assertEquals(Nil, List.dropWhile(l2, ((x:Int) => x<6)))

  	  val l3 = List(1,2,3,4,5)
	  assertEquals(List(1,2,3,4,5), List.dropWhile(l3, ((x:Int) => x<1)))

  	  val l4 = Nil
	  assertEquals(Nil, List.dropWhile(l4, ((x:Int) => x<99)))
  }

  @Test 
  def testExercise_3_06() {		// init
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List(1,2,3,4), List.init(l1))

	  val l2 = List(1)
	  assertEquals(Nil, List.init(l2))

	  val l3 = Nil
	  try {
		  val t = List.init(l3)
		  fail("init of empty list - test failed")
	  }
	  catch {
	  	case ex: RuntimeException => // expected
	  	case _: Throwable => fail("init of empty list - test failed")
	  }
  }

  @Test 
  def testExercise_3_07() {	// just a question, nothing to implement
	  // nothing to test here
  }

  @Test 
  def testExercise_3_08() {	// returns the original list, right?
	  val l1 = List(1,2,3,4,5)
	  assertEquals(l1, List.foldRight(l1, Nil: List[Int])(Cons(_,_)))
  }

  @Test 
  def testExercise_3_09() {	// length via foldRight
	  val l1 = List(1,2,3,4,5)
	  assertEquals(5, List.length(l1))
	  
	  val l2 = List(1)
	  assertEquals(1, List.length(l2))
	  
	  val l3 = Nil
	  assertEquals(0, List.length(l3))
  }

  @Test 
  def testExercise_3_10() {	// foldLeft
	  val l1 = List(1,2,3,4,5)
	  
	  val l_fr_plus = List.foldRight(l1, 0)((x,y) => x+y)
	  val l_fl_plus = List.foldLeft (l1, 0)((x,y) => x+y)
	  assertEquals(l_fr_plus, l_fl_plus)			// plus is commutative
	  
	  val l_fr_minus = List.foldRight(l1, 0)((x,y) => x-y)
	  val l_fl_minus = List.foldLeft (l1, 0)((x,y) => x-y)
	  assertNotEquals(l_fr_minus, l_fl_minus)		// minus is not
	  
	  val l_fr_mult = List.foldRight(l1, 0)((x,y) => x+y)
	  val l_fl_mult = List.foldLeft (l1, 0)((x,y) => x+y)
	  assertEquals(l_fr_mult, l_fl_mult)			// multiply is commutative
	  
	  val l_fr_div = List.foldRight(l1, 0)((x,y) => x-y)
	  val l_fl_div = List.foldLeft (l1, 0)((x,y) => x-y)
	  assertNotEquals(l_fr_div, l_fl_div)			// division is not
  }

  @Test 
  def testExercise_3_11() {		// sum, product and length via foldLeft
  	  val l1 = List(1,2,3,4,5)  
  	  val s = List.sum2(l1)
  	  assertEquals(15, s)
  	  
  	  val l2 = List(1.0,2.0,3.0,4.0,5.0)
  	  val p = List.product2(l2)
  	  assertEquals(120.0, p, 0.00001)
  	  
//  	  val l3 = List(1,2,3,4,5)
//  	  val l = List.length2(l3)
//  	  assertEquals(5, l)
  }

  @Test 
  def testExercise_3_12() {		// reverse
	  val l1 = List(1,2,3,4,5)
  	  assertEquals(List(5,4,3,2,1), List.reverse(l1))
  	  
	  val l2 = List(1)
  	  assertEquals(l2, List.reverse(l2))
  	  
	  val l3 = Nil
  	  assertEquals(Nil, List.reverse(l3))
  }

  @Test 
  def testExercise_3_13() {		// foldRight, foldLeft
	  val l1 = List(1,2,3,4,5)
	  
	  val l_fr_plus = List.foldRight(l1, 0)((x,y) => x+y)
	  val l_fl_plus = List.foldLeft (l1, 0)((x,y) => x+y)
	  val l_fl_plus_Viafr = List.foldLeft (l1, 0)((x,y) => x+y)
	  assertEquals(l_fr_plus, l_fl_plus)			// plus is commutative
	  assertEquals(l_fl_plus, l_fl_plus_Viafr)
	  
	  val l_fr_minus = List.foldRight(l1, 0)((x,y) => x-y)
	  val l_fl_minus = List.foldLeft (l1, 0)((x,y) => x-y)
	  val l_fl_minus_Viafr = List.foldLeft (l1, 0)((x,y) => x-y)
	  assertNotEquals(l_fr_minus, l_fl_minus)		// minus is not
	  assertEquals(l_fl_minus, l_fl_minus_Viafr)
	  
	  val l_fr_mult = List.foldRight(l1, 0)((x,y) => x+y)
	  val l_fl_mult = List.foldLeft (l1, 0)((x,y) => x+y)
	  val l_fl_mult_Viafr = List.foldLeft (l1, 0)((x,y) => x+y)
	  assertEquals(l_fr_mult, l_fl_mult)			// multiply is commutative
	  assertEquals(l_fl_mult, l_fl_mult_Viafr)
	  
	  val l_fr_div = List.foldRight(l1, 0)((x,y) => x-y)
	  val l_fl_div = List.foldLeft (l1, 0)((x,y) => x-y)
	  val l_fl_div_Viafr = List.foldLeft (l1, 0)((x,y) => x-y)
	  assertNotEquals(l_fr_div, l_fl_div)			// division is not
	  assertEquals(l_fl_div, l_fl_div_Viafr)
  }

  @Test 
  def testExercise_3_14() {		// append
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List(1,2,3,4,5,99), List.append(l1, List(99)))
//	  assertEquals(List(1,2,3,4,5,99), List.appendViaFoldRight(l1, List(99)))
	  assertEquals(List(1,2,3,4,5,99), List.appendViaFoldLeft(l1, List(99)))
	  
	  val l2 = List(1)
	  assertEquals(List(1, 99), List.append(l2, List(99)))
//	  assertEquals(List(1, 99), List.appendViaFoldRight(l2, List(99)))
	  assertEquals(List(1, 99), List.appendViaFoldLeft(l2, List(99)))
	  
	  val l3 = Nil
	  assertEquals(List(99), List.append(l3, List(99)))
//	  assertEquals(List(99), List.appendViaFoldRight(l3, List(99)))
	  assertEquals(List(99), List.appendViaFoldLeft(l3, List(99)))

  	  val l4 = List(1,2,3,4,5)
	  assertEquals(l4, List.append(l4, Nil))
//	  assertEquals(l4, List.appendViaFoldRight(l4, Nil))
	  assertEquals(l4, List.appendViaFoldLeft(l4, Nil))
	  
	  val l5 = List()
	  assertEquals(l5, List.append(l5, Nil))
//	  assertEquals(l5, List.appendViaFoldRight(l5, Nil))
	  assertEquals(l5, List.appendViaFoldLeft(l5, Nil))
	  
	  val l6 = Nil
	  assertEquals(l6, List.append(l6, Nil))
//	  assertEquals(l6, List.appendViaFoldRight(l6, Nil))
	  assertEquals(l6, List.appendViaFoldLeft(l6, Nil))
  }

  @Test 
  def testExercise_3_15() {		// concat
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List(1,2,3,4,5,99), List.join(List(l1, List(99))))
	  
	  val l2 = List(1)
	  assertEquals(List(1, 99), List.join(List(l2, List(99))))
	  
	  val l3 = Nil
	  assertEquals(List(99), List.join(List(l3, List(99))))

	  val l4 = List(1,2,3,4,5)
	  assertEquals(List(1,2,3,4,5,99), List.join(List(l1, List(99), Nil)))
	  
	  val l5 = List(1)
	  assertEquals(List(1, 99, 0), List.join(List(l2, List(99), List(0))))
	  
	  val l6 = Nil
	  assertEquals(List(99), List.join(List(l3, List(99), Nil, Nil)))
  }

  @Test 
  def testExercise_3_16() {		// add one 
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List(2,3,4,5,6), List.mapIntAddOne(l1))
	  
	  val l2 = List(1)
	  assertEquals(List(2), List.mapIntAddOne(l2))
	  
	  val l3 = Nil
	  assertEquals(Nil, List.mapIntAddOne(l3))
  }

  @Test 
  def testExercise_3_17() {		// convert to String
	  val l1 = List(1.0,2.0,3.0,4.0,5.0)
	  assertEquals(List("1.0","2.0","3.0","4.0","5.0"), List.mapDoubleToString(l1))
	  
	  val l2 = List(1.0)
	  assertEquals(List("1.0"), List.mapDoubleToString(l2))
	  
	  val l3 = Nil
	  assertEquals(Nil, List.mapDoubleToString(l3))
  }

  @Test 
  def testExercise_3_18() {		// map
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List(2,3,4,5,6), List.map(l1)(x => x+1))
	  
	  val l2 = List(1)
	  assertEquals(List(2), List.map(l2)(x => x+1))
	  
	  val l3 = List[Int]()
	  assertEquals(Nil, List.map(l3)(x => x+1))
    	  
	  val l4 = List(1.0,2.0,3.0,4.0,5.0)
	  assertEquals(List("1.0","2.0","3.0","4.0","5.0"), List.map(l4)(x => "" + x))
	  
	  val l5 = List(1.0)
	  assertEquals(List("1.0"), List.map(l5)(x => "" + x))
	  
	  val l6 = List[Int]()
	  assertEquals(Nil, List.map(l6)(x => "" + x))
  }

  @Test 
  def testExercise_3_19() {		// filter
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List(2,4), List.filter(l1)(x => (x%2)==0))

	  val l2 = List(1)
	  assertEquals(l2, List.filter(l2)(x => x<2))
	  
	  val l3 = List[Int]()
	  assertEquals(Nil, List.filter(l3)(x => (x%2)!=0))  
    	  
	  val l4 = List(1.0,2.0,3.0,4.0,5.0)
	  assertEquals(List(2.0,4.0), List.filter(l4)(x => (x%2)==0))
	  
	  val l5 = List(1.0)
	  assertEquals(Nil, List.filter(l5)(x => (x%2)==0))  
	  
	  val l6 = List[Int]()
	  assertEquals(Nil, List.filter(l6)(x => (x%2)==0))  
  }

  @Test 
  def testExercise_3_20() {		// flat map
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List(1,1,2,2,3,3,4,4,5,5), List.flatMap(l1)(x => List(x,x)))
	  
	  val l2 = List(1)
	  assertEquals(List(2,2), List.flatMap(l2)(x => List(x+1, x+1)))
	  
	  val l3 = List[Int]()
	  assertEquals(Nil, List.flatMap(l3)(x => Nil))  
  }

  @Test 
  def testExercise_3_21() {		// filter via flat map
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List(2,4), List.filterViaFlatMap(l1)(x => (x%2)==0))

	  val l2 = List(1)
	  assertEquals(l2, List.filterViaFlatMap(l2)(x => x<2))
	  
	  val l3 = List[Int]()
	  assertEquals(Nil, List.filterViaFlatMap(l3)(x => (x%2)!=0))  
    	  
	  val l4 = List(1.0,2.0,3.0,4.0,5.0)
	  assertEquals(List(2.0,4.0), List.filterViaFlatMap(l4)(x => (x%2)==0))
	  
	  val l5 = List(1.0)
	  assertEquals(Nil, List.filterViaFlatMap(l5)(x => (x%2)==0))  
	  
	  val l6 = List[Int]()
	  assertEquals(Nil, List.filterViaFlatMap(l6)(x => (x%2)==0))
  }

  @Test 
  def testExercise_3_22() {		// add pair wise
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List(2,4,6,8,10), List.zipAddInts(l1, l1))
  	  assertEquals(l1, List.zipAddInts(l1, Nil))

	  val l2 = List[Int]()
	  assertEquals(Nil, List.zipAddInts(l2, l2))

	  val l3 = List(1,2,3,4,5)
	  val l4 = List(99,98,97,96,95)
	  assertEquals(List(100,100,100,100,100), List.zipAddInts(l3, l4))
  }

  @Test 
  def testExercise_3_23() {		// zipWith
  	  val l1 = List(1,2,3,4,5)
	  assertEquals(List(2,4,6,8,10), List.zipWith(l1, l1)((x,y) => x+y))
	  assertEquals(List(1,4,9,16,25), List.zipWith(l1, l1)((x,y) => x*y))
  	  assertEquals(Nil, List.zipWith(l1, List[Int]())((x,y) => x+y))
  	  
	  val l2 = List[Int]()
	  assertEquals(Nil, List.zipWith(l2, l2)((x,y) => x+y))

  	  val l3 = List(1,2,3,4,5)
	  val l4 = List(99,98,97,96,95)
	  assertEquals(List(100,100,100,100,100), List.zipWith(l3, l4)((x,y) => x+y))
	  assertEquals(List(98,96,94,92,90), List.zipWith(l3, l4)((x,y) => Math.abs(x-y)))
  }

  @Test 
  def testExercise_3_24() {		// has subsequence?
	  val l1 = List(1,2,3,4,5)

	  assertTrue(List.hasSubsequence(l1, l1))
	  assertTrue(List.hasSubsequence(l1, Nil))
	  assertTrue(List.hasSubsequence(l1, List[Int]()))
	  assertFalse(List.hasSubsequence(l1, List(99)))
	  assertFalse(List.hasSubsequence(l1, List(2,3,5,4)))
	  assertFalse(List.hasSubsequence(Nil, List(99)))
  }

  // ------------------ Tree ------------------------------------------------------------------------------------------------
  
  @Test 
  def testExercise_3_25() {		// size
	  val t1: Tree[Int] = Branch (Branch(Leaf[Int](5), Leaf[Int](6)), Branch(Leaf[Int](7), Leaf[Int](8)))
	  assertEquals(7, Tree.size(t1))

	  val t2: Tree[Int] = Leaf(99)
	  assertEquals(1, Tree.size(t2))
  }

  @Test 
  def testExercise_3_26() {		// maximum
	  val t1: Tree[Int] = Branch (Branch(Leaf[Int](5), Leaf[Int](6)), Branch(Leaf[Int](7), Leaf[Int](8)))
	  assertEquals(8, Tree.maximum(t1))

	  val t2: Tree[Int] = Leaf(99)
	  assertEquals(99, Tree.maximum(t2))
  }

  @Test 
  def testExercise_3_27() {		// depth
	  val t1: Tree[Int] = Branch (Branch(Leaf[Int](5), Leaf[Int](6)), Branch(Leaf[Int](7), Branch(Leaf[Int](8), Leaf[Int](9))))	  
	  assertEquals(4, Tree.depth(t1))

	  val t2: Tree[Int] = Leaf(99)
	  assertEquals(1, Tree.depth(t2))
  }

  @Test 
  def testExercise_3_28() {		// map
	  val t1: Tree[Int] = Branch (Branch(Leaf[Int](5), Leaf[Int](6)), Branch(Leaf[Int](7), Branch(Leaf[Int](8), Leaf[Int](9))))	  
	  val mappedt1 = Tree.map(t1)((x: Int) => ""+x)
	  assertEquals(
	      Branch (Branch(Leaf[String]("5"), Leaf[String]("6")), Branch(Leaf[String]("7"), Branch(Leaf[String]("8"), Leaf[String]("9")))),
	      mappedt1)

	  val t2: Tree[Int] = Leaf(99)
	  val mappedt2 = Tree.map(t2)((x: Int) => ""+x)
	  assertEquals(Leaf("99"), mappedt2)
  }

  @Test 
  def testExercise_3_29() {		// fold and size/maximum/depth via fold
      val t1: Tree[Int] = Branch (Branch(Leaf[Int](5), Leaf[Int](6)), Branch(Leaf[Int](7), Leaf[Int](8)))
	  assertEquals(7, Tree.size(t1))
	  assertEquals(Tree.size(t1), Tree.sizeViaFold(t1))
	  val t2: Tree[Int] = Leaf(99)
	  assertEquals(1, Tree.size(t2))
	  assertEquals(Tree.size(t2), Tree.sizeViaFold(t2))  
  	  
	  val t3: Tree[Int] = Branch (Branch(Leaf[Int](5), Leaf[Int](6)), Branch(Leaf[Int](7), Leaf[Int](8)))
	  assertEquals(8, Tree.maximum(t3))
	  assertEquals(Tree.maximum(t3), Tree.maximumViaFold(t3))
	  val t4: Tree[Int] = Leaf(99)
	  assertEquals(99, Tree.maximum(t4))
	  assertEquals(Tree.maximum(t4), Tree.maximumViaFold(t4))
  	  
	  val t5: Tree[Int] = Branch (Branch(Leaf[Int](5), Leaf[Int](6)), Branch(Leaf[Int](7), Branch(Leaf[Int](8), Leaf[Int](9))))	  
	  assertEquals(4, Tree.depth(t5))
	  assertEquals(Tree.depth(t5), Tree.depthViaFold(t5))
	  val t6: Tree[Int] = Leaf(99)
	  assertEquals(1, Tree.depth(t6))
	  assertEquals(Tree.depth(t6), Tree.depthViaFold(t6))
  }
}
