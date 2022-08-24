package com.tkroman.kpi.y2022.l1
import munit.FunSuite
import List.*
class OptionalSuite extends FunSuite {


  test("distinctBy 1 Nil") {
    val expected = Nil
    val actual = distinctBy(Nil, x => x==3)
    assertEquals(actual, expected)
  }
  test("distinctBy 2") {
    val expected = List(2, 1, 3)
    val actual = distinctBy(List(2, 4, 1, 3), x => x % 3)
    assertEquals(actual, expected)
  }
  test("distinctBy 3") {
    val expected = List(2, 3, 4, 1)
    val actual = distinctBy(List(2, 3, 4, 1), x => x % 5)
    assertEquals(actual, expected)
  }




  test("minBy 1 Nil") {
    val expected = None
    val actual = minBy(Nil, x => x=="fff" )
    assertEquals(actual, expected)
  }
  test("minBy 2") {
    val expected = Some(6)
    val actual = minBy(List(5, 6, 7, 1, 4), x => x % 6)
    assertEquals(actual, expected)
  }



  test("splitAt 1") {
    val expected = (List("kk", "33", 4), List(9, 8))
    val actual = splitAt(List("kk", "33", 4, 9, 8), 3)
    assertEquals(actual, expected)
  }
  test("splitAt 2 Nil") {
    val expected = (Nil, Nil)
    val actual = splitAt(Nil, 8)
    assertEquals(actual, expected)
  }
  
  test("dropLastWhile 1 Nil") {
    val expected = Nil
    val actual = dropLastWhile(Nil, x => x == 9)
    assertEquals(actual, expected)
  }
  test("dropLastWhile 2") {
    val expected = List(3, 5)
    val actual = dropLastWhile(List(3, 5, 9, 8, 10), x => x == 9)
    assertEquals(actual, expected)
  }

}
