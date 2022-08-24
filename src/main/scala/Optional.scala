package com.tkroman.kpi.y2022.l1
import scala.annotation.tailrec
import scala.collection.mutable.StringBuilder
import List.*
import scala.collection.mutable

enum List[+A]:
  case Nil
  case Cons(hd: A, tl: List[A])

  override def toString: String =
    @scala.annotation.tailrec
    def go(sb: mutable.StringBuilder, as: List[A]): String = {
      as match {
        case Nil => sb.append(']').result
        case Cons(hd, Nil) => sb.append(hd).append(']').result
        case Cons(hd, tl) => go(sb.append(hd).append(", "), tl)
      }
    }

    go(new mutable.StringBuilder("["), this)

object List:
  def empty[A]: List[A] = Nil
  def apply[A](xs: A*): List[A] = of(xs *)
  def of[A](xs: A*): List[A] =
    xs.foldRight(Nil: List[A]) { case (x, acc) => Cons(x, acc) }





@tailrec
def reverse[A](x: List[A], y: List[A]=Nil): List[A] = {
  x match
    case Nil => y
    case Cons(hd, tl) => reverse(tl, Cons(hd, y))
}

@tailrec
def find[A, B](list: List[A], f: A => B, element: B): Boolean = {
  list match
    case Nil => false
    case Cons(hd, tl) =>
      if (element.equals(f(hd))) true
      else
        find(tl, f, element)
}


def distinctBy[A, B](xs: List[A], f: A => B): List[A] = {
  @tailrec
  def go[A, B](list: List[A], f: A => B, y: List[A]): List[A] =
    list match
      case Nil => reverse(y)
      case Cons(hd, tl) =>
        if (find(tl, f, f(hd)))
          go(tl, f, y)
        else go(tl, f, Cons(hd, y))

  go(xs, f, Nil)
}




def compareMin[B] (x: B, y: B)(implicit ord: Ordering[B]): Boolean = {
  if (ord.compare(x, y) >= 0)
    false
  else true
}

def minBy[A, B](xs: List[A], f: A => B)(implicit ord: Ordering[B]): Option[A]= {
  @tailrec
  def go(x: List[A], f: A => B, min: B, resultMin: Option[A]): Option[A]=
    x match
      case Nil => resultMin
      case Cons(hd, tl) =>
        if( compareMin(min, f(hd)) )
          go(tl, f, min, resultMin)
        else go(tl, f, f(hd), Some(hd))

  xs match
    case Nil => None
    case Cons(hd, tl) => go(tl, f, f(hd), Some(hd))
}




def splitAt[A](xs: List[A], n: Int): (List[A], List[A])= {
  @tailrec
  def go[A](x: List[A], n: Int, i: Int, y: List[A]): (List[A], List[A]) =
    x match
      case Nil => (x, Nil)
      case Cons(hd, Nil) => (x,y)
      case Cons(hd, tl) =>
        if (i==n)
          (reverse(y), x)
        else
          go(tl, n, i+1, Cons(hd, y))

  go(xs, n, 0, Nil)
}





def dropLastWhile[A](xs: List[A], f: A => Boolean): List[A]={
  @tailrec
  def go[A](x: List [A], f: A => Boolean, y: List[A]): List[A]=
    x match
      case Nil => reverse(y)
      case Cons(hd, tl) =>
        if (f(hd))
          reverse(y)
        else go(tl, f, Cons(hd, y))

  go(xs, f, Nil)
}

@main def run() =
  println("Hello")



