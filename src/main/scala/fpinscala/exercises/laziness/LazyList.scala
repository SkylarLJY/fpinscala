package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.cons
import fpinscala.exercises.laziness.LazyList.empty
import fpinscala.exercises.laziness.LazyList.unfold
import scala.runtime.LazyVals

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  // not tail rec
  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  
  def toListTailRec: List[A] = 
    @annotation.tailrec
    def go(ll: LazyList[A], acc: List[A]): List[A] = ll match
      case Empty => acc.reverse
      case Cons(h, t) => go(t(), h()::acc)
    go(this, Nil)
    

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] =  this match
    case Cons(h, t) if n>1 => cons(h(), t().take(n-1))
    case Cons(h, t) if n==1 => cons(h(), empty)
    case _ => empty
      

  def drop(n: Int): LazyList[A] = this match
    case Cons(h, t) if n>0 => t().drop(n-1)
    case _ => this
  

  def takeWhile(p: A => Boolean): LazyList[A] =
    // case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    // case _ => empty 
    // this match
    foldRight(empty)((a, b)=>if p(a) then cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b)=>p(a) && b)
  

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

 

  def map[B](f: A=>B): LazyList[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A=>Boolean): LazyList[A] = 
    foldRight(empty[A])((a, acc) => if f(a) then cons(a, acc) else acc)

  def append[B>:A](lst: =>LazyList[B]): LazyList[B] = foldRight(lst)((a, acc) => cons(a, acc))

  def flatMap[B](f: A=>LazyList[B]): LazyList[B] = foldRight(empty[B])((a, acc)=>f(a).append(acc))

  // unfold 
  def mapUnfold[B](f: A=>B): LazyList[B] = LazyList.unfold(this)(lst => this match
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  )

  def takeUnfold(n: Int): LazyList[A] = LazyList.unfold((this, n)): 
    case (Cons(h, t), n) if n>1 => Some(h(), (t(), n-1))
    case (Cons(h, t), 1) => Some((h(), (empty, 0)))
    case _ => None

  def takeWhileUnfold(p: A=>Boolean): LazyList[A] = unfold(this):
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None

  def zipWith[B,C](that: LazyList[B])(f: (A, B) => C): LazyList[C] = unfold((this, that)):
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] = unfold((this, that)):
   case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
   case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), empty))
   case (_, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
   case _ => None

  def startsWith[A](s: LazyList[A]): Boolean = 
    zipAll(s).takeWhile((a, b) => b.isDefined).forAll((a, b) => a==b)

  def tails: LazyList[LazyList[A]] = 
    unfold(this):
      case l @ Cons(h, t) => Some((l, t()))
      case _ => None 
    .append(Empty)

  def scanRight[B](x: =>B)(f: (A, =>B)=>B): LazyList[B] = 
    foldRight(x -> LazyList(x)): (a, b) => // initilize with a tuple 
      lazy val b1 = b // use lazy val to ensure only on evaluation 
      val v = f(a, b1._1)
      v -> cons(v, b1._2)
    ._2



object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    lazy val lst = LazyList.cons(a, continually(a))
    lst

  def from(n: Int): LazyList[Int] = cons(n, from(n+1))

  lazy val fibs: LazyList[Int] = 
    def func(n1: Int, n2: Int): LazyList[Int] = cons(n1, func(n1, n1+n2))
    func(0,1) 
     

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = f(state) match
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    
  lazy val fibsViaUnfold: LazyList[Int] = unfold((0, 1))((n1, n2)=>Some((n1, (n2, n1+n2))))

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(x => Some((x, x+1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(())(_ => Some((a, ())))

  lazy val onesViaUnfold: LazyList[Int] = continuallyViaUnfold(1)
  
  
  

def hasSubsequence[A](sup: LazyList[A], sub: LazyList[A]): Boolean = 
  sup.tails.exists(_.startsWith(sub))