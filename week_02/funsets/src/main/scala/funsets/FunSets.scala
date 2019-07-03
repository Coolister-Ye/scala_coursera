package funsets


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
    def singletonSet(elem: Int): Set = (x: Int) => x == elem


  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
    def union(s: Set, t: Set): Set = (x: Int) => contains(s, x) || contains(t, x)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
    def intersect(s: Set, t: Set): Set = (x: Int) => contains(s, x) && contains(t, x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Set, t: Set): Set = (x: Int) => contains(s, x) && !contains(t, x)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Set, p: Int => Boolean): Set = intersect(s, p)


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (diff(s, p)(a)) false
      else iter(a + 1)
    }
    iter(-1000)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   * here we need to use the concepts of univeral && existential set
   * set1: if all elements in s, but not in p satisify
   * forall(s, (x: Int) => !p(x)) will return true,
   * but we actually need to use the result inversely
   * 简单来说就是有一个存在的补集是都不存在，forall() 里面表达的意思是,
   * x在s里面，但不再p的补集里面， forall就是所有在s里面的元素都在p的补集里面，
   * 就是s和p不相交，然后再取反就是我们想要的结果了
   */
    def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, (x: Int) => !p(x))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   * 这里也是比较绕的，我们要返回一个Set（Int => Boolean）,y在这里变成参数，我们要检查有没有x
   * 满足s,同事f(x)满足p
   */
    def map(s: Set, f: Int => Int): Set = (y: Int) => exists(s, (x: Int) => f(x) == y)
  
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
