
object DistanceMeasures extends App{
  println(s"HammingDistance(${10},${17}): " + new HammingDistance().distance(10,17))
  println(s"EuclideanDistance((${10},${16}),(${37},${6})): " + new EuclideanDistance().distance(new Point(10,16), new Point(37,6)))
  println(s"JaccardDistance((1,2,3,4,5),(3,4,5,6,7,8,9)): " + new JaccardDistance().distance(Set(1,2,3,4,5), Set(3,4,5,6,7,8,9)))
  println(s"EditDistance('sittin', 'sitting'): " + new EditDistance().distance("sittin", "sitting"))
  println(s"CosineDistance({'foo': 2, 'bar': 3, 'baz': 5 }, {'foo': 1, 'bar': 0, 'baz': 20 }): "
    + new CosineDistance().distance(Map("foo" -> 2, "bar" -> 3, "baz" -> 5), Map("foo" -> 1, "bar" -> 0, "baz" -> 20)))
}

// src: https://en.wikipedia.org/wiki/Hamming_distance
class HammingDistance {
  def distance(x: Int, y: Int): Int ={
    var dist = 0
    var v = x ^ y
    while (v != 0){
      dist = dist + 1
      v &= v -1
    }
    dist
  }
}

// src: https://en.wikipedia.org/wiki/Euclidean_distance
class EuclideanDistance{
  def distance(p: Point, q: Point): Double ={
    Math.sqrt(Math.pow((q.x-p.x),2) + Math.pow((q.y-p.y),2))
  }
}

// src: http://rosettacode.org/wiki/Levenshtein_distance
class EditDistance{
  def minimum(i1: Int, i2: Int, i3: Int) = Math.min(Math.min(i1,i2),i3)
  def distance(wordA: String, wordB: String): Double = {
    val dist = Array.tabulate(wordB.length+1, wordA.length+1){
      (j,i) => if(j==0) i else if (i==0) j else 0
    }
    for(j<-1 to wordB.length; i<-1 to wordA.length){
      // if letters are equal, keep old distance
      // else increment distance of previous paths + 1, choose minimum value
      dist(j)(i) =  if (wordB(j-1) == wordA(i-1)) dist(j-1)(i-1)
      else minimum(dist(j-1)(i) + 1, dist(j)(i-1)+1, dist(j-1)(i-1) + 1)
    }
    dist(wordB.length)(wordA.length)
  }
}

class CosineDistance{
  def distance(a: Map[String, Int], b: Map[String, Int]): Double = {
    dotprod(a,b) / norm(a) / norm(b)
  }

  def dotprod(a: Map[String, Int], b: Map[String, Int]): Double = {
      a
      .keys
      .filter(k => b.contains(k))
      .map(k => a.getOrElse(k, 0) * b.getOrElse(k, 0))
      .foldLeft(0)(_+_)
  }

  def norm(a : Map[String,Int]): Double = Math.sqrt(dotprod(a,a))
}

// src: https://en.wikipedia.org/wiki/Jaccard_index
class JaccardDistance{
  def distance(a: Set[Int], b: Set[Int]): Double = {
    val union = a.union(b)
    val intersection = a.intersect(b)
    intersection.size.toDouble / union.size
  }
}

class Point(var x: Int = 0, var y: Int = 0)
