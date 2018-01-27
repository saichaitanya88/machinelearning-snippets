import scala.math.Integral.Implicits.infixIntegralOps
// Moment: https://en.wikipedia.org/wiki/Moment_(mathematics)
// Second moment: https://en.wikipedia.org/wiki/Rotational_inertia
// 0th moment: Number of distinct elements in a stream.
// 1st moment: Sum of distinct elements in a stream.
// 2nd moment: Sum of squares of distinct elements in a stream.
// Why do we need moments in stats?: https://www.thoughtco.com/what-are-moments-in-statistics-3126234
object EstimatingMoments extends App {
  val a = "a"; val b = "b"; val c = "c"; val d = "d";
  val stream = List(a,b,c,b,d,a,c,d,a,b,d,c,a,a,b);
  val ams = new AlonMatiasSzegedy(stream)

  val results = List(2,7,12).map(i => Map("index" -> i, "item" -> stream(i), "moment" -> ams.momentAt(i)))

  results.foreach(result => println(s"Estimated Moment of ${result.getOrElse("item", "N/A")} from " +
    s"${result.getOrElse("index", "N/A")} is ${result.getOrElse("moment", -1)}"))

  val sumOfSecondMoments = results
    .map(r => r.getOrElse("moment", 0).toString.toDouble)
    .reduce((a,b) => a + b)
  val averageSecondMoment = sumOfSecondMoments / results.size

  println(s"Average Second Moment is: ${averageSecondMoment}")
  println(s"Actual Second Moment: ${ams.secondMoment()}")
}

class AlonMatiasSzegedy(val stream: List[String]){
  val size = stream.length;

  def streamFrom(index: Int = 0): List[Element] = {
    var items = List[Element]()
    stream.drop(index).foreach(s => {
      var element = items.find(e => e.item == s)
      if (element.isEmpty){
        items = items ::: List(new Element(s, 1))
      }
      else{
        element.get.value += 1
      }
    })
    items
  }

  def secondMoment(): Double = {
    streamFrom().map(element => Math.pow(element.value, 2)).reduce((a,b) => a + b)
  }

  def momentAt(index: Int): Double = {
    val item = stream(index)
    val element = streamFrom(index).find(e => e.item == item).get
    size * (2 * element.value - 1)
  }

}

class Element(var item: String, var value: Int)