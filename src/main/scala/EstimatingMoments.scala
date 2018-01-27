// Moment: https://en.wikipedia.org/wiki/Moment_(mathematics)
// Second moment: https://en.wikipedia.org/wiki/Rotational_inertia
// 0th moment: Number of distinct elements in a stream.
// 1st moment: Sum of distinct elements in a stream.
// 2nd moment: Sum of squares of distinct elements in a stream.
// Why do we need moments in stats?: https://www.thoughtco.com/what-are-moments-in-statistics-3126234
// --> to identify skewness or kurtosis (fat tail) symmetry of the distribution of the elements.
object EstimatingMoments extends App {
  def finiteStream() : Unit = {
    println("--- Finite Stream ----")
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

    print(s"Average Second Moment is: ${averageSecondMoment}  /  ")
    println(s"Actual Second Moment: ${ams.secondMoment()}")
  }
  def infiniteStream() : Unit = {
    var streamIndex = 15;
    println(s"--- Infinite Stream window size ${streamIndex} ----")
    val a = "a"; val b = "b"; val c = "c"; val d = "d";
    val stream = List(a,b,c,b,d,a,c,d,a,b,d,c,a,a,b,a,b,c,b,d,a,c,d,a,b,d,c,a,a,b,a,b,c,b,d,a,c,d,a,b,d,c,a,a,b,a,b,c,b,d,a,c,d,a,b,d,c,a,a,b);
    val is = new InfiniteStream(stream.take(5));
    stream.drop(5).foreach(item => {
      is.insert(item)
      val ams = new AlonMatiasSzegedy(is.elements)
      val results = List(0,2,4).map(i => Map("index" -> i, "item" -> stream(i), "moment" -> ams.momentAt(i)))

//      results.foreach(result => println(s"Estimated Moment of ${result.getOrElse("item", "N/A")} from " +
//        s"${result.getOrElse("index", "N/A")} is ${result.getOrElse("moment", -1)}"))

      val sumOfSecondMoments = results
        .map(r => r.getOrElse("moment", 0).toString.toDouble)
        .reduce((a,b) => a + b)
      val averageSecondMoment = sumOfSecondMoments / results.size

      print(s"Average Second Moment is: ${averageSecondMoment}  /  ")
      println(s"Actual Second Moment: ${ams.secondMoment()}")
    })
  }

  finiteStream();
  infiniteStream();
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


/**
  Infinite streams? How to find moments, or rather, the points at which to look for moments?
  */
class InfiniteStream(var elements: List[String]){
  var size = elements.size
  def insert(item: String): Unit ={
    if (elements.size < size){
      elements = elements ::: List(item);
    }
    if (shouldChooseItem){
      val indexToReplace = (Math.random() * size).toInt
      elements = elements.patch(indexToReplace, Seq(item), 1)
    }
  }

  def shouldChooseItem: Boolean = {
    // this is naive
    val p = (Math.random() * size).toInt + 1;
    if (p == size)
      true
    else
      false
  }
}