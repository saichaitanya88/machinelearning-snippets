import scala.util.hashing.MurmurHash3
import scala.io.Source

// Src: http://www.maxburstein.com/blog/creating-a-simple-bloom-filter/

object BloomFilterApp extends App{
  val filter = new BloomFilter(500000, 7)
  // Load the ubuntu dictionary
  val lines = Source.fromFile("/usr/share/dict/american-english").getLines
  var lineCount = 0;
  lines.foreach(line => {
    filter.add(line)
    lineCount = lineCount + 1
  })

  println(s"Loaded ${lineCount} lines!")
  println(s"Googl3 probably exists?: " + filter.lookup("Googl3"))
  println(s"4017714 probably exists?: " + filter.lookup("4017714"))
  println(s"AOL probably exists?: " + filter.lookup("AOL"))
  println(s"Hello probably exists?: " + filter.lookup("hello"))
  println(s"Max probably exists?: " + filter.lookup("Max"))
  println(s"mice probably exists?: " + filter.lookup("mice"))
  println(s"3 probably exists?: " + filter.lookup("3"))
}

class BloomFilter (size: Int, hashCount: Int) {
  var bitArray: Array[Boolean] = Array.fill[Boolean](size)(false)

  def add(text: String): Unit ={
    for (i<-0 to hashCount){
      val result = MurmurHash3.stringHash(text, i) % size
      // MurmurHash3.stringHash sometimes returns -ve value.
      // Ignore this value for simplicity, at the cost of potentially lower accuracy of the filter
      if (result >= 0)
        bitArray(result) = true
    }
  }

  def lookup(text: String): Boolean = {
    for (i<-0 to hashCount){
      val result = MurmurHash3.stringHash(text, i) % size
      if (result >= 0 && bitArray(result) == false)
        return false
    }

    true
  }
}
