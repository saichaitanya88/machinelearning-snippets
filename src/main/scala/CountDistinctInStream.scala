import scala.io.Source
import scala.util.hashing.MurmurHash3
import sys.process._


// src: https://en.wikipedia.org/wiki/Flajolet%E2%80%93Martin_algorithm
object CountDistinctInStream extends App {
  val words = "1 2 3 4 5 6 7 8 9 0 1 2 5 6 7 a b c d f e g h 1".split(" ")
  val size = 1231 // randomly chosen

  println(s"Estimates Average Number of Distinct words: ${getDistinct} and actual distinct ${words.distinct.length}")
  // Results:
  // Estimates Average Number of Distinct words: 16.0 and actual distinct 18
  // Note: There's an issue with large word documents. which is off by one bit,
  // and so.. the error gets larger as the number gets larger?

  def getDistinct(): Double = {
    var largestTrailingZeros = 0

    for(i<-0 to words.length - 1){
      val word = words(i)
      val h = hash(word)
      val result = h.toBinaryString
      val trailingZeros = Integer.numberOfTrailingZeros(h)// result.length() - result.lastIndexOf("1")
      if (trailingZeros < result.length())
        largestTrailingZeros = Math.max(trailingZeros, largestTrailingZeros)
    }

    Math.pow(2, largestTrailingZeros)
  }

  def hash(word: String) : Int = {
    word.hashCode % size
    //MurmurHash3.stringHash(word, 0)
  }

}