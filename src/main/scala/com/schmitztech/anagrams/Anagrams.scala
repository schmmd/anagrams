package com.schmitztech.anagrams

import java.io.File
import scala.io.Source

object Anagrams {
  /** Anagrams takes a single argument, which is a path to a word dictionary.
    * The dictionary must be a text file with one word per line. */
  def main(args: Array[String]): Unit = {
    require(args.size == 1, "Exactly 1 argument is required.")

    val dictionaryFile = new File(args(0))
    require(dictionaryFile.exists, "The dictionary file does not exist: " + dictionaryFile.getPath)

    // Load the words from the dictionary
    val dictionaryWords = {
      val source = Source.fromFile(dictionaryFile)
      try {
        source.getLines.toList
      }
      finally {
        source.close()
      }
    }

    println(s"${dictionaryWords.size} words loaded from ${dictionaryFile.getPath}.")
    println()

    // Create a map from LetterCounts to the words in the dictionary that
    // share the same letter counts.  This will be used for fast lookup
    // of anagrams.
    val dictionaryCountMap =
      dictionaryWords.foldLeft(Map.empty[LetterCounts, Seq[String]].withDefaultValue(Seq.empty[String])){
        (map, word) =>
          val letterCount = LetterCounts(word)
          val words = map(letterCount)
          map + (letterCount -> (word +: words))
      }

    println("Please enter the words you want to find anagrams for.")
    println("Enter a blank line to signal the end of your input.")

    // Read words from standard in from user.
    val inputWords = {
      Source.stdin.getLines.takeWhile(!_.trim.isEmpty).toList
    }

    // Search for and print anagrams for each word.
    for (word <- inputWords) {
      val anagrams = dictionaryCountMap(LetterCounts(word))
      println("Anagrams in dictionary for: " + word)
      if (anagrams.isEmpty) {
        println("(none)")
      }
      for (anagram <- anagrams) {
        println("* " + anagram)
      }
      println()
    }
  }
}

class LetterCounts private (private val counts: Array[Int]) {
  def this() = this(new Array('z' - 'a' + 1))

  /** Show the letters in this LetterCount collection. */
  override def toString: String = {
    val builder = new StringBuilder()

    for (i <- 0 until counts.size) {
      val c = ('a' + i).toChar
      builder.append(c.toString * counts(i))
    }

    builder.toString
  }

  // Override equals to use the equals of the underlying array as a Seq.
  override def equals(any: Any) = any match {
    case that: LetterCounts => this.counts.toSeq == that.counts.toSeq
    case _ => false
  }

  // Override equals to use the hashCode of the underlying array as a Seq.
  override def hashCode = counts.toSeq.hashCode

  /** Convert a character into an index in the counts array. */
  private def index(c: Char): Int = {
    require(c >= 'a' && c <= 'z', "Letters must be lowercase and a-z.")

    c - 'a'
  }

  /** Add a character to the bank of counts. */
  def add(c: Char): Unit = {
    val i = index(c)
    counts(i) += 1
  }

  /** Get the number of counts for a character. */
  def get(c: Char): Int = {
    val i = index(c)
    counts(i)
  }
}

object LetterCounts {
  // Create a LetterCounts object for a given word.
  def apply(word: String): LetterCounts = {
    val counts = new LetterCounts()
    for (c <- word) {
      counts.add(c)
    }

    counts
  }
}
