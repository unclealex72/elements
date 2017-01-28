package elements

import enumeratum.EnumEntry.Lowercase
import enumeratum._
import org.specs2.mutable.Specification

import scala.collection.immutable.IndexedSeq

/**
  * Created by alex on 26/01/17
  **/
class WordsBuilderSpec extends Specification {

  val A: (String, String) = "a" -> "A"
  val AL: (String, String) = "al" -> "AL"
  val L: (String, String) = "l" -> "L"
  val LE: (String, String) = "le" -> "LE"
  val E: (String, String) = "e" -> "E"
  val EX: (String, String) = "ex" -> "EX"

  val elementsBySymbol: Map[String, String] = Seq(A, AL, L, LE, E, EX).toMap
  val wordsBuilder: WordsBuilder = new WordsBuilder(elementsBySymbol)

  "alex" should {
    "be constructable in two ways" in {
      wordsBuilder.lookup("alex") must containTheSameElementsAs(Seq(Seq(AL, EX), Seq(A, L, EX)))
    }
  }

  "lex" should {
    "be constructable in one way" in {
      wordsBuilder.lookup("lex") must containTheSameElementsAs(Seq(Seq(L, EX)))
    }
  }

  "alx" should {
    "not be constructable" in {
      wordsBuilder.lookup("alx") must beEmpty
    }
  }

  "alle" should {
    "be constructable in four ways" in {
      wordsBuilder.lookup("alle") must containTheSameElementsAs(Seq(
        Seq(A, L, L, E),
        Seq(AL, LE),
        Seq(AL, L, E),
        Seq(A, L, LE)))
    }
  }
}
