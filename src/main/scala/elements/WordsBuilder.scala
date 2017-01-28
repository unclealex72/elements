package elements


/**
  * Created by alex on 26/01/17
  **/
class WordsBuilder(wordsBySymbol: Map[String, String]) {

  type Symbol = String
  type Element = String
  type Word = Seq[(Symbol, Element)]

  val symbols: Seq[Symbol] = wordsBySymbol.keys.toSeq

  def lookup(word: String): Seq[Word] = {
    def _lookup(partialWord: String, currentSymbols: Seq[Symbol] = Seq.empty, currentWords: Seq[Word] = Seq.empty): Seq[Word] = {
      if (partialWord.isEmpty) {
        val newWord: Word = for {
          symbol <- currentSymbols
          element <- wordsBySymbol.get(symbol)
        } yield {
          (symbol, element)
        }
        currentWords :+ newWord
      }
      else {
        symbols.foldLeft(currentWords) { (words, symbol) =>
          if (partialWord.toLowerCase.startsWith(symbol.toLowerCase)) {
            val remainingWord = partialWord.substring(symbol.length)
            val newSymbols: Seq[Symbol] = currentSymbols :+ symbol
            words ++ _lookup(remainingWord, newSymbols, currentWords)
          }
          else {
            words
          }
        }
      }
    }
    _lookup(word)
  }
}

