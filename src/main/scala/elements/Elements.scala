package elements


/**
  * Created by alex on 26/01/17
  **/
class Elements(elementsBySymbol: Map[String, String]) {

  type Symbol = String
  type Element = String
  type Word = Seq[(Symbol, Element)]

  val symbols: Seq[Symbol] = elementsBySymbol.keys.toSeq

  def lookup(word: String): Seq[Word] = {
    def _lookup(partialWord: String, currentSymbols: Seq[Symbol] = Seq.empty, currentWords: Seq[Word] = Seq.empty): Seq[Word] = {
      if (partialWord.isEmpty) {
        val newWord: Word = for {
          symbol <- currentSymbols
          element <- elementsBySymbol.get(symbol)
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

object Elements extends App {

  val allElements: Map[String, String] = Seq(
    "H" -> "Hydrogen",
    "He" -> "Helium",
    "Li" -> "Lithium",
    "Be" -> "Beryllium",
    "B" -> "Boron",
    "C" -> "Carbon",
    "N" -> "Nitrogen",
    "O" -> "Oxygen",
    "F" -> "Fluorine",
    "Ne" -> "Neon",
    "Na" -> "Sodium",
    "Mg" -> "Magnesium",
    "Al" -> "Aluminium",
    "Si" -> "Silicon",
    "P" -> "Phosphorus",
    "S" -> "Sulfur",
    "Cl" -> "Chlorine",
    "Ar" -> "Argon",
    "K" -> "Potassium",
    "Ca" -> "Calcium",
    "Sc" -> "Scandium",
    "Ti" -> "Titanium",
    "V" -> "Vanadium",
    "Cr" -> "Chromium",
    "Mn" -> "Manganese",
    "Fe" -> "Iron",
    "Co" -> "Cobalt",
    "Ni" -> "Nickel",
    "Cu" -> "Copper",
    "Zn" -> "Zinc",
    "Ga" -> "Gallium",
    "Ge" -> "Germanium",
    "As" -> "Arsenic",
    "Se" -> "Selenium",
    "Br" -> "Bromine",
    "Kr" -> "Krypton",
    "Rb" -> "Rubidium",
    "Sr" -> "Strontium",
    "Y" -> "Yttrium",
    "Zr" -> "Zirconium",
    "Nb" -> "Niobium",
    "Mo" -> "Molybdenum",
    "Tc" -> "Technetium",
    "Ru" -> "Ruthenium",
    "Rh" -> "Rhodium",
    "Pd" -> "Palladium",
    "Ag" -> "Silver",
    "Cd" -> "Cadmium",
    "In" -> "Indium",
    "Sn" -> "Tin",
    "Sb" -> "Antimony",
    "Te" -> "Tellurium",
    "I" -> "Iodine",
    "Xe" -> "Xenon",
    "Cs" -> "Caesium",
    "Ba" -> "Barium",
    "La" -> "Lanthanum",
    "Ce" -> "Cerium",
    "Pr" -> "Praseodymium",
    "Nd" -> "Neodymium",
    "Pm" -> "Promethium",
    "Sm" -> "Samarium",
    "Eu" -> "Europium",
    "Gd" -> "Gadolinium",
    "Tb" -> "Terbium",
    "Dy" -> "Dysprosium",
    "Ho" -> "Holmium",
    "Er" -> "Erbium",
    "Tm" -> "Thulium",
    "Yb" -> "Ytterbium",
    "Lu" -> "Lutetium",
    "Hf" -> "Hafnium",
    "Ta" -> "Tantalum",
    "W" -> "Tungsten",
    "Re" -> "Rhenium",
    "Os" -> "Osmium",
    "Ir" -> "Iridium",
    "Pt" -> "Platinum",
    "Au" -> "Gold",
    "Hg" -> "Mercury",
    "Tl" -> "Thallium",
    "Pb" -> "Lead",
    "Bi" -> "Bismuth",
    "Po" -> "Polonium",
    "At" -> "Astatine",
    "Rn" -> "Radon",
    "Fr" -> "Francium",
    "Ra" -> "Radium",
    "Ac" -> "Actinium",
    "Th" -> "Thorium",
    "Pa" -> "Protactinium",
    "U" -> "Uranium",
    "Np" -> "Neptunium",
    "Pu" -> "Plutonium",
    "Am" -> "Americium",
    "Cm" -> "Curium",
    "Bk" -> "Berkelium",
    "Cf" -> "Californium",
    "Es" -> "Einsteinium",
    "Fm" -> "Fermium",
    "Md" -> "Mendelevium",
    "No" -> "Nobelium",
    "Lr" -> "Lawrencium",
    "Rf" -> "Rutherfordium",
    "Db" -> "Dubnium",
    "Sg" -> "Seaborgium",
    "Bh" -> "Bohrium",
    "Hs" -> "Hassium",
    "Mt" -> "Meitnerium",
    "Ds" -> "Darmstadtium",
    "Rg" -> "Roentgenium",
    "Cn" -> "Copernicium",
    "Nh" -> "Nihonium",
    "Fl" -> "Flerovium",
    "Mc" -> "Moscovium",
    "Lv" -> "Livermorium",
    "Ts" -> "Tennessine",
    "Og" -> "Oganesson"
  ).toMap

  val elements: Elements = new Elements(allElements)

  val elementWords = for {
    el <- allElements.values.toSeq.sorted
    word <- elements.lookup(el)
  } yield {
    el -> word
  }
  elementWords.foreach { case(el, word) =>
    val symbols = word.map(_._1).mkString(", ")
    val els = word.map(_._2).mkString(", ")
    println(s"$el: $symbols ($els)")
  }
}