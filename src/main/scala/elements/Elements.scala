package elements

import java.io.{File, FileWriter}

import scala.io.Source

/**
  * Created by alex on 28/01/17
  **/
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

  val wordsBuilder: WordsBuilder = new WordsBuilder(allElements)

  val elementWords: Seq[(String, wordsBuilder.Word)] = for {
    el <- allElements.values.toSeq.sorted
    word <- wordsBuilder.lookup(el)
  } yield {
    el -> word
  }


  val rows: Seq[Seq[String]] = elementWords.foldLeft((None.asInstanceOf[Option[String]], Seq.empty[Seq[String]])) { case (previousElementAndRows, (element, word)) =>
    val maybePreviousElement = previousElementAndRows._1
    val previousRows = previousElementAndRows._2
    def list(f: wordsBuilder.Word => Seq[String]): String = f(word).mkString(", ")
    val elementCell = if (maybePreviousElement.contains(element)) "" else element
    (Some(element), previousRows :+ Seq(elementCell, list(_.map(_._1)), list(_.map(_._2))))
  }._2

  val widths: Seq[Int] = rows.foldLeft(Seq.empty[Int]) { case (widths, thisRow) =>
    if (widths.isEmpty) {
      thisRow.map(_.length)
    }
    else {
      thisRow.zip(widths).map { case (cell, width) => Math.max(cell.length, width)}
    }
  }

  def printRow(row: Seq[String], padding: Char): String = {
    val cells = row.zip(widths).map { case (cell, width) =>
      cell.padTo(width, padding)
    }
    cells.mkString("|", "|", "|")
  }

  val readmeFile = new File("README.md")
  val preamble = Source.fromFile(readmeFile).getLines().takeWhile(!_.startsWith("|"))

  val newContent =
    preamble.toSeq ++
      Seq(printRow(Seq("Element", "Symbols", "Elements"), ' '),
          printRow(Seq(":", ":", ":"), '-')) ++
    rows.map(printRow(_, ' '))

  val writer = new FileWriter(readmeFile)
  writer.append(newContent.mkString("\n"))
  writer.close()
}
