import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    def input = Source.fromResource("input.txt").getLines().toList

    val bagGraph = input.map(parseBag).toMap
    println(bagGraph.count(elem => {
      elem match {
        case (bag, _) =>
          containsTransitively(bagGraph, bag, Bag("shiny gold"))
      }
    }))

  }

  def parseBag(line: String) = {
    def parseBagName(line: String) = {
      line.split(" ")(0) + " " + line.split(" ")(1)
    }

    def parseContainedBags(line: String) = {
      def getAmountFromContainsLine(line: String) = {
        line.strip().take(1)
      }
      def getBagNameFromContainsLine(line: String) = {
        val parts = line.drop(1).strip().split(" ")
        parts(0) + " " + parts(1)
      }

      val bagList = line.split("contain")
      if (bagList(1).contains("no other")) Nil
      else {
        bagList(1)
          .split(',')
          .map(_.strip)
          .map(s =>
            Contains(
              Bag(getBagNameFromContainsLine(s)),
              getAmountFromContainsLine(s).toInt
            )
          )
          .toList
      }
    }

    (Bag(parseBagName(line)), parseContainedBags(line))

  }

  def containsTransitively(
      graph: Map[Bag, List[Contains]],
      bag: Bag,
      contains: Bag
  ): Boolean = {

    def containsTransitivelyContainsList(
        graph: Map[Bag, List[Contains]],
        containsList: List[Contains],
        contains: Bag
    ): Boolean = {
      containsList.exists(c => c.bag == contains) || {
        containsList match {
          case Nil => false
          case x :: xs =>
            containsTransitivelyContainsList(
              graph,
              graph(x.bag),
              contains
            ) || containsTransitivelyContainsList(graph, xs, contains)
        }
      }
    }

    val containsList = graph(bag)
    containsTransitivelyContainsList(graph, containsList, contains)
  }
}

case class Bag(name: String)
case class Contains(bag: Bag, amount: Int)
