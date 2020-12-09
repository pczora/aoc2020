import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    def input = Source.fromResource("input.txt").getLines().toList
    def program = input.map(parseLine)
    def result = runMachine(0, 0, program, Nil)
    println(result)

  }

  def parseLine(line: String): Instruction = {
    def parseAmount(s: String): Int = {
      val amount = s.drop(1).toInt
      if (s.take(1) == "-") amount * -1 else amount
    }

    val tokens = line.split(" ")
    (tokens(0), tokens(1)) match {
      case (name, amount) => Instruction(name, parseAmount(amount))
    }
  }

  def runMachine(acc: Int, pc: Int, program: List[Instruction], alreadyRun: List[(Int, Instruction)]): Int = {
    val instruction = program(pc)
    println("Running instruction " + instruction + "at pc: " + pc + " acc: " + acc)
    if (alreadyRun.contains((pc, instruction))) acc else {
      program(pc) match {
        case Instruction("nop", amount) => runMachine(acc, pc + 1, program, (pc, Instruction("nop", amount)) :: alreadyRun)
        case Instruction("acc", amount) => runMachine(acc + amount, pc + 1, program, (pc, Instruction("acc", amount)) :: alreadyRun)
        case Instruction("jmp", amount) => runMachine(acc, pc + amount, program, (pc, Instruction("jmp", amount)) :: alreadyRun)
      }
    }
  }
  case class Instruction(name: String, amount: Int)
}
