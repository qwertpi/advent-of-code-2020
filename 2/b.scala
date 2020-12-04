import io.Source.fromFile
import java.lang.management.ManagementFactory
import math.pow

object b extends App {
    val timer = ManagementFactory.getThreadMXBean()
    val start = timer.getCurrentThreadCpuTime()
	def check_password(condition: String, password: String): Boolean ={
        val split_condition: Array[String] = condition.split("-")
        val pos1: Int = split_condition(0).toInt - 1
        val resplit_condition: Array[String] = split_condition(1).split(" ")
        val pos2: Int = resplit_condition(0).toInt - 1
        val char: Character = resplit_condition(1).charAt(0)
        return ((password(pos1) == char) ^ (password(pos2) == char))
    }
    val answer: Int = fromFile("input.txt").getLines.toArray.filter(x => check_password(x.split(": ")(0), x.split(": ")(1))).length
    val end = timer.getCurrentThreadCpuTime()
	println(answer)
	println(s"Took: ${end-start} nanoseconds, that's ${(end-start)/pow(10,9)} seconds")
}
