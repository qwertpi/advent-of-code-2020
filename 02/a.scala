import io.Source.fromFile
import java.lang.management.ManagementFactory
import math.pow

object a extends App {
    val timer = ManagementFactory.getThreadMXBean()
    val start = timer.getCurrentThreadCpuTime()
	def check_password(condition: String, password: String): Boolean ={
        val split_condition: Array[String] = condition.split("-")
        val min: Int = split_condition(0).toInt
        val resplit_condition: Array[String] = split_condition(1).split(" ")
        val max = resplit_condition(0).toInt
        val char: Character = resplit_condition(1).charAt(0)
        val num_char: Int = password.toCharArray.filter(_ == char).length
        return ((num_char >= min) && (num_char <= max))
    }
    val answer: Int = fromFile("input.txt").getLines.toArray.filter(x => check_password(x.split(": ")(0), x.split(": ")(1))).length
    val end = timer.getCurrentThreadCpuTime()
	println(answer)
	println(s"Took: ${end-start} nanoseconds, that's ${(end-start)/pow(10,9)} seconds")
}
