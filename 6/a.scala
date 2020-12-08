import io.Source.fromFile
import java.lang.management.ManagementFactory
import math.pow

object a extends App {
    val timer = ManagementFactory.getThreadMXBean()
    val start = timer.getCurrentThreadCpuTime()

    def count_num_unique(answers: String): Int ={
        return answers.toCharArray.toSet.size
    }

    val answer: Int = fromFile("input.txt").mkString("").split("\n\n").map(x => count_num_unique(x.replace("\n", ""))).sum
    
    val end = timer.getCurrentThreadCpuTime()
	println(answer)
	println(s"Took: ${end-start} nanoseconds, that's ${(end-start)/pow(10,9)} seconds")
}
