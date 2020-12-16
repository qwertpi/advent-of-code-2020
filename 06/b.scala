import io.Source.fromFile
import java.lang.management.ManagementFactory
import math.pow

object b extends App {
    val timer = ManagementFactory.getThreadMXBean()
    val start = timer.getCurrentThreadCpuTime()

    def count_num_common(answers: Array[String]): Int ={
        //creates a binary string where each bit is a boolean for the existence of a single character
        //eg abd gives 11010000000..
        def _gen_binary(x: String): String ={
            var tmp: Array[Char] = ("0"*26).toCharArray
            for (char <- x) {
                tmp(char.toInt - 97) = '1'
            }
            return tmp.mkString("")
        }

        //bitwise and
        def and(a: String, b: String): String ={
            var result = ""
            for (i <- (0 until a.length)) {
                if ((a(i) == b(i)) && (a(i) == '1')) result += "1" else result += "0"
            }
            return result
        }

        //takes a bitwise and on all answers
        var fin: String = "1"*26
        for (answer <- answers.map(x => _gen_binary(x))) {
            fin = and(fin, answer)
        }
        //returns the number of 1s in the binary string produced after the AND of all the answers
        return fin.toString.count(_ == '1')
    }

    val answer: Int = fromFile("input.txt").mkString("").split("\n\n").map(x => count_num_common(x.split("\n"))).sum
    
    val end = timer.getCurrentThreadCpuTime()
	println(answer)
	println(s"Took: ${end-start} nanoseconds, that's ${(end-start)/pow(10,9)} seconds")
}
