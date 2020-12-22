import math.pow
import io.Source.fromFile
import io.StdIn.readLine
import java.lang.management.ManagementFactory

object both extends App {
    val timer = ManagementFactory.getThreadMXBean()
    val start = timer.getCurrentThreadCpuTime()
    def generate_pos(old_pos: Array[Int]): Array[Int] ={
        var pos: Array[Int] = old_pos
        var prev_el: Int = 0
        for (i <- (0 until pos.length)){
            if (i == 0){pos(i)+= 1}
            else{
                pos(i) = prev_el/(lines.length)
            }
            prev_el = pos(i)
        }
        return pos
    }
    val n: Int = readLine("Enter the value of n to use  ").toInt
    val lines: Array[Int] = fromFile("input.txt").getLines.toArray.map(x => x.toInt)
    var pos: Array[Int] = Array.tabulate(n)(x => if (x == 0) -1 else 0)
    var selected_lines_indexes: Array[Int] = Array.tabulate(n)(_ => 0)
    var selected_lines: Array[Int] = Array.tabulate(n)(_ => 0)
    var solved: Boolean = false
    while (!solved){
        pos = generate_pos(pos)
        selected_lines_indexes = pos.map(_ % lines.length)
        if (selected_lines_indexes.distinct.length == selected_lines_indexes.length){
            selected_lines = selected_lines_indexes.map(x => lines(x))
            if (selected_lines.sum == 2020){
                solved = true
            }
        }
    }
    println(selected_lines.reduceLeft(_ * _))
    val end = timer.getCurrentThreadCpuTime()
    println(s"Took: ${end-start} nanoseconds, that's ${(end-start)/pow(10,9)} seconds")
}
