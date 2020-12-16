import io.Source.fromFile
import java.lang.management.ManagementFactory
import math.pow

object a extends App {
    def gen_x(amount: Int, i: Int): Int ={
            return amount * i
    }
    def gen_y(amount: Int, i: Int): Int ={
            return amount * i
    }
	
    val timer = ManagementFactory.getThreadMXBean()
    val start = timer.getCurrentThreadCpuTime()
    val input: Array[Array[Char]] = fromFile("input.txt").getLines.map(x => x.toString.toCharArray()).toArray
    
    val answer: Int = (0 to input.length).map(i => input(gen_y(1, i) % input.length)(gen_x(3, i) % input(0).length)).filter(x => (x == '#')).length
    val end = timer.getCurrentThreadCpuTime()

    println(answer)
	println(s"Took: ${end-start} nanoseconds, that's ${(end-start)/pow(10,9)} seconds")
}
