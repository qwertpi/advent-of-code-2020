import io.Source.fromFile
import java.lang.management.ManagementFactory
import math.pow

object b extends App {
    def gen_x(amount: Int, i: Int): Int ={
            return amount * i
    }
    def gen_y(amount: Int, i: Int): Int ={
            return amount * i
    }

	val timer = ManagementFactory.getThreadMXBean()
    val start = timer.getCurrentThreadCpuTime()
    val input: Array[Array[Char]] = fromFile("input.txt").getLines.map(x => x.toString.toCharArray()).toArray

    val answer1: BigInt = (0 to input.length).map(i => input(gen_y(1, i) % input.length)(gen_x(1, i) % input(0).length)).filter(x => (x == '#')).length
    val answer2: BigInt = (0 to input.length).map(i => input(gen_y(1, i) % input.length)(gen_x(3, i) % input(0).length)).filter(x => (x == '#')).length
    val answer3: BigInt = (0 to input.length).map(i => input(gen_y(1, i) % input.length)(gen_x(5, i) % input(0).length)).filter(x => (x == '#')).length
    val answer4: BigInt = (0 to input.length).map(i => input(gen_y(1, i) % input.length)(gen_x(7, i) % input(0).length)).filter(x => (x == '#')).length
    val answer5: BigInt = (0 to input.length / 2).map(i => input(gen_y(2, i) % input.length)(gen_x(1, i) % input(0).length)).filter(x => (x == '#')).length
    val final_answer: BigInt = answer1 * answer2 * answer3 * answer4 * answer5
    val end = timer.getCurrentThreadCpuTime()
    
    println(final_answer)
	println(s"Took: ${end-start} nanoseconds, that's ${(end-start)/pow(10,9)} seconds")
}
