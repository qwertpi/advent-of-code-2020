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

    def gen_answer(x_amount: Int, y_amount: Int): BigInt = {
        return (0 to input.length / y_amount).map(i => input(gen_y(y_amount, i) % input.length)(gen_x(x_amount, i) % input(0).length)).filter(x => (x == '#')).length
    }
    
    val final_answer: BigInt = gen_answer(1,1) * gen_answer(3,1) * gen_answer(5,1) * gen_answer(7,1) * gen_answer(1,2)
    val end = timer.getCurrentThreadCpuTime()
    
    println(final_answer)
	println(s"Took: ${end-start} nanoseconds, that's ${(end-start)/pow(10,9)} seconds")
}
