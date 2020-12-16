import io.Source.fromFile
import java.lang.management.ManagementFactory
import math.pow

object a extends App {
    val timer = ManagementFactory.getThreadMXBean()
    val start = timer.getCurrentThreadCpuTime()

    //returns a tuple (row, column)
    def find_row_and_column(seat_code: String): (Int, Int) ={
    	def _find(seat_code: String, min_pos: Int, max_pos: Int): Int ={
    		def _calc_new(min_pos: Int, max_pos: Int): Int ={
    			return (max_pos - min_pos)/2
    		}
    		if (min_pos == max_pos) return min_pos else if ((seat_code(0) == 'F') || (seat_code(0) == 'L')) return _find(seat_code.substring(1), min_pos, min_pos + _calc_new(min_pos, max_pos)) else return _find(seat_code.substring(1), min_pos + _calc_new(min_pos, max_pos) + 1, max_pos)
    	}
    	return (_find(seat_code, 0, 127), _find(seat_code.substring(7), 0, 7))
    }
    def calculate_id(location: (Int, Int)): Int ={
    	location._1 * 8 + location._2
    }
    
    val answer: Int = fromFile("input.txt").getLines.map(x => calculate_id(find_row_and_column(x))).max
    val end = timer.getCurrentThreadCpuTime()
	println(answer)
	println(s"Took: ${end-start} nanoseconds, that's ${(end-start)/pow(10,9)} seconds")
}
