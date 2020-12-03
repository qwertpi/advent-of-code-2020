import math.floor
import io.Source.fromFile
import io.StdIn.readLine

object a extends App {
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
    def product(a: Array[Int]): Int = {
        var total: Int = 1
        for (el <- a){
            total *= el
        }
        return total
    }
    val n: Int = readLine("Enter the value of n to use  ").toInt
    val lines: Array[Int] = fromFile("input.txt").getLines.toArray.map(x => x.toInt)
    var pos: Array[Int] = Array.tabulate(n)(x => if (x==0) -1 else 0)
    var selected_lines_indexes: Array[Int] = Array.tabulate(n)(x => 0)
    var selected_lines: Array[Int] = Array.tabulate(n)(x => 0)
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
    println(product(selected_lines))
}