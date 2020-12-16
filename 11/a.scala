import io.Source.fromFile
import java.lang.management.ManagementFactory
import math.pow

object a extends App {
    def apply_rules(floor: Array[Array[Char]]): Array[Array[Char]] ={
        def get_adjecent(row: Int, column: Int): Array[Char] ={
            var adjacent: Array[Char] = new Array[Char](8).map(x => ' ')

            val look_right: Boolean = (column != (floor_width - 1))
            val look_left: Boolean = (column != 0)
            val look_up: Boolean = (row != 0)
            val look_down: Boolean = (row != (floor_height - 1))

            adjacent(0) = if (look_right) floor(row)(column+1) else ' '
            adjacent(1) =  if (look_left) floor(row)(column - 1) else ' '
            adjacent(2) = if (look_up) floor(row - 1)(column) else ' '
            adjacent(3) = if (look_down) floor(row + 1)(column) else ' '
            adjacent(4) = if (look_left && look_up) floor(row - 1)(column - 1) else ' '
            adjacent(5) = if (look_left && look_down) floor(row + 1)(column - 1) else ' '
            adjacent(6) = if (look_right && look_up) floor(row - 1)(column + 1) else ' '
            adjacent(7) = if (look_right && look_down) floor(row + 1)(column + 1) else ' '

            return adjacent.filter((x => x != ' '))
        }    

        var new_floor: Array[Array[Char]] = Array.ofDim[Char](floor_height, floor_width)
        for (row <- (0 until floor_height)) {
            for (column <- (0 until floor_width)) {
                if (floor(row)(column) != '.') {
                    var adjacent: Array[Char] = get_adjecent(row, column)
                    new_floor(row)(column) = if (!(adjacent.contains('#'))) '#' else if (adjacent.count(x => x == '#') >= 4) 'L' else floor(row)(column)
                }
                else {
                    new_floor(row)(column) = floor(row)(column)
                } 
            }
        }
        return new_floor
    }


    val timer = ManagementFactory.getThreadMXBean()
    val start = timer.getCurrentThreadCpuTime()

    var floor: Array[Array[Char]] = fromFile("input.txt").getLines.map(x => x.toCharArray()).toArray
    val floor_width: Int = floor(0).length
    val floor_height: Int = floor.length

    //deep-copies floor
    var new_floor:  Array[Array[Char]] = floor.transpose.transpose
    var first: Boolean = true

    //comparing multidimensional arrays in Scala is surprisingly hard
    while ((!(floor.flatten[Char].sameElements(new_floor.flatten[Char])))||(first)) {
        first = false
        floor = new_floor.transpose.transpose
        new_floor = apply_rules(floor)
    }

    val answer: Int = floor.map(x => x.count(a => a == '#')).sum
    val end = timer.getCurrentThreadCpuTime()
    println(answer)

	println(s"Took: ${end-start} nanoseconds, that's ${(end-start)/pow(10,9)} seconds")
}
