import io.Source.fromFile
import java.lang.management.ManagementFactory
import collection.mutable.LinkedHashSet

import math.pow

object b extends App {
    def apply_rules(floor: Array[Array[Char]]): Array[Array[Char]] ={
        def get_occupied(row: Int, column: Int): Array[Boolean] ={
            var occupied: Array[Boolean] = new Array[Boolean](8).map(_ => false)

            
            val look_right: LinkedHashSet[Int] = LinkedHashSet((1 until (floor_width - column)): _*)
            val look_left: LinkedHashSet[Int] = LinkedHashSet((1 to column): _*)
            val look_down: LinkedHashSet[Int] = LinkedHashSet((1 until (floor_height - row)): _*)
            val look_up: LinkedHashSet[Int] = LinkedHashSet((1 to row): _*)

            def check(m: LinkedHashSet[Char]): Boolean ={
                return m.find(_ != '.').getOrElse(None)  == '#'
            }

            occupied(0) = check(look_right.map(n => floor(row)(column + n)))
            occupied(1) = check(look_left.map(n => floor(row)(column - n)))
            occupied(2) = check(look_up.map(n => floor(row - n)(column)))
            occupied(3) = check(look_down.map(n => floor(row + n)(column)))
            occupied(4) = check(look_up.intersect(look_left).map(n => floor(row - n)(column - n)))
            occupied(5) = check(look_down.intersect(look_left).map(n => floor(row + n)(column - n)))
            occupied(6) = check(look_up.intersect(look_right).map(n => floor(row - n)(column + n)))
            occupied(7) = check(look_down.intersect(look_right).map(n => floor(row + n)(column + n)))
            return occupied
        }    

        var new_floor: Array[Array[Char]] = Array.ofDim[Char](floor_height, floor_width)
        for (row <- (0 until floor_height)) {
            for (column <- (0 until floor_width)) {
                if (floor(row)(column) != '.') {
                    val adjacent: Array[Boolean] = get_occupied(row, column)
                    new_floor(row)(column) = if (!(adjacent.contains(true))) '#' else if (adjacent.count(_ == true) >= 5) 'L' else floor(row)(column)
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

    /*
    def show_floor: Unit = {
        for (row <- floor) {
            for (seat <- row){
                print(seat)
            }
            println("")
        }
        println("")
    }
    */

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
