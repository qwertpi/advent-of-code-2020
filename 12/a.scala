import io.Source.fromFile
import java.lang.management.ManagementFactory
import math.{pow, abs}

object a extends App {
	class Position(val x: Int, val y: Int, val direction: Char)


	def change_direction(current: Char, amount: Int): Char ={
		val directions: Array[Char] = Array('N', 'E', 'S', 'W')
		return directions((directions.indexOf(current) + amount/90) % 4)
	}
	def update_position(pos: Position, command: String): Position ={
		val operation: Char = command(0)
		val operand: Int = command.substring(1, command.length()).toInt
		if (operation == 'N') {
			return new Position(pos.x, pos.y + operand, pos.direction)
		}
		if (operation == 'S') {
			return new Position(pos.x, pos.y - operand, pos.direction)
		}
		if (operation == 'E') {
			return new Position(pos.x + operand, pos.y, pos.direction)
		}
		if (operation == 'W') {
			return new Position(pos.x - operand, pos.y, pos.direction)
		}
		if (operation == 'L') {
			return new Position(pos.x, pos.y, change_direction(pos.direction, 360 - operand))
		}
		if (operation == 'R') {
			return new Position(pos.x, pos.y, change_direction(pos.direction, operand))
		}
		if (operation == 'F') {
			return update_position(pos, pos.direction.toString + operand.toString)
		}
		else {
			return ???
		}
	}

	def calc_distance(pos2: Position, pos1: Position): Int ={
		abs(pos2.x - pos1.x) + abs(pos2.y - pos1.y)
	}

	val timer = ManagementFactory.getThreadMXBean()
	val start = timer.getCurrentThreadCpuTime()

	val origin: Position = new Position(0, 0, 'E')
	val input: Array[String] = fromFile("input.txt").getLines.toArray

	val final_pos: Position = input.foldLeft(origin)(update_position(_,_))
	val answer: Int = calc_distance(final_pos, origin)
	val end = timer.getCurrentThreadCpuTime()
	
	println(final_pos.x)
	println(final_pos.y)
	println(final_pos.direction)
	println(answer)

	println(s"Took: ${end-start} nanoseconds, that's ${(end-start)/pow(10,9)} seconds")
}
