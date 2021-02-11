import io.Source.fromFile
import java.lang.management.ManagementFactory
import math.{pow, abs}

object a extends App {
	case class Position(x: Int, y: Int, direction: Char)


	def change_direction(current: Char, amount: Int): Char ={
		val directions: Array[Char] = Array('N', 'E', 'S', 'W')
		return directions((directions.indexOf(current) + amount/90) % 4)
	}
	def update_position(pos: Position, command: String): Position ={
		val operation: Char = command(0)
		val operand: Int = command.substring(1, command.length()).toInt
		if (operation == 'N') {
			return pos.copy(y = pos.y + operand)
		}
		if (operation == 'S') {
			return pos.copy(y = pos.y - operand)
		}
		if (operation == 'E') {
			return pos.copy(x = pos.x + operand)
		}
		if (operation == 'W') {
			return pos.copy(x = pos.x - operand)
		}
		if (operation == 'L') {
			return pos.copy(direction = change_direction(pos.direction, 360 - operand))
		}
		if (operation == 'R') {
			return pos.copy(direction = change_direction(pos.direction, operand))
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

	val origin: Position = Position(0, 0, 'E')

	val final_pos: Position = fromFile("input.txt").getLines.foldLeft(origin)(update_position(_,_))
	val answer: Int = calc_distance(final_pos, origin)
	val end = timer.getCurrentThreadCpuTime()

	println(final_pos.x)
	println(final_pos.y)
	println(final_pos.direction)
	println(answer)

	println(s"Took: ${end-start} nanoseconds, that's ${(end-start)/pow(10,9)} seconds")
}
