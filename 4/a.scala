import io.Source.fromFile
import java.lang.management.ManagementFactory
import math.pow

object a extends App {
    val timer = ManagementFactory.getThreadMXBean()
    val start = timer.getCurrentThreadCpuTime()

    class Passport(details: Array[String]) {
        def extract(search_term: String): String ={
            try {return details.filter(x => (x.substring(0,3) == search_term)).mkString("").split(":")(1)}
            catch {case e: ArrayIndexOutOfBoundsException => return ""}
        }
        
        val birth_year: Int = try{extract("byr").toInt} catch{case e: NumberFormatException => 0}
        val issue_year: Int = try{extract("iyr").toInt} catch{case e: NumberFormatException => 0}
        val expiration_year: Int = try{extract("eyr").toInt} catch{case e: NumberFormatException => 0}
        val height: String = extract("hgt")
        val hair_colour: String = extract("hcl")
        val eye_color: String = extract("ecl")
        val passport_id: String = extract("pid")
        val country_id: String = extract("cid")
        val num_fields: Int = details.length
    }

    def check(passport: Passport): Boolean ={
        return (passport.num_fields == 8)||((passport.num_fields == 7) && (passport.country_id == ""))
    }
    // two newline characters in a row signify the start of a new passport/
    // a single new line is just the same continued on a new line so can be safely ignored
    val answer: Int = fromFile("input.txt").mkString("").split("\n\n").toArray.map(x => new Passport(x.replace("\n", " ").split(" "))).filter(x => check(x)).length
    
    val end = timer.getCurrentThreadCpuTime()
    println(answer)
	println(s"Took: ${end-start} nanoseconds, that's ${(end-start)/pow(10,9)} seconds")
}
