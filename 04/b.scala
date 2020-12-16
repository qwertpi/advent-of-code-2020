import io.Source.fromFile
import java.lang.management.ManagementFactory
import math.pow
import java.lang.{Integer, NumberFormatException}

object b extends App {
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
        def check_height(h: String): Boolean ={
            val unit: String = h.substring(h.length - 2)
            if ((unit == "cm") || (unit == "in")){
                val amount: Int = h.substring(0, h.length - 2).toInt
                return (((unit == "cm") && (amount >= 150) && (amount <= 193)) || ((unit == "in") && (amount >= 59) && (amount <= 76)))
            }
            else{
                return false
            }
        }
        def check_hair(h: String): Boolean = {
            try{
                Integer.parseInt(h.substring(1), 16)
                return true
            }
            catch {
                case e: NumberFormatException => return false
            }
        }
        def check_passport_id(i: String): Boolean ={
            try{
                i.toInt
                return true
            }
            catch{case e: NumberFormatException => return false}
        }

        def basic_check(p: Passport): Boolean ={
            return (p.num_fields == 8)||((p.num_fields == 7) && (p.country_id == ""))
        }
        if (basic_check(passport)) return  ((1920 to 2002).contains(passport.birth_year) && (2010 to 2020).contains(passport.issue_year) && (2020 to 2030).contains(passport.expiration_year) && check_height(passport.height) && check_hair(passport.hair_colour) && (passport.hair_colour(0) == '#') && ("amb", "blu", "brn", "gry", "grn", "hzl", "oth").productIterator.toList.contains(passport.eye_color) && (passport.passport_id.length == 9) && check_passport_id(passport.passport_id)) else return false
    }

    // two newline characters in a row signify the start of a new passport/
    // a single new line is just the same continued on a new line so can be safely ignored
    val answer: Int = fromFile("input.txt").mkString("").split("\n\n").toArray.map(x => new Passport(x.replace("\n", " ").split(" "))).filter(x => check(x)).length
    val end = timer.getCurrentThreadCpuTime()
    
    println(answer)
	println(s"Took: ${end-start} nanoseconds, that's ${(end-start)/pow(10,9)} seconds")
}
