import java.io.File

fun main() {
    val tests = Tests()
    tests.testAll()
    
//    print("Input file name: ")
//    val filename: String? = readLine()
//    if (filename != null && File(filename).exists()) {
//        val str = File(filename).readText()
//        val prs = Parser(str)
//        val res = prs.parse()
//        if (res == null) {
//            println("This file is a correct prolog file")
//        } else {
//            println("Syntax error: line ${res.first}, colon ${res.second}")
//        }
//    } else {
//        println("Something is wrong with file name")
//    }
}