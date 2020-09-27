import java.io.File

fun main() {
    print("Input file name: ")
    val filename: String? = readLine()
    if (filename != null && File(filename).exists()) {
        val str = File(filename).readText()
        val prs = Parser(str)
        println("The file is${if (prs.parse()) "" else " not"} a correct prolog file")
    } else {
        println("Check file name")
    }
}
