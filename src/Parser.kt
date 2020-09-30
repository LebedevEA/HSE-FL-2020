import java.lang.Integer.min

class Parser constructor(private var str: String) {
    private var pos: Int = 0
    private var lastSuccess: Int = -1

    fun parse(): Boolean {
        skipWhitespace()
        if (!parseP()) {
            printPosInFile(lastSuccess)
            return false
        }
        skipWhitespace()
        return pos == str.length
    }

    private fun printPosInFile(p: Int) {
        var lineNo = 1
        var linePos = 1
        for (i in 0 until p) {
            if (str[i] == '\n') {
                lineNo++
                linePos = 1
            } else {
                linePos++
            }
        }
        println("Syntax error: line $lineNo, colon $linePos")
    }

    private fun parseP(): Boolean {
        val initPos = pos
        if (!parseDR()) {
            pos = initPos
            return false
        }
        skipWhitespace()
        if (pos != str.length) {
            return if (parseP()) {
                lastSuccess = pos
                true
            } else {
                pos = initPos
                false
            }
        }
        lastSuccess = pos
        return true
    }

    private fun parseDR(): Boolean {
        val initPos = pos
        if (!parseH() || !parseSymb('.')) {
            pos = initPos
            return if (parseH() && parseCorkscrew() && parseB() && parseSymb('.')) {
                lastSuccess = pos
                true
            } else {
                pos = initPos
                false
            }
        }
        lastSuccess = pos
        return true
    }

    private fun parseH() = parseID()
    private fun parseB() = parseD()

    private fun parseDKL(
        parseItem: () -> Boolean,
        parseSep: () -> Boolean
    ): Boolean {
        val initPos = pos
        if (!parseItem() || !parseSep() || !parseDKL(parseItem, parseSep)) {
            pos = initPos
            if (!parseItem()) {
                pos = initPos
                return false
            }
        }
        skipWhitespace()
        lastSuccess = pos
        return true
    }

    private fun parseD() = parseDKL({ parseK() }, { parseSymb(';') })
    private fun parseK() = parseDKL({ parseL() }, { parseSymb(',') })

    private fun parseL(): Boolean {
        val initPos = pos
        if (!parseID()) {
            pos = initPos
            if (!parseSymb('(') || !parseD() || !parseSymb(')')) {
                pos = initPos
                return false
            }
        }
        skipWhitespace()
        lastSuccess = pos
        return true
    }

    private fun parseSymb(ch: Char): Boolean {
        val initPos = pos
        if (pos < str.length && str[pos++] == ch) {
            skipWhitespace()
            lastSuccess = pos
            return true
        }
        pos = initPos
        return false
    }

    private fun parseCorkscrew(): Boolean {
        val initPos = pos
        skipWhitespace()
        val corkscrew = ":-".toRegex()
        val found = corkscrew.find(str.subSequence(pos, min(pos + 2, str.length)))
        if (found != null) {
            pos += found.range.last + 1
            skipWhitespace()
            lastSuccess = pos
            return true
        }
        pos = initPos
        return false
    }

    private fun parseID(): Boolean {
        val initPos = pos
        val id = "[a-zA-Z_][0-9a-zA-Z_]*".toRegex()
        val found = id.find(str, pos)
        if (found != null && found.range.first - pos == 0) {
            pos = found.range.last + 1
            skipWhitespace()
            lastSuccess = pos
            return true
        }
        pos = initPos
        return false
    }

    private fun skipWhitespace() {
        val whitespaces = arrayOf(' ', '\n', '\t')
        while (pos < str.length && whitespaces.contains(str[pos]))
            pos++
    }

}