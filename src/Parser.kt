import java.lang.Integer.min

class Parser constructor(private var str: String) {
    /*
     * P = program
     * DR = def or relation
     * H = head
     * B = body
     * D = disjunction
     * K = conjunction
     * L = literal | id
     *
     * P  -> DR P | eps
     * DR -> H .  | H :- B .
     * H  -> id
     * B  -> D
     * D  -> (K;)* K
     * K  -> (L,)* L
     * L  -> id   | '(' D ')'
     */
    private var pos: Int = 0

    fun parse(): Boolean {
        skipWhitespace()
        if (!parseP()) return false
        skipWhitespace()
        return pos == str.length
    }

    private fun parseP(): Boolean {
        if (!parseDR()) {
            return false
        }
        skipWhitespace()
        if (pos != str.length) {
            return parseP()
        }
        return true
    }

    private fun parseDR(): Boolean {
        val initPos = pos
        if (!parseH() || !parseSymb('.')) {
            pos = initPos
            return parseH() && parseCorkscrew() && parseB() && parseSymb('.')
        }
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
                skipWhitespace()
                return false
            }
        }
        skipWhitespace()
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
        return true
    }

    private fun parseSymb(ch: Char): Boolean {
        val initPos = pos
        if (pos < str.length && str[pos++] == ch) {
            skipWhitespace()
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
