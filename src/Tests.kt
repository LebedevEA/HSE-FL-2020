
class Tests {
    fun testAll() {
        for (i in correctTests.indices) {
            correctTest(correctTests[i], i)
        }
        for (i in incorrectTests.indices) {
            incorrectTest(incorrectTests[i], i)
        }
        if (errors.isEmpty()) {
            println("All test have been successfully passed!")
        } else {
            println("Failed ${errors.size} of ${correctTests.size + incorrectTests.size} tests")
            for (str in errors) {
                println(str)
            }
        }
    }

    private fun correctTest(str: String, i: Int) = makeTest(str, i, true)
    private fun incorrectTest(str: String, i: Int) = makeTest(str, i, false)

    private fun makeTest(str: String, i: Int, isCorrect: Boolean) {
        val parser = Parser(str)
        if (when (isCorrect) {
                    true -> parser.parse() != null
                    false -> parser.parse() == null
                }) {
            errors += "correctTest #$i did not pass: \"$str\""
        }
    }
    
    private var errors = arrayOf<String>()
    private val correctTests = arrayOf(
            "f :- a, b, c; d.",
            " Afasf :- _HL_AlcdckscNLKDn__As, b; c, d. ",
            "    AAAAAAAA           :- (AAAA, bbbb; (ddd, _AA__a_),        a; (b,c) )  .         ",
            " B :- B. ",
            " A. ",
            " __A            .     ",
            " CORRECT :- L,I;N,E.",
            "a:-a,b,c,d,e,f,g,h,i,j,k,l,m,n,zxc.",
            "f :- a, b;    aa; ____Hellow_W0rlD__, (weoig, uhsuah_sd, sdvsv;euirgei).\n" +
                    "sdjbvldbvljsdvjb :- f,f,f,f,f, f,f, f; f, f.\n" +
                    "    f:-  f , f ; f ; f ; (f; f),\n" +
                    "\n" +
                    "     DVDLV, IUFVSKD  .\n" +
                    "This_is :- correct, (__almostProlog; file).",
            ""
    )
    private val incorrectTests = arrayOf(
            ".",  ",.", ";.", "..", "().",
            "f", "f:.", "ff:-", ":-.", "fff", "f:-a,().",
            "f :- g, h;  .", "f :- g \n, h, \n.",
            "f :-  .", "f :- (a, (b; c)."
    )
}
