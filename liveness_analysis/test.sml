CM.make "sources.cm";

fun test () =
    let fun prependFolder filename = "../testcases/" ^ filename
        fun testFiles n =
            let val testFile = "test" ^ Int.toString n ^ ".tig"
            in
                if n < 50
                then testFile::(testFiles (n+1))
                else []
            end
        fun run filename = (
            print (filename ^ "\n");
            Main.compile (prependFolder filename);
            print "\n"
        )
    in
        app run ((testFiles 1) @ ["merge.tig", "queens.tig"])
    end
