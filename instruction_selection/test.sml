CM.make "sources.cm";

fun test () =
    let fun prependFolder filename = "../testcases/" ^ filename
        fun testFiles n =
            let val testFile = "test" ^ Int.toString n ^ ".tig"
            in
                if n < 49
                then testFile::(testFiles (n+1))
                else []
            end
        fun run filename = (
            print (filename ^ "\n");
            Main.compile (prependFolder filename)
        )
    in
        app run (["merge.tig", "queens.tig"] @ (testFiles 1))
    end
