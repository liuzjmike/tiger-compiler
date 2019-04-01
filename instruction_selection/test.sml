CM.make "sources.cm";

fun useSilently s =
    let val saved = !Control.Print.out
        fun done () = Control.Print.out := saved
    in
        Control.Print.out := {say=fn _ => (), flush=fn () => ()};
        use s;
        done ()
    handle _ => done ()
end

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
        app run (["merge.tig", "queens.tig"] @ (testFiles 1))
    end
