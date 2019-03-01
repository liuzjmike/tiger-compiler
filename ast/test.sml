use "prabsyn.sml";

fun test filename =
    let val output = TextIO.openOut filename
        fun f n =
            let val input = "../testcases/test" ^ Int.toString n ^ ".tig"
            in
                TextIO.output(output, "test " ^ Int.toString n ^ "\n");
                PrintAbsyn.print (output, Parse.parse input);
                TextIO.output(output, "\n");
                if n < 49 then f (n+1) else ()
            end
    in
        f 1;
        TextIO.closeOut output
    end
