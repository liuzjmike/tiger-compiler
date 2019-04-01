fun test filename =
    let val output = TextIO.openOut filename
        fun writeFrag frag =
            case frag
            of  MipsFrame.PROC {body, frame} => (
                TextIO.output (output, Symbol.name (MipsFrame.name frame) ^ "\n");
                Printtree.printtree (output, body)
            )
            |   _ => ()
        fun f n =
            let val input = "../testcases/test" ^ Int.toString n ^ ".tig"
                val testStr = "test " ^ Int.toString n ^ "\n"
            in
                if n < 49
                then (
                    print (testStr);
                    Main.main input;
                    (print "\n"; f (n+1))
                )
                else (
                    app writeFrag (Main.main input);
                    ()
                )
            end
    in
        f 1;
        TextIO.closeOut output
    end
