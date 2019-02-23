fun test () =
    let fun f n =
            let val input = "../testcases/test" ^ Int.toString n ^ ".tig"
            in
                print ("test " ^ Int.toString n ^ "\n");
                Main.main input;
                if n < 49 then (print "\n"; f (n+1)) else ()
            end
    in
        f 1
    end
