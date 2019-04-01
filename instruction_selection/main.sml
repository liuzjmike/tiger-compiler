structure Main =
struct

  structure F = MipsFrame
  structure Tr = Translate (MipsFrame)
  structure Semant = Semant (Tr)
  (* structure R = RegAlloc *)

  fun tempName t =
    case Temp.Table.look (F.tempMap, t) of
        SOME name => name
      | NONE => Temp.makestring t

  fun emitproc out (F.PROC{body,frame}) =
      let
        val _ = print ("emit " ^ F.name frame ^ "\n")
        (* val _ = Printtree.printtree(out,body); *)
        val stms = Canon.linearize body
        val _ = app (fn s => Printtree.printtree(out,s)) stms;
        val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
        val instrs = List.concat(map (MipsGen.codegen frame) stms') 
        val format0 = Assem.format(tempName)
      in app (fn i => TextIO.output(out,format0 i)) instrs
      end
    | emitproc out (F.STRING (lab,s)) = TextIO.output(out,F.string(lab,s))

  fun withOpenFile fname f = 
      let val out = TextIO.openOut fname
      in (f out before TextIO.closeOut out) 
      handle e => (TextIO.closeOut out; raise e)
      end

  fun compile filename = 
      let
        val absyn = Parse.parse filename
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
      in 
        withOpenFile (filename ^ ".s") (fn out => (app (emitproc out) frags))
      end

end
