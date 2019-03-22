structure Translate : TRANSLATE =
struct
    datatype level = Level of {parent: level, frame: Frame.frame}
                   | Outermost
    type access = level * Frame.access

    val outermost = Outermost

    fun newLevel {parent, name, formals} = Level {
        parent=parent,
        frame=Frame.newFrame {name=name, formals=true::formals}
    }

    fun formals level = (
        case level
        of  Level {parent, frame} => map (fn a => (level, a)) (Frame.formals frame)
        |   Outermost => ErrorMsg.impossible "access formals of the outermost level"
    )

    fun allocLocal level escape = (
        case level
        of  Level {parent, frame} => (level, Frame.allocLocal frame escape)
        |   Outermost => ErrorMsg.impossible "allocate local variable in the outermost level"
    )
end