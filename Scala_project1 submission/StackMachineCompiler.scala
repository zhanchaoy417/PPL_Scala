package edu.colorado.csci3155.project1

object StackMachineCompiler {



    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stackmachine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        e match {
            case Const(f) => PushIns(f)::Nil
            case Ident(id)=> StoreIns(id)::Nil
            case Plus(e1, e2)=>compileToStackMachineCode(e1):::compileToStackMachineCode(e2):::List(AddIns)
            case Minus(e1, e2)=>compileToStackMachineCode(e1):::compileToStackMachineCode(e2):::List(SubIns)
            case Mult(e1, e2)=>compileToStackMachineCode(e1):::compileToStackMachineCode(e2):::List(MultIns)
            case Div(e1, e2)=>compileToStackMachineCode(e1):::compileToStackMachineCode(e2):::List(DivIns)
            case Log(e)=>compileToStackMachineCode(e) :+ LogIns
            case Exp(e)=>compileToStackMachineCode(e) :+ ExpIns
            case Sine(e)=>compileToStackMachineCode(e) :+ SinIns
            case Cosine(e)=>compileToStackMachineCode(e):+ CosIns
            case Let(ident, e1, e2)=>compileToStackMachineCode(e1):::List(LoadIns(ident)):::compileToStackMachineCode(e2)
        }// TODO: Implement this
    }
}
