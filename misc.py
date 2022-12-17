from __future__ import annotations
from ast_tools import *
from typing import List
from parser_1 import Parser
from lexer import Lexer

class Env:
    def __init__(self, p : Env):
        self.hashTable = {}
        self.prev = p 
    
    def put(self, var_name: str):
        self.hashTable[var_name]  = None
    
    def exists(self, var_name: str):
        e: Env = self
        while(e != None):
            if(var_name in e.hashTable):
                return True
            e = e.prev
        return False
    def existsInCurrent(self, var_name: str):
        return var_name in self.hashTable
# this is a stupid thing but i was short on time couldn't figure out any other way
class Finder(ABC):
    def __init__(self):
        self.ASTNodes = {
            SLiteral: self.visit_SLiteral,
            Program : self.visit_Program,
            ErrorStmt: self.visit_ErrorStmt,
            VarDecl: self.visit_VarDecl,
            FunDecl: self.visit_FunDecl,
            Assign: self.visit_Assign,
            SetVector: self.visit_SetVector,
            ForLoop: self.visit_ForLoop,
            Return: self.visit_Return,
            WhileLoop: self.visit_WhileLoop,
            Block: self.visit_Block,
            Print: self.visit_Print,
            IfElse: self.visit_IfElse,
            LBinary: self.visit_LBinary,
            Comparison: self.visit_Comparison,
            LLiteral: self.visit_LLiteral,
            LPrimary: self.visit_LPrimary,
            GetVector: self.visit_GetVector,
            Variable: self.visit_Variable,
            LNot: self.visit_LNot,
            ABinary: self.visit_ABinary,
            AUMinus: self.visit_AUMinus,
            ALiteral: self.visit_ALiteral,
            Call: self.visit_Call
        }

    def visit(self, ast_node: ASTNode, e: Env):
        return self.ASTNodes[type(ast_node)](ast_node, e)
    @abstractmethod
    def visit_SLiteral(self, sliteral: SLiteral, e: Env):
        pass

    @abstractmethod
    def visit_Program(self, program: Program, e: Env):
        pass

    @abstractmethod
    def visit_ErrorStmt(self, errorstmt: ErrorStmt, e: Env):
        pass

    @abstractmethod
    def visit_VarDecl(self, vardecl: VarDecl, e: Env):
        pass

    @abstractmethod
    def visit_FunDecl(self, fundecl: FunDecl, e: Env):
        pass

    @abstractmethod
    def visit_Assign(self, assign: Assign, e: Env):
        pass

    @abstractmethod
    def visit_SetVector(self, setvector: SetVector, e: Env):
        pass

    @abstractmethod
    def visit_ForLoop(self, forloop: ForLoop, e: Env):
        pass

    @abstractmethod
    def visit_Return(self, returnn: Return, e: Env):
        pass

    @abstractmethod
    def visit_WhileLoop(self, whileloop: WhileLoop, e: Env):
        pass

    @abstractmethod
    def visit_Block(self, block: Block, e: Env):
        pass

    @abstractmethod
    def visit_Print(self, printt: Print, e: Env):
        pass

    @abstractmethod
    def visit_IfElse(self, ifelse: IfElse, e: Env):
        pass

    @abstractmethod
    def visit_LBinary(self, lbinary: LBinary, e: Env):
        pass

    @abstractmethod
    def visit_Comparison(self, comparison: Comparison, e: Env):
        pass

    @abstractmethod
    def visit_LLiteral(self, lliteral: LLiteral, e: Env):
        pass

    @abstractmethod
    def visit_LPrimary(self, lprimary: LPrimary, e: Env):
        pass

    @abstractmethod
    def visit_GetVector(self, getvector: GetVector, e: Env):
        pass

    @abstractmethod
    def visit_Variable(self, variable: Variable, e: Env):
        pass

    @abstractmethod
    def visit_LNot(self, lnot: LNot, e: Env):
        pass

    @abstractmethod
    def visit_ABinary(self, abinary: ABinary, e: Env):
        pass

    @abstractmethod
    def visit_AUMinus(self, auminus: AUMinus, e: Env):
        pass

    @abstractmethod
    def visit_ALiteral(self, aliteral: ALiteral, e: Env):
        pass

    @abstractmethod
    def visit_Call(self, calll: Call, e: Env):
        pass

class Translate(Finder):
    def __init__(self):
        super().__init__()
        self.undeclared: List[Identifier] = []
        self.multi_declared : List[Identifier] = []

            
    def get_multidecl(self):
        return self.multi_declared
    def get_undecl(self):
        return self.undeclared
    
    def visit_Program(self, program: Program, e:Env):
        globals: Env = Env(None)
        # add global declarations into global scope
        for elem in program.var_decls:
            self.visit(elem, globals)
        for elem in program.fun_decls:
            self.visit(elem, globals)
        for elem in program.statements:
            self.visit(elem, globals)

    def visit_SLiteral(self, sliteral: SLiteral, e: Env):
        pass
    def visit_ErrorStmt(self, errorstmt: ErrorStmt, e: Env):
        pass

    def visit_VarDecl(self, vardecl: VarDecl, e: Env):
        if vardecl.initializer == None:
            if(e.existsInCurrent(vardecl.identifier.name) == False):
                e.put(vardecl.identifier.name)
            else:
                self.multi_declared.append(vardecl.identifier)
        elif type(vardecl.initializer) == list:
            if(e.existsInCurrent(vardecl.identifier.name) == False):
                e.put(vardecl.identifier.name)
            for elem in vardecl.initializer:
                self.visit(elem, e)
        else:
            if(e.existsInCurrent(vardecl.identifier.name) == False):
                e.put(vardecl.identifier.name)
            self.visit(vardecl.initializer, e)
            

    def visit_FunDecl(self, fundecl: FunDecl, e: Env):
        new_e = Env(e)

        for param in fundecl.params:
            if new_e.existsInCurrent(param.name):
                self.multi_declared.append(param)
                continue
            new_e.put(param.name)
        
        self.visit(fundecl.body, new_e)

    def visit_Assign(self, assign: Assign, e: Env):
        if(e.exists(assign.identifier.name) == False):
            self.undeclared.append(assign.identifier)
        self.visit(assign.expr, e)

    def visit_SetVector(self, setvector: SetVector, e: Env):
        if(e.exists(setvector.identifier.name) == False):
            self.undeclared.append(setvector.identifier)
        self.visit(setvector.expr, e)
        self.visit(setvector.vector_index, e)

    def visit_ForLoop(self, forloop: ForLoop, e: Env):
        if forloop.condition != None:
            self.visit(forloop.condition, e)
        if forloop.increment != None:
            self.visit(forloop.increment, e)
        if forloop.initializer != None:
            self.visit(forloop.initializer, e)
        new_e = Env(e)

        self.visit(forloop.body, new_e)

    def visit_Return(self, returnn: Return, e: Env):
        self.visit(returnn.expr, e)

    def visit_WhileLoop(self, whileloop: WhileLoop, e: Env):
        self.visit(whileloop.condition, e)
        new_e = Env(e)
        self.visit(whileloop.body, new_e)


    def visit_Block(self, block: Block, e: Env):
        for elem in block.var_decls:
            self.visit(elem, e)
        for stmt in block.statements:
            self.visit(stmt, e)

    def visit_Print(self, printt: Print, e: Env):
        self.visit(printt.expr, e)

    def visit_IfElse(self, ifelse: IfElse, e: Env):
        self.visit(ifelse.condition, e)
        e1 = Env(e)
        self.visit(ifelse.if_branch, e1)
        if ifelse.else_branch != None:
            e2 = Env(e)
            self.visit(ifelse.else_branch, e2)

    def visit_LBinary(self, lbinary: LBinary, e: Env):
        self.visit(lbinary.left, e)
        self.visit(lbinary.right, e)

    def visit_Comparison(self, comparison: Comparison, e: Env):
        self.visit(comparison.left, e)
        self.visit(comparison.right, e)

    def visit_LLiteral(self, lliteral: LLiteral, e: Env):
        pass

    def visit_LPrimary(self, lprimary: LPrimary, e: Env):
        self.visit(lprimary.primary, e)

    def visit_GetVector(self, getvector: GetVector, e: Env):
        if(e.exists(getvector.identifier.name) == False):
            self.undeclared.append(getvector.identifier)
        self.visit(getvector.vector_index, e)

    def visit_Variable(self, variable: Variable, e: Env):
        if(e.exists(variable.identifier.name) == False):
            self.undeclared.append(variable.identifier)


    def visit_LNot(self, lnot: LNot, e: Env):
        self.visit(lnot.right, e)
    def visit_ABinary(self, abinary: ABinary, e: Env):
        self.visit(abinary.left, e)
        self.visit(abinary.right, e)

    def visit_AUMinus(self, auminus: AUMinus, e: Env):
        self.visit(auminus.right, e)
    def visit_ALiteral(self, aliteral: ALiteral, e: Env):
        pass
    def visit_Call(self, calll: Call, e: Env):
        # if(e.exists(calll.callee.name) == False):
        #     if calll.callee.name not in self.undeclared:
        #         self.undeclared.append(calll.callee.name)
        for arg in calll.arguments:
            self.visit(arg, e)

def process(source):
    '''parse the source text here. you may return the AST specified in ast_tools.py or something else.'''
    l = Lexer()
    p = Parser()
    return p.parse(l.tokenize(source))

def generate_ast(intermediate) -> Program:
    '''return the AST using the output of process() here.'''
    return intermediate

def undeclared_vars(intermediate) -> List[Identifier]:
    '''return all of the undeclared uses of the variables in the order they appear in the source code here, using the return value of process()'''
    t = Translate()
    t.visit(generate_ast(intermediate), None)
    return t.get_undecl()

def multiple_var_declarations(intermediate) -> List[Identifier]:
    '''return all of the subsequent declarations of a previously declared variable if the re-declaration cannot be explained by shadowing,
    in the order they appear in the source code, using the return value of process()'''
    t = Translate()
    t.visit(generate_ast(intermediate), None)
    return t.get_multidecl()
