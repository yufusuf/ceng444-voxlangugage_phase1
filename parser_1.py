import sly
import lexer
from ast_tools import *
import sys
from misc import *

class Parser(sly.Parser):
    tokens = lexer.Lexer.tokens
    debugfile = 'parser_debug.out'

    # PROGRAM
    @_('V F S')
    def program(self, p):
        return Program(p.V, p.F, p.S)

    # VARIABLE DECLARATIONS
    @_('V varDecl')
    def V(self, p):
        p[0].append(p[1])
        return p[0]

    @_('empty')
    def V(self, p):
        return []

    # variable declaration without definition
    @_('VAR ID ";"')
    def varDecl(self, p):
        return VarDecl(Identifier(p.ID, p.lineno, p.index), None)

    # variable declaration with definiton
    @_('VAR ID ASSIGN expr ";" ')
    def varDecl(self, p):
        return VarDecl(Identifier(p.ID, p.lineno, p.index), p.expr)

    # vector declaration
    @_('VAR ID ASSIGN "[" Vexprs "]" ";"')
    def varDecl(self, p):
        return VarDecl(Identifier(p.ID, p.lineno, p.index), p.Vexprs)

    @_('Vexprs "," expr')
    def Vexprs(self, p):
        p.Vexprs.append(p.expr)
        return p.Vexprs

    @_('expr')
    def Vexprs(self, p):
        return [p.expr]
    # ====== VARIABLE DECLARATIONS

    # FUNCTION DECLARATIONS

    @_('F funDecl')
    def F(self, p):
        p[0].append(p[1])
        return p[0]

    @_('empty')
    def F(self, p):
        return []

    @_('FUN ID "(" parameters ")" block')
    def funDecl(self, p):
        return FunDecl(Identifier(p.ID, p.lineno, p.index), p.parameters, p.block)

    @_('empty')
    def parameters(self, p):
        return []

    @_('P')
    def parameters(self, p):
        return  p.P

    @_('ID')
    def P(self, p):
        return [Identifier(p.ID, p.lineno, p.index)]

    @_('P "," ID')
    def P(self, p):
        p.P.append(Identifier(p.ID, p.lineno, p.index))
        return p.P


    # ====== FUNCTION DECLARATIONS

    # BLOCK
    @_('"{" V  S "}"')
    def block(self, p):
        return Block(p.V, p.S)
    # ====== BLOCK

    # STATEMENTS
    @_('empty')
    def S(self, p):
        return []

    @_('S free_stmt')
    def S(self, p):
        p.S.append(p[1])
        return p.S

    @_('free_stmt')
    def stmt(self, p):
        return p.free_stmt

    @_('block')
    def stmt(self, p):
        return p.block

    @_('simple_stmt ";"')
    def free_stmt(self, p):
        return p.simple_stmt

    @_('ID ASSIGN expr')
    def asgn_stmt(self, p):
        return Assign(Identifier(p.ID, p.lineno, p.index), p.expr)

    @_('ID "[" aexpr "]" ASSIGN expr')
    def asgn_stmt(self, p):
        return SetVector(Identifier(p.ID, p.lineno, p.index), p.aexpr, p.expr)

    @_('asgn_stmt')
    def simple_stmt(self,p):
        return p.asgn_stmt

    @_('print_stmt')
    def simple_stmt(self, p):
        return p.print_stmt

    @_('PRINT expr')
    def print_stmt(self, p):
        return Print(p.expr)

    @_('return_stmt')
    def simple_stmt(self, p):
        return p.return_stmt

    @_('RETURN expr')
    def return_stmt(self, p):
        return Return(p.expr)

    @_('compound_stmt')
    def free_stmt(self, p):
        return p.compound_stmt

    # IF STATEMENT
    @_('if_stmt')
    def compound_stmt(self, p):
        return p.if_stmt

    @_('IF lexpr stmt')
    def if_stmt(self, p):
        return IfElse(p.lexpr, p.stmt, None)

    @_('IF lexpr stmt ELSE stmt')
    def if_stmt(self, p):
        return IfElse(p.lexpr, p.stmt0, p.stmt1)
    # ====== IF STATEMENT

    # WHILE STATEMENT
    @_('while_stmt')
    def compound_stmt(self, p):
        return p.while_stmt

    @_('WHILE lexpr stmt')
    def while_stmt(self, p):
        return WhileLoop(p.lexpr, p.stmt)

    # ====== WHILE STATEMENT

    # FOR STATEMENT
    @_('for_stmt')
    def compound_stmt(self, p):
        return p.for_stmt

    @_('FOR "(" for_asgn ";" for_lexpr ";" for_asgn ")" stmt')
    def for_stmt(self, p):
        return ForLoop(p.for_asgn0, p.for_lexpr, p.for_asgn1, p.stmt)

    @_('asgn_stmt')
    def for_asgn(self, p):
        return p.asgn_stmt

    @_('empty')
    def for_asgn(self, p):
        return None

    @_('lexpr')
    def for_lexpr(self, p):
        return p.lexpr

    @_('empty')
    def for_lexpr(self, p):
        return None
    # ====== FOR STATEMENT

    # EXPRESSIONS
    @_('lexpr')
    def expr(self, p):
        return p.lexpr

    @_('sexpr')
    def expr(self, p):
        return p.sexpr
    @_('STRING')
    def sexpr(self, p):
        return SLiteral(p.STRING)

    @_('aexpr')
    def expr(self, p):
        return p.aexpr

    # ====== EXPRESSIONS

    # ARITHMETIC EXPRESSIONS
    @_( 'aexpr PLUS term')
    def aexpr(self, p):
        return ABinary("+", p.aexpr, p.term)

    @_( 'aexpr MINUS term')
    def aexpr(self, p):
        return ABinary("-", p.aexpr, p.term)

    @_('term')
    def aexpr(self, p):
        return p.term

    @_('term DIVIDE fact')
    def term(self, p):
        return ABinary("/", p.term, p.fact)

    @_('term TIMES fact')
    def term(self, p):
        return ABinary("*", p.term, p.fact)

    @_('fact')
    def term(self, p):
        return p.fact

    @_('MINUS fact')
    def fact(self, p):
        return AUMinus(p.fact)

    @_('call')
    def fact(self, p):
        return p.call

    @_('NUMBER')
    def fact(self, p):
        return ALiteral(p.NUMBER)

    @_('"(" aexpr ")"')
    def fact(self, p):
        return p.aexpr

    @_('ID')
    def fact(self, p):
        return Variable(Identifier(p.ID, p.lineno, p.index))

    @_('ID "[" aexpr "]"')
    def fact(self, p):
        return GetVector(Identifier(p.ID, p.lineno, p.index), p.aexpr)


    @_( 'aexpr EQ aexpr')
    def cexpr(self, p):
        return Comparison("==", p.aexpr0, p.aexpr1)

    @_('aexpr NE aexpr')
    def cexpr(self, p):
        return Comparison("!=", p.aexpr0, p.aexpr1)

    @_('aexpr GT aexpr')
    def cexpr(self, p):
        return Comparison(">", p.aexpr0, p.aexpr1)

    @_('aexpr GE aexpr')
    def cexpr(self, p):
        return Comparison(">=", p.aexpr0, p.aexpr1)

    @_('aexpr LT aexpr')
    def cexpr(self, p):
        return Comparison("<", p.aexpr0, p.aexpr1)

    @_('aexpr LE aexpr')
    def cexpr(self, p):
        return Comparison("<=", p.aexpr0, p.aexpr1)
    # ====== ARITHMETIC EXPRESSIONS

    # LOGICAL EXPRESSIONS
    @_('lexpr OR lterm')
    def lexpr(self, p):
        return LBinary("or", p.lexpr, p.lterm)

    @_('lterm')
    def lexpr(self, p):
        return p.lterm

    @_('lterm AND lfact')
    def lterm(self, p):
        return LBinary("and", p.lterm, p.lfact)

    @_('lfact')
    def lterm(self, p):
        return p.lfact

    @_('cexpr')
    def lfact(self, p):
        return p.cexpr

    @_('"#" call')
    def lfact(self, p):
        return LPrimary(p.call)

    @_('"(" lexpr ")"')
    def lfact(self,p):
        return p.lexpr

    @_('"#" ID')
    def lfact(self, p):
        return LPrimary(Variable(Identifier(p.ID, p.lineno, p.index)))

    @_('"#" ID "[" aexpr "]"')
    def lfact(self, p):
        return LPrimary(GetVector(Identifier(p.ID, p.lineno, p.index), p.aexpr))

    @_('NOT lfact')
    def lfact(self, p):
        return LNot(p.lfact)

    @_('TRUE')
    def lfact(self, p):
        return LLiteral(p.TRUE)

    @_('FALSE')
    def lfact(self, p):
        return LLiteral(p.FALSE)
    # ====== LOGICAL EXPRESSIONS

    # CALL
    @_('ID "(" args ")" ')
    def call(self, p):
        return Call(Identifier(p.ID, p.lineno, p.index), p.args)

    @_('empty')
    def args(self, p):
        return []

    @_('A')
    def args(self, p):
        return p.A

    @_('expr')
    def A(self, p):
        return [p.expr]

    @_('A "," expr')
    def A(self, p):
        p.A.append(p.expr)
        return p.A

    # ====== CALL
    @_('')
    def empty(self, p):
        pass

if __name__ == '__main__':
    if len(sys.argv) < 2:
        #lxr = lexer.Lexer()
        #pars = Parser()
        while True:
            try:
                text  = input('> ')
                ast = generate_ast(process(text))
                t = Translate()
                v = PrintVisitor()
                print(v.visit_Program(ast))
                print(ast)
                t.visit_Program(ast, None)
                print("undeclared: ")
                print(t.get_undecl())
                print("multidecl: ")
                print(t.get_multidecl())
            except EOFError:
                break
        quit()
    test_number = sys.argv[1]
    test_path = "./parser_tests/test" + test_number + ".vox"
    f = open(test_path)
    source = f.read()
    #print("SOURCE: ")
    #print(source)

    ast = generate_ast(process(source))
    t = Translate()

    print(PrintVisitor().visit(ast))
    t.visit_Program(ast, None)
    print("undeclared: ")
    print(t.get_undecl())
    print("multidecl: ")
    print(t.get_multidecl())



