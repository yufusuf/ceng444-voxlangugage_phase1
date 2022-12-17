import sly

class Lexer(sly.Lexer):
    tokens = { NUMBER, ID, WHILE, IF, ELSE, PRINT,
               PLUS, MINUS, TIMES, DIVIDE, ASSIGN,
               EQ, LT, LE, GT, GE, NE, AND,
               FALSE, TRUE, FUN, FOR, OR,
               RETURN, VAR, STRING, NOT}

    ignore = ' \t' 
    ignore_comment = r'//.*'

    ID = r'[a-zA-Z]\w*'
    ID['if'] = IF 
    ID['else'] = ELSE
    ID['while'] = WHILE
    ID['for'] = FOR
    ID['or'] = OR
    ID['and'] = AND
    ID['fun'] = FUN
    ID['return'] = RETURN
    ID['var'] = VAR
    ID['print'] = PRINT
    ID['false'] = FALSE
    ID['true'] = TRUE

    PLUS = r'\+'
    MINUS = r'-'
    TIMES = r'\*'
    DIVIDE = r'/'
    EQ = r'=='
    ASSIGN = r'='
    LE = r'<='
    GE = r'>='
    NE = r'!='
    LT = r'<'
    GT = r'>'
    NOT = r'!' 
    
    literals = { '(', ')', '{', '}', '[', ']', ';' , ',', '#'}

    @_(r'(\d+[.])?\d+')
    def NUMBER(self, t):
        t.value = float(t.value)
        return t

    @_(r'\n+')
    def ignore_newline(self, t):
        self.lineno += len(t.value)
        return t

    @_(r'true')
    def TRUE(self, t):
        t.value = True
        return t
    
    @_(r'false')
    def FALSE(self, t):
        t.value = False
        return t

    @_(r'"[^"]*"') 
    def STRING(self, t):
        t.value = str(t.value[1:len(t.value) - 1])
        return t

    def error(self, t):
        # print('line : %d illegal character %r' % ((self.lineno), (t.value[0])))
        self.index += 1
        t.value = t.value[0]
        return t
