import ply.lex as lex

tokens = [
    # 'NUM',
    'ID',
    'CORK',
    'DOT',
    'COMMA',
    'SC',
    'LBR',
    'RBR'
]


def t_ID(t):
    r"""[0-9a-zA-Z_]+"""
    return t


t_CORK = r':-'
t_DOT = r'\.'
t_COMMA = r','
t_SC = r';'
t_LBR = r'\('
t_RBR = r'\)'
t_ignore = ' \t'


def t_newline(t):
    r"""\n+"""
    t.lexer.lineno += len(t.value)


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()
