import ply.lex as lex

class SyntaxException(Exception):
    pass


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
    r"""[a-zA-Z_][0-9a-zA-Z_]*"""
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
    raise SyntaxException


lexer = lex.lex()
