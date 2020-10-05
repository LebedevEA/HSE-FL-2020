from sys import argv

import ply.yacc as yacc

from lex import tokens


class Node:
    def __init__(self, left, string, right):
        self.left = left
        self.string = string
        self.right = right


def printNode(node):
    ret_str = ""
    if node is not None:
        ret_str = '('
        ret_str += node.string
        ret_str += ' ' if node.left is not None else ''
        ret_str += printNode(node.left)
        ret_str += ' ' if node.right is not None else ''
        ret_str += printNode(node.right)
        ret_str += ')'
    return ret_str


def p_PROG(p):
    """ PROG : DEF
             | DEF PROG """
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 3:
        p[0] = Node(p[1], "DefSeq", p[2])
    else:
        assert()


def p_DEF(p):
    """ DEF : ATOM DOT
            | ATOM CORK D DOT"""
    if len(p) == 5:
        p[0] = Node(p[1], "Def", p[3])
    elif len(p) == 3:
        p[0] = p[1]


def p_D(p):
    """ D : C SC D
          | C """
    if len(p) == 4:
        p[0] = Node(p[1], "or", p[3])
    elif len(p) == 2:
        p[0] = p[1]
    else:
        assert ()


def p_C(p):
    """ C : EXPR COMMA C
          | EXPR """
    if len(p) == 4:
        p[0] = Node(p[1], "and", p[3])
    elif len(p) == 2:
        p[0] = p[1]
    else:
        assert ()


def p_EXPR(p):
    """ EXPR : ATOM
             | LBR D RBR """
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4:
        p[0] = p[2]
    else:
        assert ()


def p_ATOM(p):
    """ ATOM : ID
             | ID ATOM
             | LBR ATOM RBR ATOM
             | LBR ATOM RBR """
    if len(p) == 2:
        p[0] = Node(None, "ID = " + p[1], None)
    elif len(p) == 3:
        p[0] = Node(Node(None, "ID = " + p[1], None), "AtomSeq", p[2])
    elif len(p) == 4:
        p[0] = Node(p[2], "AtomGroup", None)
    elif len(p) == 5:
        p[0] = Node(Node(p[2], "AtomGroup", None), "AtomSeq", p[4])
    else:
        assert ()


def p_error(p):
    print("Syntax error")


parser = yacc.yacc()

filename = argv[1]
# filename = input("Input filename: ")

with open(filename, 'r') as file:
    result = parser.parse(file.read())
    with open(filename + ".out", 'w') as outFile:
        outFile.write(printNode(result) + "\n")


#  Тут возникает shift/reduce conflict, он возникает потому что когда парсится '(' `ATOM` ')' <...>
# он хочет распарсить `ATOM` как expression (то есть reduce), а второй вариант - просто
# сделать shift -- взять точку, а потом перейдет туда, где всё будет редуцировано в
# `ATOM`, ну, или почти, в любом случае, это уже редуцируется в expression, поэтому нам хватает
# shift, поэтому конфликт разрешается правильно. (Ну и вообще, делать здесь expression бесполезно)
