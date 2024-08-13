grammar FLang;

// aqui pode ser uma expressão condicional, aplicação de função, adição, multiplicação, uma constante inteira, ou um identificador
expr: ifExpr
    | appExpr
    | addExpr
    | mulExpr
    | intExpr
    | idExpr;

ifExpr: 'if' expr 'then' expr 'else' expr;

// app depois um identificador e um argumento.
appExpr: 'app' ID expr;

addExpr: 'add' expr expr;
mulExpr: 'mul' expr expr;

intExpr: INT;

// começa com uma letra ou sublinhado e é seguido de letras, números ou sublinhados.
idExpr: ID;

INT: '-'?[0-9]+;
ID: [a-zA-Z_][a-zA-Z0-9_]*;
WS: [ \t\r\n]+ -> skip;

