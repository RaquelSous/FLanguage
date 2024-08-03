grammar FLang;

expr: def | app | ifThenElse | whileLoop | lambda | logicExpr | arithExpr | var;

def: 'def' VAR ':' expr '=' expr;

app: '(' expr expr ')';

ifThenElse: '(' 'if' expr expr expr ')';

whileLoop: '(' 'while' expr expr ')';

lambda: '(' 'lambda' VAR expr ')';

logicExpr: (notExpr | andExpr | orExpr | equalsExpr);

notExpr: '(' 'not' expr ')';

andExpr: '(' 'and' expr expr ')';

orExpr: '(' 'or' expr expr ')';

equalsExpr: '(' 'equals' expr expr ')';

arithExpr: (addExpr | subExpr | mulExpr | divExpr | number);

addExpr: '(' '+' expr expr ')';

subExpr: '(' '-' expr expr ')';

mulExpr: '(' '*' expr expr ')';

divExpr: '(' '/' expr expr ')';

var: VAR;

number: DIGIT+;

VAR: [a-zA-Z_][a-zA-Z_0-9]*;

DIGIT: '0'..'9';

WS: [ \t\r\n]+ -> skip;

// (def x : int = 5)
   //(def add : (lambda a (lambda b (+ a b))))
   //(add 2 3)

//(if (> x 0) (print "Positive") (print "Non-positive"))
  //(while (> x 0) (begin (decrement x) (print x)))

//(and (not (equals 1 2)) (or (equals 2 2) (equals 3 4)))