"use strict"
const binaryOperation = f => (expr1, expr2) => (a, b, c) => f(expr1(a, b, c), expr2(a, b, c));
const unaryOperation = f => expr => (a, b, c) => f(expr(a, b, c));
const variable = (str) => (a, b, c) => str === "x" ? a : str === "y" ? b : str === "z" ? c : undefined;
const cnst = val => () => val;
const add = binaryOperation((x, y) => x + y);
const subtract = binaryOperation((x, y) => x - y);
const multiply = binaryOperation((x, y) => x * y);
const divide = binaryOperation((x, y) => x / y);
const negate = unaryOperation(x => -x);
const sinh = unaryOperation(x => Math.sinh(x));
const cosh = unaryOperation(x => Math.cosh(x));
const pi = cnst(Math.PI);
const e = cnst(Math.E);

//Test
let testExpr = add(subtract(multiply(variable('x'), variable('x')), multiply(cnst(2),
    variable('x'))), cnst(1));
for (let i = 0; i < 10; i++) {
    testExpr(i, 0, 0);
}


//Parser
const operationBinarySymbols = {
    '+' : add,
    '-' : subtract,
    '*': multiply,
    '/' : divide,
}

const operationUnarySymbols = {
    "n" : negate,
}

const consts = {
    "x" : variable("x"),
    "y" : variable("y"),
    "z" : variable("z"),
    "p" : pi(),
    "e" : e(),

}

const skip = (i, str) => {
    while (str[i] !== " " && i < str.length) {
        i++;
    }
    while (str[i] === " ") {
        i++;
    }
    return i;
}

const parse = str => {
    let stack = [], i = 0, a;
    while (i < str.length) {
        if (operationBinarySymbols.hasOwnProperty(str[i]) && (i + 1 === str.length || str[i + 1] === " ")) {
            a = stack.pop();
            stack.push(operationBinarySymbols[str[i]](stack.pop(), a));
        } else if (operationUnarySymbols.hasOwnProperty(str[i])) {
            stack.push(operationUnarySymbols[str[i]](stack.pop()));
        } else if (consts.hasOwnProperty(str[i])) {
            stack.push(consts[str[i]]);
        } else {
            stack.push(cnst(parseInt(str.substring(i, str.length))));
        }
        i = skip(i, str);
    }
    return stack.pop();
}



//Test parse
parse("x x 2 - * x * 1 +")(5);