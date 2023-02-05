"use strict"

function AbstractOperation(...args) {
        this.args = args;
}
AbstractOperation.prototype.evaluate =  function (a, b, c) {
    return this.evalImpl(this.args.map(arg => arg.evaluate(a, b, c)));
}

AbstractOperation.prototype.toString = function () {
    return this.args.reduce((str, a) => str + a.toString() + " ", "") + this.sym;
}

AbstractOperation.prototype.diff = function (str) {
    return this.diffImpl(this.args.map(arg => arg.diff(str)));
}

AbstractOperation.prototype.prefix = function () {
    return "(" + this.sym + " " + this.args.reduce((str, a) => str + a.prefix() + " ", "").slice(0, -1) + ")";
}

function Const(a) {
    AbstractOperation.call(this, a);
}
Const.prototype = Object.create(AbstractOperation.prototype);
Const.prototype.evaluate = function () {
    return parseInt(this.args[0]);
}
Const.prototype.toString = function () {
    return this.args[0].toString();
}
Const.prototype.diff = function () {
    return new Const(0);
}
Const.prototype.prefix = Const.prototype.toString;

function Variable(str) {
    AbstractOperation.call(this, str);
}
Variable.prototype = Object.create(AbstractOperation.prototype);
Variable.prototype.evaluate = function (a, b, c) {
    return this.args[0] === "x" ? a : this.args[0] === "y" ? b : this.args[0] === "z" ? c : undefined;
}
Variable.prototype.toString = function () {
    return this.args[0].toString();
}
Variable.prototype.diff = function (str) {
    return this.args[0] === str ? new Const(1) : new Const(0);
}
Variable.prototype.prefix = Variable.prototype.toString;

function Add(x, y) {
    AbstractOperation.call(this, x, y);
}
Add.prototype = Object.create(AbstractOperation.prototype);
Add.prototype.evalImpl = arr => arr[0] + arr[1];
Add.prototype.sym = "+";
Add.prototype.diffImpl = arr => new Add(arr[0], arr[1]);

function Subtract(x, y) {
    AbstractOperation.call(this, x, y);
}
Subtract.prototype = Object.create(AbstractOperation.prototype);
Subtract.prototype.evalImpl = arr => arr[0] - arr[1];
Subtract.prototype.sym = "-";
Subtract.prototype.diffImpl = arr => new Subtract(arr[0], arr[1]);


function Multiply(x, y) {
    AbstractOperation.call(this, x, y);
}
Multiply.prototype = Object.create(AbstractOperation.prototype);
Multiply.prototype.evalImpl = arr => arr[0] * arr[1];
Multiply.prototype.sym = "*";
Multiply.prototype.diffImpl = function (arr) {
    return new Add(new Multiply(arr[0], this.args[1]), new Multiply(this.args[0], arr[1]));
}


function Divide(x, y) {
    AbstractOperation.call(this, x, y);
}
Divide.prototype = Object.create(AbstractOperation.prototype);
Divide.prototype.evalImpl = arr => arr[0] / arr[1];
Divide.prototype.sym = "/";
Divide.prototype.diffImpl = function (arr) {
    return new Divide(new Subtract(new Multiply(arr[0], this.args[1]),
        new Multiply(this.args[0], arr[1])), new Multiply(this.args[1], this.args[1]));
}


function Negate(x) {
    AbstractOperation.call(this, x);
}
Negate.prototype = Object.create(AbstractOperation.prototype);
Negate.prototype.evalImpl = arr => -arr[0];
Negate.prototype.sym = "negate";
Negate.prototype.diffImpl = arr => new Negate(arr[0]);

function Sinh(x) {
    AbstractOperation.call(this, x);
}
Sinh.prototype = Object.create(AbstractOperation.prototype);
Sinh.prototype.evalImpl = arr => Math.sinh(arr[0]);
Sinh.prototype.sym = "sinh";
Sinh.prototype.diffImpl = function (arr) {
    return new Multiply(new Cosh(this.args[0]), arr[0]);
}

function Cosh(x) {
    AbstractOperation.call(this, x);
}
Cosh.prototype = Object.create(AbstractOperation.prototype);
Cosh.prototype.evalImpl = arr => Math.cosh(arr[0]);
Cosh.prototype.sym = "cosh";
Cosh.prototype.diffImpl = function (arr) {
    return new Multiply(new Sinh(this.args[0]), arr[0]);
}


////////HW8--------------------------------------------------------------------------------------
function Mean(...args) {
    AbstractOperation.call(this, ...args);
}
Mean.prototype = Object.create(AbstractOperation.prototype);
Mean.prototype.evalImpl = arr => arr.reduce((sum, arg) => sum + arg, 0) / arr.length;
Mean.prototype.sym = "mean";
Mean.prototype.diffImpl = arr => new Mean(...arr);

function Var(...args) {
    AbstractOperation.call(this, ...args);
}
Var.prototype = Object.create(AbstractOperation.prototype);
Var.prototype.evalImpl = arr => arr.reduce((sum, arg) => sum + arg * arg, 0) / arr.length -
                                Math.pow(arr.reduce((sum, arg) => sum + arg, 0) / arr.length, 2);
Var.prototype.sym = "var";
Var.prototype.diffImpl = function (arr) {
    let j = 0
    return  new Multiply(new Const(2), new Subtract(new Mean(...arr.map(arg =>
            new Multiply(new Multiply(arg, arg), this.args[j++]))),
            new Multiply(new Mean(...this.args),
            new Mean(...arr))));
}
////////////////////-------------------------

const operations = (s) => (stack) => {
    switch (s) {
        case 'x' : return new Variable('x');
        case 'y' : return new Variable('y');
        case 'z' : return new Variable('z');
        case '+' : return new Add(stack[stack.length - 2], stack.pop(), stack.pop());
        case '-' : return new Subtract(stack[stack.length - 2], stack.pop(), stack.pop());
        case '*':  return new Multiply(stack[stack.length - 2], stack.pop(), stack.pop());
        case '/' : return new Divide(stack[stack.length - 2], stack.pop(), stack.pop());
        case 'negate' : return new Negate(stack.pop());
        case 'sinh' : return new Sinh(stack.pop());
        case 'cosh' : return new Cosh(stack.pop());
    }
}

const parse = str => {
    let arr = str.split(" ").filter(arg => arg !== " " && arg !== ""), stack = [];
    for (const element of arr) {
        if (parseInt(element) || element === "0") {
            stack.push(new Const(parseInt(element)));
        } else {
            stack.push(operations(element)(stack));
        }
    }
    return stack.pop();
}



//HW8------------------------------------------------------------------------------
class ParsingError extends Error {
    constructor(typeMessage, parseExpr, argNumber) {
        super("Incorrectly expression in position (argument number): " + argNumber +
              "." + " Error environment: " + parseExpr + ". " + typeMessage);
        this._typeMessage = typeMessage;
        this._parseExpr = parseExpr;
        this._argNumber = argNumber;
    }
    //просто ф-ции (на случай, если кому-нибудь захочется использовать подробности ошибки по отдельности)
    getTypeMessage() { return this._typeMessage; }
    getParseExpr() { return this._parseExpr; }
    getArgNumber() { return this._argNumber; }
}

const mapOperation = {
    '+' : [Add, 2],
    '-' : [Subtract, 2],
    '*' :  [Multiply, 2],
    '/' : [Divide, 2],
    'negate' : [Negate, 1],
    'sinh' : [Sinh, 1],
    'cosh' : [Cosh, 1],
    'mean' : [Mean],
    'var' : [Var],
}

const parsePrefix = str => {
    let currentPos = 0;
    //получаем eps-окрестность ошибки для красоты)
    const getEnvironmentError = n => {
        let str = "";
        for (let j = Math.max(0, currentPos - n); j < Math.min(ArrayOfExprChars.length, currentPos + n); j++) {
            str += ArrayOfExprChars[j] + " ";
        }
        return str.slice(0, -1);
    }

    //вспомогательная ф-ция 1 (парсит указанное или произвольное число аргументов операции)
    const makeOperation = (func, countArguments) => {
        let stayArguments = countArguments, arr = [];
        while ((stayArguments-- !== 0) && ArrayOfExprChars[currentPos] !== ")") {
            arr.push(parsePrefixOperation());
        }
        if (arr.length < countArguments) {
            throw new ParsingError("The operation contains an invalid number of arguments",
                getEnvironmentError(5), currentPos);
        }
        return new func(...arr);
    }

    //вспомогательная ф-ция 2 (парсит операцию)
    const parsePrefixOperation = () => {
        let operation;

        if (ArrayOfExprChars[currentPos] === "x" || ArrayOfExprChars[currentPos] === "y" || ArrayOfExprChars[currentPos] === "z") {
            return new Variable(ArrayOfExprChars[currentPos++]);
        } else if (parseInt(ArrayOfExprChars[currentPos]) || ArrayOfExprChars[currentPos] === "0") {
            if (parseInt(ArrayOfExprChars[currentPos]).toString() !== ArrayOfExprChars[currentPos]) {
                throw new ParsingError("The constant must contain only digits!",
                    getEnvironmentError(5), currentPos);
            }
            return new Const(parseInt(ArrayOfExprChars[currentPos++]));
        }

        if (ArrayOfExprChars[currentPos++] !== "(") {
            throw new ParsingError("Operation should start with an opening bracket!",
                getEnvironmentError(5), currentPos);
        }

        if (mapOperation.hasOwnProperty(ArrayOfExprChars[currentPos])) {
            operation = makeOperation(...mapOperation[ArrayOfExprChars[currentPos++]]);
        } else {
            throw new ParsingError("Invalid operation sign!",
                getEnvironmentError(5), currentPos);
        }

        if (ArrayOfExprChars[currentPos++] !== ")") {
            throw new ParsingError("Operation should finish with an closing bracket!",
                getEnvironmentError(5), currentPos);
        }
        return operation;
    }
    //
    
    let ArrayOfExprChars = str.split("").map(arg => arg === "(" ? " ( " : arg === ")" ? " ) " : arg).
                           join("").split(" ").filter(arg => arg !== " " && arg !== "");
    let expression = parsePrefixOperation();
    if (currentPos !== ArrayOfExprChars.length) {
        throw new ParsingError("There is ambiguity in the expression (it has parsed, but not completely).",
                               getEnvironmentError(10), currentPos);
    }
    return expression;
}