// Requires

var antlr4 = require('antlr4/index');
var cpdLexer = require('./cpdLexer').cpdLexer;
var cpdParser = require('./cpdParser').cpdParser;
var cpdVisitor = require('./cpdVisitor').cpdVisitor;
var Map = require("collections/map");

// AST

function AST() {
    this.type = "ast";
    this.stmt_list = [];
}

function Assign_Stmt(lhs,rhs) {
    this.type = "assign";
    this.lhs = lhs;
    this.rhs = rhs;
}

function Do_Stmt(it,start,end,step,stmt_list) {
    this.type = "do";
    this.it = it;
    this.start = start;
    this.end = end;
    this.step = step;
    this.stmt_list = stmt_list;
}

function If_Stmt(expr,if_part,else_if_part,else_part) {
    this.type = "if";
    this.expr = expr;
    this.if_part = if_part;
    this.else_if_part = else_if_part;
    this.else_part = else_part;
}

function Op(lhs,op,rhs) {
    this.type = "op";
    this.lhs = lhs;
    this.op = op;
    this.rhs = rhs;
}

function Array(id,index_list) {
    this.type = "array";
    this.id = id;
    this.index_list = index_list;
}

function ElseIf(expr,stmt_list) {
    this.type = "else_if";
    this.expr = expr;
    this.stmt_list = stmt_list;
}

function JustElse(stmt_list) {
    this.type = "just_else";
    this.stmt_list = stmt_list;
}

function Val(val) {
    this.type = "val";
    this.val = val;
}

function Var(variable) {
    this.type = "var";
    this.variable = variable;
}

// AST Builder

var ASTBuilder = function() {
    cpdVisitor.call(this);
    return this;
};

ASTBuilder.prototype = Object.create(cpdVisitor.prototype);
ASTBuilder.prototype.constructor = ASTBuilder;

ASTBuilder.prototype.visitCompileUnit = function(ctx) {
    var ast = new AST();
    for (var i = 0; i < ctx.getChildCount()-1; i++)
        ast.stmt_list[i] = this.visitStmt(ctx.children[i]);
    return ast;
};

ASTBuilder.prototype.visitStmt = function(ctx) {
    return this.visit(ctx.children[0]);
}

ASTBuilder.prototype.visitDo_stmt = function(ctx) {
    var it = ctx.children[1].getText();
    var start = null;
    var end = null;
    var step = null;
    var stmt_list = [];
    var ch = ctx.getChildCount();
    if ( ctx.children[ch-1] == "DO" ) { ch = ch - 1; }
    if (this.visit(ctx.children[2]) != '\n' && this.visit(ctx.children[2]) != '\r'){
        start = this.visitExpr(ctx.children[3]);
        end = this.visitExpr(ctx.children[5]);
        if (this.visit(ctx.children[6]) != ','){
            for (var i = 0; i < ch - 8; i++)
                stmt_list[i] = this.visitStmt(ctx.children[i+7]);
        }
        else {
            step = this.visitExpr(ctx.children[7]);
            for (i = 0; i < ch - 10; i++)
                stmt_list[i] = this.visitStmt(ctx.children[i+9]);
        }
    }
    else {
        for (i = 0; i < ch - 4; i++)
            stmt_list[i] = this.visitStmt(ctx.children[i+3]);
    }
    return new Do_Stmt(it,start,end,step,stmt_list);
};

ASTBuilder.prototype.visitAssign_stmt = function(ctx) {
    var lhs = this.visit(ctx.children[0]);
    var rhs = this.visit(ctx.children[2]);
    return new Assign_Stmt(lhs,rhs);
};

ASTBuilder.prototype.visitIf_stmt = function(ctx) {
    var expr = this.visit(ctx.children[2]);
    var block = this.visit(ctx.children[4]);
    if (block.length == 3) { return new If_Stmt(expr,block[0],block[1],block[2]); }
    else { return new If_Stmt(expr,[block],null,null); }
}

ASTBuilder.prototype.visitBlock_if_stmt = function(ctx) {
    var first_if_block = this.visit(ctx.children[0]);
    var else_if_list = [];
    var just_else = null;

    if (this.visit(ctx.children[ctx.getChildCount()-1]) == "IF") {
        for (var i = 0; i < ctx.getChildCount()-4; i++) {
            else_if_list[i] = this.visitElse_if_stmt(ctx.children[i+1]);
        }
        just_else = this.visitElse_stmt(ctx.children[ctx.getChildCount()-3]);
        if (just_else == null) {
            else_if_list[ctx.getChildCount()-4] = this.visitElse_if_stmt(ctx.children[ctx.getChildCount()-3]);
        }
        else {
            else_if_list.pop();
        }
    }
    else {
        for (i = 0; i < ctx.getChildCount()-2; i++) {
            else_if_list[i] = this.visitElse_if_stmt(ctx.children[i+1]);
        }
        just_else = this.visitElse_stmt(ctx.children[ctx.getChildCount()-2]);
        if (just_else == null) {
            else_if_list[ctx.getChildCount()-3] = this.visitElse_if_stmt(ctx.children[ctx.getChildCount()-2]);
        }
        else {
            else_if_list.pop();
        }
    }
    if (else_if_list.length == 0) {
        else_if_list = null;
    }
    return [first_if_block,else_if_list,just_else];
}

ASTBuilder.prototype.visitFirst_if_block = function(ctx) {
    var stmt_list = [];
    for (var i = 0; i < ctx.getChildCount() - 2; i ++) {
        stmt_list[i] = this.visit(ctx.children[i+2]);
    }
    return stmt_list;
}

ASTBuilder.prototype.visitElse_if_stmt = function(ctx) {
    var expr = this.visit(ctx.children[2]);
    var stmt_list = [];
    if (expr == "(") { 
        expr = this.visit(ctx.children[3]);
        for (var i = 0; i < ctx.getChildCount() - 7; i ++) {
            stmt_list[i] = this.visit(ctx.children[i+7]);
        }
        return new ElseIf(expr,stmt_list);
    }
    else {
        for (i = 0; i < ctx.getChildCount() - 6; i ++) {
            stmt_list[i] = this.visit(ctx.children[i+6]);
        }
        return new ElseIf(expr,stmt_list);
    }
}

ASTBuilder.prototype.visitElse_stmt = function(ctx) {
    if (this.visit(ctx.children[1]) == "\n"){
        var stmt_list = [];
        for (var i = 0; i < ctx.getChildCount() - 2; i ++) {
            stmt_list[i] = this.visit(ctx.children[i+2]);
        }
        return new JustElse(stmt_list);
    }
    else {
        return null;
    }
}

ASTBuilder.prototype.visitArray = function(ctx) {
    var id = ctx.children[0].getText();
    var index_list = [];
    var cnt = 0;
    for (var i = 2; i < ctx.getChildCount()-1; i = i + 2){
        index_list[cnt++] = this.visitExpr(ctx.children[i]);
    }
    return new Array(id,index_list);
};

ASTBuilder.prototype.visitExpr = function(ctx) { // needs remake try specifying
    if ( ctx.getChildCount() == 1 ) {
        return this.visit(ctx.children[0]);
    }
    else {
        return new Op(this.visit(ctx.children[0]),ctx.children[1],this.visit(ctx.children[2]));
    }
}

ASTBuilder.prototype.visitOr_expr = function(ctx) {
    if ( ctx.getChildCount() == 1 ) {
        return this.visit(ctx.children[0]);
    }
    else {
        return new Op(this.visit(ctx.children[0]),ctx.children[1],this.visit(ctx.children[2]));
    }
}

ASTBuilder.prototype.visitAnd_expr = function(ctx) {
    if ( ctx.getChildCount() == 1 ) {
        return this.visit(ctx.children[0]);
    }
    else {
        return new Op(this.visit(ctx.children[0]),ctx.children[1],this.visit(ctx.children[2]));
    }
}

ASTBuilder.prototype.visitEq_expr = function(ctx) {
    if ( ctx.getChildCount() == 1 ) {
        return this.visit(ctx.children[0]);
    }
    else {
        return new Op(this.visit(ctx.children[0]),ctx.children[1],this.visit(ctx.children[2]));
    }
}

ASTBuilder.prototype.visitAdd_expr = function(ctx) {
    if ( ctx.getChildCount() == 1 ) {
        return this.visit(ctx.children[0]);
    }
    else {
        return new Op(this.visit(ctx.children[0]),ctx.children[1],this.visit(ctx.children[2]));
    }
}

ASTBuilder.prototype.visitMult_expr = function(ctx) {
    if ( ctx.getChildCount() == 1 ) {
        return this.visit(ctx.children[0]);
    }
    else {
        return new Op(null,ctx.children[0],this.visit(ctx.children[1]));
    }
}

ASTBuilder.prototype.visitFinal_expr = function(ctx) {
    if ( ctx.getChildCount() == 1 ) {
        return this.visit(ctx.children[0]); // <------------- here
    }
    else {
        return this.visit(ctx.children[1]);
    }
}

ASTBuilder.prototype.visitTerminal = function(ctx) {
    return ctx.getText();
}

ASTBuilder.prototype.visitValue = function(ctx) {
    return new Val(parseInt(ctx.getText()));
}

ASTBuilder.prototype.visitVarname = function(ctx) {
    return new Var(ctx.getText());
}

// AST Printer

var ASTPrinter = function() {
    return this;
}

ASTPrinter.prototype.visitAST = function(ast) {
    var txt = "AST(";
    for(var i = 0; i < ast.stmt_list.length; i++ ){
        txt += this.visitStmt(ast.stmt_list[i]) + ",";
    }
    if ( txt.length != 4 ){
        txt = txt.substring(0,txt.length-1);
    }
    txt += ")";
    return txt;
}

ASTPrinter.prototype.visitStmt = function(stmt) {
    if (stmt.type == "do") { return this.visitDo_Stmt(stmt); }
    else if (stmt.type == "assign") { return this.visitAssign_Stmt(stmt); }
    else if (stmt.type == "if") { return this.visitIf_Stmt(stmt); }
    //console.log("Not a valid stmt: "+stmt);
}

ASTPrinter.prototype.visitDo_Stmt = function(do_stmt) {
    var txt = "";
    for(var i = 0; i < do_stmt.stmt_list.length; i++ ){
        txt += this.visitStmt(do_stmt.stmt_list[i]) + ","; 
    }
    if ( txt.length != 0 ){
        txt = txt.substring(0,txt.length-1);
    }
    return "Do(it="+do_stmt.it+",start="+this.visitOp(do_stmt.start)+",end="+this.visitOp(do_stmt.end)+",step="+this.visitOp(do_stmt.step)+",stmt_list="+txt+")";
}

ASTPrinter.prototype.visitAssign_Stmt = function(assign_stmt) {
    return "Assign("+this.visitOp(assign_stmt.lhs)+","+this.visitOp(assign_stmt.rhs)+")";
}

ASTPrinter.prototype.visitIf_Stmt = function(if_stmt) {
    var else_if_list = "";
    var if_list = "";
    if (if_stmt.else_if_part != null){
        for (var i = 0; i < if_stmt.else_if_part.length; i++) {
            else_if_list += this.visitElseIf(if_stmt.else_if_part[i]) + ",";
        }
        if (else_if_list.length != 0){
            else_if_list = else_if_list.substring(0,else_if_list.length-1);
        }
    }
    for (i = 0; i < if_stmt.if_part.length; i++) {
        if_list += this.visitStmt(if_stmt.if_part[i]) + ",";
    }
    if (if_list.length != 0){
        if_list = if_list.substring(0,if_list.length-1);
    }
    return "If(expr="+this.visitOp(if_stmt.expr)+",if_part="+if_list+",else_if_part="+else_if_list+",else_part="+this.visitElse(if_stmt.else_part)+")";
}

ASTPrinter.prototype.visitElseIf = function(else_if) {
    var stmt_list = "";
    for (var i = 0; i < else_if.stmt_list.length; i++){
        stmt_list += this.visitStmt(else_if.stmt_list[i]) + ",";
    }
    if (stmt_list.length != 0) {
        stmt_list = stmt_list.substring(0,stmt_list.length-1);
    }
    return "ElseIf(expr="+this.visitOp(else_if.expr)+",block="+stmt_list+")";
}

ASTPrinter.prototype.visitElse = function(just_else) {
    var stmt_list = "";
    if (just_else != null) {
        for (var i = 0; i < just_else.stmt_list.length; i++){
            stmt_list += this.visitStmt(just_else.stmt_list[i]) + ",";
        }
        if (stmt_list.length != 0) {
            stmt_list = stmt_list.substring(0,stmt_list.length-1);
        }
        return "Else(block="+stmt_list+")";
    }
    return null;
}

ASTPrinter.prototype.visitOp = function(op) {
    if (op != null) {
        if (op.type == "array") {
            var index_list = "";
            for (var i = 0; i < op.index_list.length; i++) {
                index_list += this.visitOp(op.index_list[i]) + ",";
           }
           index_list = index_list.substring(0,index_list.length-1);
            return op.id+"("+index_list+")";
        }
        else if (op.type == "op") {
            return "Op("+this.visitOp(op.lhs)+","+op.op+","+this.visitOp(op.rhs)+")";
        }
        else if (op.type == "val") {
            return op.val;
        }
        else if (op.type == "var") {
            return op.variable;
        }
        else {
            //error ("Not a valid expression: "+op);
        }
    }
    else {
        return null;
    }
}

// Evaluator

function Memory(key,state) {
    this.key = key;
    this.state = state;
}

function evaluate(expr) { // A(expr,expr) -> evaluates expr for current iteration
    var type = expr.type;
    var op = expr.op;
    var lhs = expr.lhs;
    var rhs = expr.rhs;

    if ( type == "op" ) {
        if (lhs == null) {
            if (op == "-") return -evaluate(rhs);
            else if (op == "+") return evaluate(rhs);
            // else console.log("Not a valid operation: "+expr);
        }
        else {
            if (op == "+") return evaluate(lhs) + evaluate(rhs);
            else if (op == "-") return evaluate(lhs) - evaluate(rhs);
            else if (op == "*") return evaluate(lhs) * evaluate(rhs);
            else if (op == "/") return evaluate(lhs) / evaluate(rhs);
            else if (op == "%") return evaluate(lhs) % evaluate(rhs);
            else if (op == ".LT.") return evaluate(lhs) < evaluate(rhs);
            else if (op == ".LE.") return evaluate(lhs) <= evaluate(rhs);
            else if (op == ".GT.") return evaluate(lhs) > evaluate(rhs);
            else if (op == ".GE.") return evaluate(lhs) >= evaluate(rhs);
            else if (op == ".EQ.") return evaluate(lhs) == evaluate(rhs);
            else if (op == ".NE.") return evaluate(lhs) != evaluate(rhs);
            else if (op == ".OR.") return evaluate(lhs) || evaluate(rhs);
            else if (op == ".AND.") return evaluate(lhs) && evaluate(rhs);
            //else console.log("Not a valid operation: "+expr);
        }
    }
    else if ( type == "val" ) {
        return expr.val;
    }
    else if (type == "var" ) {
        if (iterators.has(expr.variable)) return iterators.get(expr.variable);
        else return 0;
    }
    else if (type == "array") {
        return 0;
    }
    else {
        //console.log("Not a valid expr: "+expr); 
    }
}

function simulate_expr(expr,side) {
    if (expr.type == "array") {
        var memory = get_memory(expr);
        if (side == "lhs") writes.push(new Memory(memory,iterators.clone())); 
        else if (side == "rhs") reads.push(new Memory(memory,iterators.clone()));
    }
    else if (expr.type == "op") {
        simulate_expr(expr.lhs,side);
        simulate_expr(expr.rhs,side);
    }
}

function simulate_stmts(stmt_list) {
    for (var i = 0; i < stmt_list.length; i++) {
        var stmt = stmt_list[i];
        if (stmt.type == "do") {
            simulate_do(stmt);
        }
        else if (stmt.type == "assign") {
            var wr = writes.length;
            var rd = reads.length;
            simulate_expr(stmt.lhs,"lhs");
            simulate_expr(stmt.rhs,"rhs");
            links.add([writes.slice(wr-writes.length),reads.slice(rd-reads.length)]);
        }
        else if (stmt.type == "if") {
            if (evaluate(stmt.expr)) {
                simulate_stmts(stmt.if_part);
            }
            else if (stmt.else_if_part != null) {
                for (i = 0; i < stmt.else_if_part.else_if_list.length; i ++) {
                    if (evaluate(stmt.else_if_part.else_if_list[i].expr)) {
                        simulate_stmts(stmt.else_if_part.else_if_list[i].stmt_list);
                        break;
                    }
                }
            }
            else if (stmt.else_part != null) {
                simulate_stmts(stmt.else_part.stmt_list);
            }
        }
    }
}

function simulate_do(do_stmt) {
    var it = do_stmt.it;
    iterators.set(it,evaluate(do_stmt.start));
    var step = do_stmt.step;
    if (step == null) step = 1;

    while (iterators.get(it) <= evaluate(do_stmt.end) ) { // ??? inclusive/non-inclusive
        simulate_stmts(do_stmt.stmt_list);
        var next_step = iterators.get(it)+step;
        iterators.set(it,next_step);
    }
}

function get_memory(array_call) { // eg A(i,j) with i = 5, j = 2 -> MEM(A:5,2)
    var index_final;
    var memory_key = "MEM(" + array_call.id + ":";
    for (var i = 0; i < array_call.index_list.length; i++) {
        index_final = evaluate(array_call.index_list[i]);
        memory_key += index_final + ",";
    }
    memory_key = memory_key.substring(0,memory_key.length-1);
    return memory_key + ")";
}

function run_simulation(ast) {
    simulate_stmts(ast.stmt_list);
}

function print_links(links) {
    for (var i = 0; i < links.length; i ++) {
        var link = links[i];
        var wrs = "";
        for (var j = 0; j < link[0].length; j ++) {
            wrs += link[0][j].key + ", "; 
        }
        wrs = wrs.substring(0,wrs.length-2);
        var rds = "";
        for (j = 0; j < link[1].length; j ++) {
            rds += link[1][j].key + ", ";
        }
        rds = rds.substring(0,rds.length-2);
        //console.log("LINK: from "+rds+" --> to "+wrs);
    }
}


// Loading tests

/*var fs = require('fs');

require.extensions['.in'] = function (module, filename) {
    module.exports = fs.readFileSync(filename, 'utf8');
};*/

/*
var tests = [];
var current_test_location = "";

for (var i = 1; i <= 55; i++) {
    current_test_location = "./book_tests/test"+i+".in";
    tests[i] = require(current_test_location);
}

for (var i = 1; i <= 54; i++) {
    var chars = new antlr4.InputStream(tests[i]);
    var lexer = new cpdLexer(chars);
    var tokens  = new antlr4.CommonTokenStream(lexer);
    var parser = new cpdParser(tokens);
    parser.buildParseTrees = true;
    var cst = parser.compileUnit();
    var ast = new ASTBuilder().visitCompileUnit(cst);
    var ast_txt = new ASTPrinter().visitAST(ast);

    console.log("Test "+i+":");
    console.log(ast_txt);
    console.log("///////////");
}
*/
/*
var chars = new antlr4.InputStream(tests[55]);
var lexer = new cpdLexer(chars);
var tokens  = new antlr4.CommonTokenStream(lexer);
var parser = new cpdParser(tokens);
parser.buildParseTrees = true;
var cst = parser.compileUnit();
var ast = new ASTBuilder().visitCompileUnit(cst);
//var ast_txt = new ASTPrinter().visitAST(ast);
run_simulation(ast);
*/

/*console.log("Writes:");
console.log(writes);
console.log(" ");
console.log("Reads:");
console.log(reads);*/
//console.log("Links:");
//print_links(links);

function parse_routine(code) {
    var chars = new antlr4.InputStream(code);
    var lexer = new cpdLexer(chars);
    var tokens  = new antlr4.CommonTokenStream(lexer);
    var parser = new cpdParser(tokens);
    parser.buildParseTrees = true;
    var cst = parser.compileUnit();
    var ast = new ASTBuilder().visitCompileUnit(cst);
    //var ast_txt = new ASTPrinter().visitAST(ast);
    return ast;
}

/*var tests = [];
var current_test_location = "";
var i = 55;

current_test_location = "./book_tests/test"+i+".in";
tests[i] = require(current_test_location);

simulate_routine(parse_routine(tests[55]));*/

