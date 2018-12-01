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

ASTBuilder.prototype.visitExpr = function(ctx) {
    var i = 1;
    var child = this.visit(ctx.children[0]);

    while (i < ctx.getChildCount()) {
        child = new Op(child,ctx.children[i],this.visit(ctx.children[i+1]));
        i = i+2;
    }

    return child;
}

ASTBuilder.prototype.visitOr_expr = function(ctx) {
    var i = 1;
    var child = this.visit(ctx.children[0]);

    while (i < ctx.getChildCount()) {
        child = new Op(child,ctx.children[i],this.visit(ctx.children[i+1]));
        i = i+2;
    }

    return child;
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
    var i = 1;
    var child = this.visit(ctx.children[0]);

    while (i < ctx.getChildCount()) {
        child = new Op(child,ctx.children[i],this.visit(ctx.children[i+1]));
        i = i+2;
    }

    return child;
}

ASTBuilder.prototype.visitAdd_expr = function(ctx) {
    var i = 1;
    var child = this.visit(ctx.children[0]);

    while (i < ctx.getChildCount()) {
        child = new Op(child,ctx.children[i],this.visit(ctx.children[i+1]));
        i = i+2;
    }

    return child;
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
        return this.visit(ctx.children[0]);
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

function parse_routine(code) {
    var chars = new antlr4.InputStream(code);
    var lexer = new cpdLexer(chars);
    var tokens  = new antlr4.CommonTokenStream(lexer);
    var parser = new cpdParser(tokens);
    parser.buildParseTrees = true;
    var cst = parser.compileUnit();
    var ast = new ASTBuilder().visitCompileUnit(cst);
    return ast;
}