// Generated from cpd.g4 by ANTLR 4.7.2
// jshint ignore: start
var antlr4 = require('antlr4/index');

// This class defines a complete listener for a parse tree produced by cpdParser.
function cpdListener() {
	antlr4.tree.ParseTreeListener.call(this);
	return this;
}

cpdListener.prototype = Object.create(antlr4.tree.ParseTreeListener.prototype);
cpdListener.prototype.constructor = cpdListener;

// Enter a parse tree produced by cpdParser#compileUnit.
cpdListener.prototype.enterCompileUnit = function(ctx) {
};

// Exit a parse tree produced by cpdParser#compileUnit.
cpdListener.prototype.exitCompileUnit = function(ctx) {
};


// Enter a parse tree produced by cpdParser#stmt.
cpdListener.prototype.enterStmt = function(ctx) {
};

// Exit a parse tree produced by cpdParser#stmt.
cpdListener.prototype.exitStmt = function(ctx) {
};


// Enter a parse tree produced by cpdParser#do_stmt.
cpdListener.prototype.enterDo_stmt = function(ctx) {
};

// Exit a parse tree produced by cpdParser#do_stmt.
cpdListener.prototype.exitDo_stmt = function(ctx) {
};


// Enter a parse tree produced by cpdParser#if_stmt.
cpdListener.prototype.enterIf_stmt = function(ctx) {
};

// Exit a parse tree produced by cpdParser#if_stmt.
cpdListener.prototype.exitIf_stmt = function(ctx) {
};


// Enter a parse tree produced by cpdParser#block_if_stmt.
cpdListener.prototype.enterBlock_if_stmt = function(ctx) {
};

// Exit a parse tree produced by cpdParser#block_if_stmt.
cpdListener.prototype.exitBlock_if_stmt = function(ctx) {
};


// Enter a parse tree produced by cpdParser#first_if_block.
cpdListener.prototype.enterFirst_if_block = function(ctx) {
};

// Exit a parse tree produced by cpdParser#first_if_block.
cpdListener.prototype.exitFirst_if_block = function(ctx) {
};


// Enter a parse tree produced by cpdParser#else_if_stmt.
cpdListener.prototype.enterElse_if_stmt = function(ctx) {
};

// Exit a parse tree produced by cpdParser#else_if_stmt.
cpdListener.prototype.exitElse_if_stmt = function(ctx) {
};


// Enter a parse tree produced by cpdParser#else_stmt.
cpdListener.prototype.enterElse_stmt = function(ctx) {
};

// Exit a parse tree produced by cpdParser#else_stmt.
cpdListener.prototype.exitElse_stmt = function(ctx) {
};


// Enter a parse tree produced by cpdParser#assign_stmt.
cpdListener.prototype.enterAssign_stmt = function(ctx) {
};

// Exit a parse tree produced by cpdParser#assign_stmt.
cpdListener.prototype.exitAssign_stmt = function(ctx) {
};


// Enter a parse tree produced by cpdParser#empty_stmt.
cpdListener.prototype.enterEmpty_stmt = function(ctx) {
};

// Exit a parse tree produced by cpdParser#empty_stmt.
cpdListener.prototype.exitEmpty_stmt = function(ctx) {
};


// Enter a parse tree produced by cpdParser#expr.
cpdListener.prototype.enterExpr = function(ctx) {
};

// Exit a parse tree produced by cpdParser#expr.
cpdListener.prototype.exitExpr = function(ctx) {
};


// Enter a parse tree produced by cpdParser#or_expr.
cpdListener.prototype.enterOr_expr = function(ctx) {
};

// Exit a parse tree produced by cpdParser#or_expr.
cpdListener.prototype.exitOr_expr = function(ctx) {
};


// Enter a parse tree produced by cpdParser#and_expr.
cpdListener.prototype.enterAnd_expr = function(ctx) {
};

// Exit a parse tree produced by cpdParser#and_expr.
cpdListener.prototype.exitAnd_expr = function(ctx) {
};


// Enter a parse tree produced by cpdParser#eq_expr.
cpdListener.prototype.enterEq_expr = function(ctx) {
};

// Exit a parse tree produced by cpdParser#eq_expr.
cpdListener.prototype.exitEq_expr = function(ctx) {
};


// Enter a parse tree produced by cpdParser#add_expr.
cpdListener.prototype.enterAdd_expr = function(ctx) {
};

// Exit a parse tree produced by cpdParser#add_expr.
cpdListener.prototype.exitAdd_expr = function(ctx) {
};


// Enter a parse tree produced by cpdParser#mult_expr.
cpdListener.prototype.enterMult_expr = function(ctx) {
};

// Exit a parse tree produced by cpdParser#mult_expr.
cpdListener.prototype.exitMult_expr = function(ctx) {
};


// Enter a parse tree produced by cpdParser#final_expr.
cpdListener.prototype.enterFinal_expr = function(ctx) {
};

// Exit a parse tree produced by cpdParser#final_expr.
cpdListener.prototype.exitFinal_expr = function(ctx) {
};


// Enter a parse tree produced by cpdParser#array.
cpdListener.prototype.enterArray = function(ctx) {
};

// Exit a parse tree produced by cpdParser#array.
cpdListener.prototype.exitArray = function(ctx) {
};


// Enter a parse tree produced by cpdParser#varname.
cpdListener.prototype.enterVarname = function(ctx) {
};

// Exit a parse tree produced by cpdParser#varname.
cpdListener.prototype.exitVarname = function(ctx) {
};


// Enter a parse tree produced by cpdParser#value.
cpdListener.prototype.enterValue = function(ctx) {
};

// Exit a parse tree produced by cpdParser#value.
cpdListener.prototype.exitValue = function(ctx) {
};



exports.cpdListener = cpdListener;