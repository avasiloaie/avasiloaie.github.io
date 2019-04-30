// Generated from cpd.g4 by ANTLR 4.7.2
// jshint ignore: start
var antlr4 = require('antlr4/index');

// This class defines a complete generic visitor for a parse tree produced by cpdParser.

function cpdVisitor() {
	antlr4.tree.ParseTreeVisitor.call(this);
	return this;
}

cpdVisitor.prototype = Object.create(antlr4.tree.ParseTreeVisitor.prototype);
cpdVisitor.prototype.constructor = cpdVisitor;

// Visit a parse tree produced by cpdParser#compileUnit.
cpdVisitor.prototype.visitCompileUnit = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#stmt.
cpdVisitor.prototype.visitStmt = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#do_stmt.
cpdVisitor.prototype.visitDo_stmt = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#if_stmt.
cpdVisitor.prototype.visitIf_stmt = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#block_if_stmt.
cpdVisitor.prototype.visitBlock_if_stmt = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#first_if_block.
cpdVisitor.prototype.visitFirst_if_block = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#else_if_stmt.
cpdVisitor.prototype.visitElse_if_stmt = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#else_stmt.
cpdVisitor.prototype.visitElse_stmt = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#assign_stmt.
cpdVisitor.prototype.visitAssign_stmt = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#empty_stmt.
cpdVisitor.prototype.visitEmpty_stmt = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#expr.
cpdVisitor.prototype.visitExpr = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#or_expr.
cpdVisitor.prototype.visitOr_expr = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#and_expr.
cpdVisitor.prototype.visitAnd_expr = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#eq_expr.
cpdVisitor.prototype.visitEq_expr = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#add_expr.
cpdVisitor.prototype.visitAdd_expr = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#mult_expr.
cpdVisitor.prototype.visitMult_expr = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#final_expr.
cpdVisitor.prototype.visitFinal_expr = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#array.
cpdVisitor.prototype.visitArray = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#varname.
cpdVisitor.prototype.visitVarname = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by cpdParser#value.
cpdVisitor.prototype.visitValue = function(ctx) {
  return this.visitChildren(ctx);
};



exports.cpdVisitor = cpdVisitor;