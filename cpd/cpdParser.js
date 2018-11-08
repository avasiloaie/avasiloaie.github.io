// Generated from cpd.g4 by ANTLR 4.7.1
// jshint ignore: start
var antlr4 = require('antlr4/index');
var cpdListener = require('./cpdListener').cpdListener;
var cpdVisitor = require('./cpdVisitor').cpdVisitor;

var grammarFileName = "cpd.g4";

var serializedATN = ["\u0003\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964",
    "\u0003!\u00cd\u0004\u0002\t\u0002\u0004\u0003\t\u0003\u0004\u0004\t",
    "\u0004\u0004\u0005\t\u0005\u0004\u0006\t\u0006\u0004\u0007\t\u0007\u0004",
    "\b\t\b\u0004\t\t\t\u0004\n\t\n\u0004\u000b\t\u000b\u0004\f\t\f\u0004",
    "\r\t\r\u0004\u000e\t\u000e\u0004\u000f\t\u000f\u0004\u0010\t\u0010\u0004",
    "\u0011\t\u0011\u0004\u0012\t\u0012\u0003\u0002\u0006\u0002&\n\u0002",
    "\r\u0002\u000e\u0002\'\u0003\u0002\u0003\u0002\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0005\u00035\n\u0003\u0003\u0004\u0003\u0004\u0003\u0004",
    "\u0003\u0004\u0003\u0004\u0003\u0004\u0003\u0004\u0003\u0004\u0005\u0004",
    "?\n\u0004\u0005\u0004A\n\u0004\u0003\u0004\u0003\u0004\u0006\u0004E",
    "\n\u0004\r\u0004\u000e\u0004F\u0003\u0004\u0003\u0004\u0003\u0004\u0005",
    "\u0004L\n\u0004\u0003\u0005\u0003\u0005\u0003\u0005\u0003\u0005\u0003",
    "\u0005\u0003\u0005\u0005\u0005T\n\u0005\u0003\u0006\u0003\u0006\u0007",
    "\u0006X\n\u0006\f\u0006\u000e\u0006[\u000b\u0006\u0003\u0006\u0005\u0006",
    "^\n\u0006\u0003\u0006\u0003\u0006\u0003\u0006\u0005\u0006c\n\u0006\u0003",
    "\u0007\u0003\u0007\u0003\u0007\u0006\u0007h\n\u0007\r\u0007\u000e\u0007",
    "i\u0003\b\u0003\b\u0003\b\u0005\bo\n\b\u0003\b\u0003\b\u0003\b\u0003",
    "\b\u0003\b\u0003\b\u0006\bw\n\b\r\b\u000e\bx\u0003\t\u0003\t\u0003\t",
    "\u0006\t~\n\t\r\t\u000e\t\u007f\u0003\n\u0003\n\u0003\n\u0003\n\u0003",
    "\n\u0003\n\u0003\n\u0005\n\u0089\n\n\u0003\u000b\u0003\u000b\u0003\u000b",
    "\u0007\u000b\u008e\n\u000b\f\u000b\u000e\u000b\u0091\u000b\u000b\u0003",
    "\f\u0003\f\u0003\f\u0007\f\u0096\n\f\f\f\u000e\f\u0099\u000b\f\u0003",
    "\r\u0003\r\u0003\r\u0005\r\u009e\n\r\u0003\u000e\u0003\u000e\u0003\u000e",
    "\u0007\u000e\u00a3\n\u000e\f\u000e\u000e\u000e\u00a6\u000b\u000e\u0003",
    "\u000f\u0003\u000f\u0003\u000f\u0007\u000f\u00ab\n\u000f\f\u000f\u000e",
    "\u000f\u00ae\u000b\u000f\u0003\u0010\u0007\u0010\u00b1\n\u0010\f\u0010",
    "\u000e\u0010\u00b4\u000b\u0010\u0003\u0010\u0003\u0010\u0003\u0011\u0003",
    "\u0011\u0003\u0011\u0003\u0011\u0003\u0011\u0003\u0011\u0003\u0011\u0005",
    "\u0011\u00bf\n\u0011\u0003\u0012\u0003\u0012\u0003\u0012\u0003\u0012",
    "\u0003\u0012\u0007\u0012\u00c6\n\u0012\f\u0012\u000e\u0012\u00c9\u000b",
    "\u0012\u0003\u0012\u0003\u0012\u0003\u0012\u0002\u0002\u0013\u0002\u0004",
    "\u0006\b\n\f\u000e\u0010\u0012\u0014\u0016\u0018\u001a\u001c\u001e ",
    "\"\u0002\u0005\u0003\u0002\u0017\u001c\u0003\u0002\u0010\u0011\u0003",
    "\u0002\u0012\u0013\u0002\u00d5\u0002%\u0003\u0002\u0002\u0002\u0004",
    "4\u0003\u0002\u0002\u0002\u00066\u0003\u0002\u0002\u0002\bM\u0003\u0002",
    "\u0002\u0002\nU\u0003\u0002\u0002\u0002\fd\u0003\u0002\u0002\u0002\u000e",
    "n\u0003\u0002\u0002\u0002\u0010z\u0003\u0002\u0002\u0002\u0012\u0088",
    "\u0003\u0002\u0002\u0002\u0014\u008a\u0003\u0002\u0002\u0002\u0016\u0092",
    "\u0003\u0002\u0002\u0002\u0018\u009a\u0003\u0002\u0002\u0002\u001a\u009f",
    "\u0003\u0002\u0002\u0002\u001c\u00a7\u0003\u0002\u0002\u0002\u001e\u00b2",
    "\u0003\u0002\u0002\u0002 \u00be\u0003\u0002\u0002\u0002\"\u00c0\u0003",
    "\u0002\u0002\u0002$&\u0005\u0004\u0003\u0002%$\u0003\u0002\u0002\u0002",
    "&\'\u0003\u0002\u0002\u0002\'%\u0003\u0002\u0002\u0002\'(\u0003\u0002",
    "\u0002\u0002()\u0003\u0002\u0002\u0002)*\u0007\u0002\u0002\u0003*\u0003",
    "\u0003\u0002\u0002\u0002+,\u0005\u0006\u0004\u0002,-\u0007 \u0002\u0002",
    "-5\u0003\u0002\u0002\u0002./\u0005\b\u0005\u0002/0\u0007 \u0002\u0002",
    "05\u0003\u0002\u0002\u000212\u0005\u0012\n\u000223\u0007 \u0002\u0002",
    "35\u0003\u0002\u0002\u00024+\u0003\u0002\u0002\u00024.\u0003\u0002\u0002",
    "\u000241\u0003\u0002\u0002\u00025\u0005\u0003\u0002\u0002\u000267\u0007",
    "\t\u0002\u00027@\u0007\u001e\u0002\u000289\u0007\u000f\u0002\u00029",
    ":\u0005\u001a\u000e\u0002:;\u0007\u000b\u0002\u0002;>\u0005\u001a\u000e",
    "\u0002<=\u0007\u000b\u0002\u0002=?\u0005\u001a\u000e\u0002><\u0003\u0002",
    "\u0002\u0002>?\u0003\u0002\u0002\u0002?A\u0003\u0002\u0002\u0002@8\u0003",
    "\u0002\u0002\u0002@A\u0003\u0002\u0002\u0002AB\u0003\u0002\u0002\u0002",
    "BD\u0007 \u0002\u0002CE\u0005\u0004\u0003\u0002DC\u0003\u0002\u0002",
    "\u0002EF\u0003\u0002\u0002\u0002FD\u0003\u0002\u0002\u0002FG\u0003\u0002",
    "\u0002\u0002GK\u0003\u0002\u0002\u0002HL\u0007\n\u0002\u0002IJ\u0007",
    "\u0003\u0002\u0002JL\u0007\t\u0002\u0002KH\u0003\u0002\u0002\u0002K",
    "I\u0003\u0002\u0002\u0002L\u0007\u0003\u0002\u0002\u0002MN\u0007\u0004",
    "\u0002\u0002NO\u0007\f\u0002\u0002OP\u0005\u0014\u000b\u0002PS\u0007",
    "\r\u0002\u0002QT\u0005\n\u0006\u0002RT\u0005\u0012\n\u0002SQ\u0003\u0002",
    "\u0002\u0002SR\u0003\u0002\u0002\u0002T\t\u0003\u0002\u0002\u0002UY",
    "\u0005\f\u0007\u0002VX\u0005\u000e\b\u0002WV\u0003\u0002\u0002\u0002",
    "X[\u0003\u0002\u0002\u0002YW\u0003\u0002\u0002\u0002YZ\u0003\u0002\u0002",
    "\u0002Z]\u0003\u0002\u0002\u0002[Y\u0003\u0002\u0002\u0002\\^\u0005",
    "\u0010\t\u0002]\\\u0003\u0002\u0002\u0002]^\u0003\u0002\u0002\u0002",
    "^b\u0003\u0002\u0002\u0002_c\u0007\u0007\u0002\u0002`a\u0007\u0003\u0002",
    "\u0002ac\u0007\u0004\u0002\u0002b_\u0003\u0002\u0002\u0002b`\u0003\u0002",
    "\u0002\u0002c\u000b\u0003\u0002\u0002\u0002de\u0007\u0005\u0002\u0002",
    "eg\u0007 \u0002\u0002fh\u0005\u0004\u0003\u0002gf\u0003\u0002\u0002",
    "\u0002hi\u0003\u0002\u0002\u0002ig\u0003\u0002\u0002\u0002ij\u0003\u0002",
    "\u0002\u0002j\r\u0003\u0002\u0002\u0002ko\u0007\b\u0002\u0002lm\u0007",
    "\u0006\u0002\u0002mo\u0007\u0004\u0002\u0002nk\u0003\u0002\u0002\u0002",
    "nl\u0003\u0002\u0002\u0002op\u0003\u0002\u0002\u0002pq\u0007\f\u0002",
    "\u0002qr\u0005\u0014\u000b\u0002rs\u0007\r\u0002\u0002st\u0007\u0005",
    "\u0002\u0002tv\u0007 \u0002\u0002uw\u0005\u0004\u0003\u0002vu\u0003",
    "\u0002\u0002\u0002wx\u0003\u0002\u0002\u0002xv\u0003\u0002\u0002\u0002",
    "xy\u0003\u0002\u0002\u0002y\u000f\u0003\u0002\u0002\u0002z{\u0007\u0006",
    "\u0002\u0002{}\u0007 \u0002\u0002|~\u0005\u0004\u0003\u0002}|\u0003",
    "\u0002\u0002\u0002~\u007f\u0003\u0002\u0002\u0002\u007f}\u0003\u0002",
    "\u0002\u0002\u007f\u0080\u0003\u0002\u0002\u0002\u0080\u0011\u0003\u0002",
    "\u0002\u0002\u0081\u0082\u0005\"\u0012\u0002\u0082\u0083\u0007\u000f",
    "\u0002\u0002\u0083\u0084\u0005\u0014\u000b\u0002\u0084\u0089\u0003\u0002",
    "\u0002\u0002\u0085\u0086\u0007\u001e\u0002\u0002\u0086\u0087\u0007\u000f",
    "\u0002\u0002\u0087\u0089\u0005\u0014\u000b\u0002\u0088\u0081\u0003\u0002",
    "\u0002\u0002\u0088\u0085\u0003\u0002\u0002\u0002\u0089\u0013\u0003\u0002",
    "\u0002\u0002\u008a\u008f\u0005\u0016\f\u0002\u008b\u008c\u0007\u0016",
    "\u0002\u0002\u008c\u008e\u0005\u0016\f\u0002\u008d\u008b\u0003\u0002",
    "\u0002\u0002\u008e\u0091\u0003\u0002\u0002\u0002\u008f\u008d\u0003\u0002",
    "\u0002\u0002\u008f\u0090\u0003\u0002\u0002\u0002\u0090\u0015\u0003\u0002",
    "\u0002\u0002\u0091\u008f\u0003\u0002\u0002\u0002\u0092\u0097\u0005\u0018",
    "\r\u0002\u0093\u0094\u0007\u0015\u0002\u0002\u0094\u0096\u0005\u0018",
    "\r\u0002\u0095\u0093\u0003\u0002\u0002\u0002\u0096\u0099\u0003\u0002",
    "\u0002\u0002\u0097\u0095\u0003\u0002\u0002\u0002\u0097\u0098\u0003\u0002",
    "\u0002\u0002\u0098\u0017\u0003\u0002\u0002\u0002\u0099\u0097\u0003\u0002",
    "\u0002\u0002\u009a\u009d\u0005\u001a\u000e\u0002\u009b\u009c\t\u0002",
    "\u0002\u0002\u009c\u009e\u0005\u001a\u000e\u0002\u009d\u009b\u0003\u0002",
    "\u0002\u0002\u009d\u009e\u0003\u0002\u0002\u0002\u009e\u0019\u0003\u0002",
    "\u0002\u0002\u009f\u00a4\u0005\u001c\u000f\u0002\u00a0\u00a1\t\u0003",
    "\u0002\u0002\u00a1\u00a3\u0005\u001c\u000f\u0002\u00a2\u00a0\u0003\u0002",
    "\u0002\u0002\u00a3\u00a6\u0003\u0002\u0002\u0002\u00a4\u00a2\u0003\u0002",
    "\u0002\u0002\u00a4\u00a5\u0003\u0002\u0002\u0002\u00a5\u001b\u0003\u0002",
    "\u0002\u0002\u00a6\u00a4\u0003\u0002\u0002\u0002\u00a7\u00ac\u0005\u001e",
    "\u0010\u0002\u00a8\u00a9\t\u0004\u0002\u0002\u00a9\u00ab\u0005\u001e",
    "\u0010\u0002\u00aa\u00a8\u0003\u0002\u0002\u0002\u00ab\u00ae\u0003\u0002",
    "\u0002\u0002\u00ac\u00aa\u0003\u0002\u0002\u0002\u00ac\u00ad\u0003\u0002",
    "\u0002\u0002\u00ad\u001d\u0003\u0002\u0002\u0002\u00ae\u00ac\u0003\u0002",
    "\u0002\u0002\u00af\u00b1\t\u0003\u0002\u0002\u00b0\u00af\u0003\u0002",
    "\u0002\u0002\u00b1\u00b4\u0003\u0002\u0002\u0002\u00b2\u00b0\u0003\u0002",
    "\u0002\u0002\u00b2\u00b3\u0003\u0002\u0002\u0002\u00b3\u00b5\u0003\u0002",
    "\u0002\u0002\u00b4\u00b2\u0003\u0002\u0002\u0002\u00b5\u00b6\u0005 ",
    "\u0011\u0002\u00b6\u001f\u0003\u0002\u0002\u0002\u00b7\u00bf\u0005\"",
    "\u0012\u0002\u00b8\u00bf\u0007\u001e\u0002\u0002\u00b9\u00bf\u0007\u001f",
    "\u0002\u0002\u00ba\u00bb\u0007\f\u0002\u0002\u00bb\u00bc\u0005\u001a",
    "\u000e\u0002\u00bc\u00bd\u0007\r\u0002\u0002\u00bd\u00bf\u0003\u0002",
    "\u0002\u0002\u00be\u00b7\u0003\u0002\u0002\u0002\u00be\u00b8\u0003\u0002",
    "\u0002\u0002\u00be\u00b9\u0003\u0002\u0002\u0002\u00be\u00ba\u0003\u0002",
    "\u0002\u0002\u00bf!\u0003\u0002\u0002\u0002\u00c0\u00c1\u0007\u001e",
    "\u0002\u0002\u00c1\u00c2\u0007\f\u0002\u0002\u00c2\u00c7\u0005\u0014",
    "\u000b\u0002\u00c3\u00c4\u0007\u000b\u0002\u0002\u00c4\u00c6\u0005\u0014",
    "\u000b\u0002\u00c5\u00c3\u0003\u0002\u0002\u0002\u00c6\u00c9\u0003\u0002",
    "\u0002\u0002\u00c7\u00c5\u0003\u0002\u0002\u0002\u00c7\u00c8\u0003\u0002",
    "\u0002\u0002\u00c8\u00ca\u0003\u0002\u0002\u0002\u00c9\u00c7\u0003\u0002",
    "\u0002\u0002\u00ca\u00cb\u0007\r\u0002\u0002\u00cb#\u0003\u0002\u0002",
    "\u0002\u0019\'4>@FKSY]binx\u007f\u0088\u008f\u0097\u009d\u00a4\u00ac",
    "\u00b2\u00be\u00c7"].join("");


var atn = new antlr4.atn.ATNDeserializer().deserialize(serializedATN);

var decisionsToDFA = atn.decisionToState.map( function(ds, index) { return new antlr4.dfa.DFA(ds, index); });

var sharedContextCache = new antlr4.PredictionContextCache();

var literalNames = [ null, null, null, null, null, null, null, null, null, 
                     "','", "'('", "')'", "':'", "'='", null, "'+'", "'/'", 
                     "'*'", null, null, null, null, null, null, null, null, 
                     null, "'TO'" ];

var symbolicNames = [ null, "END", "IF", "THEN", "ELSE", "ENDIF", "ELSEIF", 
                      "DO", "ENDDO", "COMMA", "LPAR", "RPAR", "COLON", "ASSIGN", 
                      "MINUS", "PLUS", "DIV", "STAR", "NOT", "AND", "OR", 
                      "LT", "LE", "GT", "GE", "NE", "EQ", "TO", "VARNAME", 
                      "INT", "EOL", "WS" ];

var ruleNames =  [ "compileUnit", "stmt", "do_stmt", "if_stmt", "block_if_stmt", 
                   "first_if_block", "else_if_stmt", "else_stmt", "assign_stmt", 
                   "expr", "or_expr", "and_expr", "eq_expr", "add_expr", 
                   "mult_expr", "final_expr", "array" ];

function cpdParser (input) {
	antlr4.Parser.call(this, input);
    this._interp = new antlr4.atn.ParserATNSimulator(this, atn, decisionsToDFA, sharedContextCache);
    this.ruleNames = ruleNames;
    this.literalNames = literalNames;
    this.symbolicNames = symbolicNames;
    return this;
}

cpdParser.prototype = Object.create(antlr4.Parser.prototype);
cpdParser.prototype.constructor = cpdParser;

Object.defineProperty(cpdParser.prototype, "atn", {
	get : function() {
		return atn;
	}
});

cpdParser.EOF = antlr4.Token.EOF;
cpdParser.END = 1;
cpdParser.IF = 2;
cpdParser.THEN = 3;
cpdParser.ELSE = 4;
cpdParser.ENDIF = 5;
cpdParser.ELSEIF = 6;
cpdParser.DO = 7;
cpdParser.ENDDO = 8;
cpdParser.COMMA = 9;
cpdParser.LPAR = 10;
cpdParser.RPAR = 11;
cpdParser.COLON = 12;
cpdParser.ASSIGN = 13;
cpdParser.MINUS = 14;
cpdParser.PLUS = 15;
cpdParser.DIV = 16;
cpdParser.STAR = 17;
cpdParser.NOT = 18;
cpdParser.AND = 19;
cpdParser.OR = 20;
cpdParser.LT = 21;
cpdParser.LE = 22;
cpdParser.GT = 23;
cpdParser.GE = 24;
cpdParser.NE = 25;
cpdParser.EQ = 26;
cpdParser.TO = 27;
cpdParser.VARNAME = 28;
cpdParser.INT = 29;
cpdParser.EOL = 30;
cpdParser.WS = 31;

cpdParser.RULE_compileUnit = 0;
cpdParser.RULE_stmt = 1;
cpdParser.RULE_do_stmt = 2;
cpdParser.RULE_if_stmt = 3;
cpdParser.RULE_block_if_stmt = 4;
cpdParser.RULE_first_if_block = 5;
cpdParser.RULE_else_if_stmt = 6;
cpdParser.RULE_else_stmt = 7;
cpdParser.RULE_assign_stmt = 8;
cpdParser.RULE_expr = 9;
cpdParser.RULE_or_expr = 10;
cpdParser.RULE_and_expr = 11;
cpdParser.RULE_eq_expr = 12;
cpdParser.RULE_add_expr = 13;
cpdParser.RULE_mult_expr = 14;
cpdParser.RULE_final_expr = 15;
cpdParser.RULE_array = 16;

function CompileUnitContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_compileUnit;
    return this;
}

CompileUnitContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
CompileUnitContext.prototype.constructor = CompileUnitContext;

CompileUnitContext.prototype.EOF = function() {
    return this.getToken(cpdParser.EOF, 0);
};

CompileUnitContext.prototype.stmt = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(StmtContext);
    } else {
        return this.getTypedRuleContext(StmtContext,i);
    }
};

CompileUnitContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterCompileUnit(this);
	}
};

CompileUnitContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitCompileUnit(this);
	}
};

CompileUnitContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitCompileUnit(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.CompileUnitContext = CompileUnitContext;

cpdParser.prototype.compileUnit = function() {

    var localctx = new CompileUnitContext(this, this._ctx, this.state);
    this.enterRule(localctx, 0, cpdParser.RULE_compileUnit);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 35; 
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        do {
            this.state = 34;
            this.stmt();
            this.state = 37; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << cpdParser.IF) | (1 << cpdParser.DO) | (1 << cpdParser.VARNAME))) !== 0));
        this.state = 39;
        this.match(cpdParser.EOF);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function StmtContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_stmt;
    return this;
}

StmtContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
StmtContext.prototype.constructor = StmtContext;

StmtContext.prototype.do_stmt = function() {
    return this.getTypedRuleContext(Do_stmtContext,0);
};

StmtContext.prototype.EOL = function() {
    return this.getToken(cpdParser.EOL, 0);
};

StmtContext.prototype.if_stmt = function() {
    return this.getTypedRuleContext(If_stmtContext,0);
};

StmtContext.prototype.assign_stmt = function() {
    return this.getTypedRuleContext(Assign_stmtContext,0);
};

StmtContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterStmt(this);
	}
};

StmtContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitStmt(this);
	}
};

StmtContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitStmt(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.StmtContext = StmtContext;

cpdParser.prototype.stmt = function() {

    var localctx = new StmtContext(this, this._ctx, this.state);
    this.enterRule(localctx, 2, cpdParser.RULE_stmt);
    try {
        this.state = 50;
        this._errHandler.sync(this);
        switch(this._input.LA(1)) {
        case cpdParser.DO:
            this.enterOuterAlt(localctx, 1);
            this.state = 41;
            this.do_stmt();
            this.state = 42;
            this.match(cpdParser.EOL);
            break;
        case cpdParser.IF:
            this.enterOuterAlt(localctx, 2);
            this.state = 44;
            this.if_stmt();
            this.state = 45;
            this.match(cpdParser.EOL);
            break;
        case cpdParser.VARNAME:
            this.enterOuterAlt(localctx, 3);
            this.state = 47;
            this.assign_stmt();
            this.state = 48;
            this.match(cpdParser.EOL);
            break;
        default:
            throw new antlr4.error.NoViableAltException(this);
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function Do_stmtContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_do_stmt;
    return this;
}

Do_stmtContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
Do_stmtContext.prototype.constructor = Do_stmtContext;

Do_stmtContext.prototype.DO = function(i) {
	if(i===undefined) {
		i = null;
	}
    if(i===null) {
        return this.getTokens(cpdParser.DO);
    } else {
        return this.getToken(cpdParser.DO, i);
    }
};


Do_stmtContext.prototype.VARNAME = function() {
    return this.getToken(cpdParser.VARNAME, 0);
};

Do_stmtContext.prototype.EOL = function() {
    return this.getToken(cpdParser.EOL, 0);
};

Do_stmtContext.prototype.ENDDO = function() {
    return this.getToken(cpdParser.ENDDO, 0);
};

Do_stmtContext.prototype.ASSIGN = function() {
    return this.getToken(cpdParser.ASSIGN, 0);
};

Do_stmtContext.prototype.eq_expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(Eq_exprContext);
    } else {
        return this.getTypedRuleContext(Eq_exprContext,i);
    }
};

Do_stmtContext.prototype.COMMA = function(i) {
	if(i===undefined) {
		i = null;
	}
    if(i===null) {
        return this.getTokens(cpdParser.COMMA);
    } else {
        return this.getToken(cpdParser.COMMA, i);
    }
};


Do_stmtContext.prototype.stmt = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(StmtContext);
    } else {
        return this.getTypedRuleContext(StmtContext,i);
    }
};

Do_stmtContext.prototype.END = function() {
    return this.getToken(cpdParser.END, 0);
};

Do_stmtContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterDo_stmt(this);
	}
};

Do_stmtContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitDo_stmt(this);
	}
};

Do_stmtContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitDo_stmt(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.Do_stmtContext = Do_stmtContext;

cpdParser.prototype.do_stmt = function() {

    var localctx = new Do_stmtContext(this, this._ctx, this.state);
    this.enterRule(localctx, 4, cpdParser.RULE_do_stmt);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 52;
        this.match(cpdParser.DO);
        this.state = 53;
        this.match(cpdParser.VARNAME);
        this.state = 62;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        if(_la===cpdParser.ASSIGN) {
            this.state = 54;
            this.match(cpdParser.ASSIGN);
            this.state = 55;
            this.eq_expr();
            this.state = 56;
            this.match(cpdParser.COMMA);
            this.state = 57;
            this.eq_expr();
            this.state = 60;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===cpdParser.COMMA) {
                this.state = 58;
                this.match(cpdParser.COMMA);
                this.state = 59;
                this.eq_expr();
            }

        }

        this.state = 64;
        this.match(cpdParser.EOL);
        this.state = 66; 
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        do {
            this.state = 65;
            this.stmt();
            this.state = 68; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << cpdParser.IF) | (1 << cpdParser.DO) | (1 << cpdParser.VARNAME))) !== 0));
        this.state = 73;
        this._errHandler.sync(this);
        switch(this._input.LA(1)) {
        case cpdParser.ENDDO:
            this.state = 70;
            this.match(cpdParser.ENDDO);
            break;
        case cpdParser.END:
            this.state = 71;
            this.match(cpdParser.END);
            this.state = 72;
            this.match(cpdParser.DO);
            break;
        default:
            throw new antlr4.error.NoViableAltException(this);
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function If_stmtContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_if_stmt;
    return this;
}

If_stmtContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
If_stmtContext.prototype.constructor = If_stmtContext;

If_stmtContext.prototype.IF = function() {
    return this.getToken(cpdParser.IF, 0);
};

If_stmtContext.prototype.LPAR = function() {
    return this.getToken(cpdParser.LPAR, 0);
};

If_stmtContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

If_stmtContext.prototype.RPAR = function() {
    return this.getToken(cpdParser.RPAR, 0);
};

If_stmtContext.prototype.block_if_stmt = function() {
    return this.getTypedRuleContext(Block_if_stmtContext,0);
};

If_stmtContext.prototype.assign_stmt = function() {
    return this.getTypedRuleContext(Assign_stmtContext,0);
};

If_stmtContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterIf_stmt(this);
	}
};

If_stmtContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitIf_stmt(this);
	}
};

If_stmtContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitIf_stmt(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.If_stmtContext = If_stmtContext;

cpdParser.prototype.if_stmt = function() {

    var localctx = new If_stmtContext(this, this._ctx, this.state);
    this.enterRule(localctx, 6, cpdParser.RULE_if_stmt);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 75;
        this.match(cpdParser.IF);
        this.state = 76;
        this.match(cpdParser.LPAR);
        this.state = 77;
        this.expr();
        this.state = 78;
        this.match(cpdParser.RPAR);
        this.state = 81;
        this._errHandler.sync(this);
        switch(this._input.LA(1)) {
        case cpdParser.THEN:
            this.state = 79;
            this.block_if_stmt();
            break;
        case cpdParser.VARNAME:
            this.state = 80;
            this.assign_stmt();
            break;
        default:
            throw new antlr4.error.NoViableAltException(this);
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function Block_if_stmtContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_block_if_stmt;
    return this;
}

Block_if_stmtContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
Block_if_stmtContext.prototype.constructor = Block_if_stmtContext;

Block_if_stmtContext.prototype.first_if_block = function() {
    return this.getTypedRuleContext(First_if_blockContext,0);
};

Block_if_stmtContext.prototype.ENDIF = function() {
    return this.getToken(cpdParser.ENDIF, 0);
};

Block_if_stmtContext.prototype.else_if_stmt = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(Else_if_stmtContext);
    } else {
        return this.getTypedRuleContext(Else_if_stmtContext,i);
    }
};

Block_if_stmtContext.prototype.else_stmt = function() {
    return this.getTypedRuleContext(Else_stmtContext,0);
};

Block_if_stmtContext.prototype.END = function() {
    return this.getToken(cpdParser.END, 0);
};

Block_if_stmtContext.prototype.IF = function() {
    return this.getToken(cpdParser.IF, 0);
};

Block_if_stmtContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterBlock_if_stmt(this);
	}
};

Block_if_stmtContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitBlock_if_stmt(this);
	}
};

Block_if_stmtContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitBlock_if_stmt(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.Block_if_stmtContext = Block_if_stmtContext;

cpdParser.prototype.block_if_stmt = function() {

    var localctx = new Block_if_stmtContext(this, this._ctx, this.state);
    this.enterRule(localctx, 8, cpdParser.RULE_block_if_stmt);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 83;
        this.first_if_block();
        this.state = 87;
        this._errHandler.sync(this);
        var _alt = this._interp.adaptivePredict(this._input,7,this._ctx)
        while(_alt!=2 && _alt!=antlr4.atn.ATN.INVALID_ALT_NUMBER) {
            if(_alt===1) {
                this.state = 84;
                this.else_if_stmt(); 
            }
            this.state = 89;
            this._errHandler.sync(this);
            _alt = this._interp.adaptivePredict(this._input,7,this._ctx);
        }

        this.state = 91;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        if(_la===cpdParser.ELSE) {
            this.state = 90;
            this.else_stmt();
        }

        this.state = 96;
        this._errHandler.sync(this);
        switch(this._input.LA(1)) {
        case cpdParser.ENDIF:
            this.state = 93;
            this.match(cpdParser.ENDIF);
            break;
        case cpdParser.END:
            this.state = 94;
            this.match(cpdParser.END);
            this.state = 95;
            this.match(cpdParser.IF);
            break;
        default:
            throw new antlr4.error.NoViableAltException(this);
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function First_if_blockContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_first_if_block;
    return this;
}

First_if_blockContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
First_if_blockContext.prototype.constructor = First_if_blockContext;

First_if_blockContext.prototype.THEN = function() {
    return this.getToken(cpdParser.THEN, 0);
};

First_if_blockContext.prototype.EOL = function() {
    return this.getToken(cpdParser.EOL, 0);
};

First_if_blockContext.prototype.stmt = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(StmtContext);
    } else {
        return this.getTypedRuleContext(StmtContext,i);
    }
};

First_if_blockContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterFirst_if_block(this);
	}
};

First_if_blockContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitFirst_if_block(this);
	}
};

First_if_blockContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitFirst_if_block(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.First_if_blockContext = First_if_blockContext;

cpdParser.prototype.first_if_block = function() {

    var localctx = new First_if_blockContext(this, this._ctx, this.state);
    this.enterRule(localctx, 10, cpdParser.RULE_first_if_block);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 98;
        this.match(cpdParser.THEN);
        this.state = 99;
        this.match(cpdParser.EOL);
        this.state = 101; 
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        do {
            this.state = 100;
            this.stmt();
            this.state = 103; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << cpdParser.IF) | (1 << cpdParser.DO) | (1 << cpdParser.VARNAME))) !== 0));
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function Else_if_stmtContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_else_if_stmt;
    return this;
}

Else_if_stmtContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
Else_if_stmtContext.prototype.constructor = Else_if_stmtContext;

Else_if_stmtContext.prototype.LPAR = function() {
    return this.getToken(cpdParser.LPAR, 0);
};

Else_if_stmtContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

Else_if_stmtContext.prototype.RPAR = function() {
    return this.getToken(cpdParser.RPAR, 0);
};

Else_if_stmtContext.prototype.THEN = function() {
    return this.getToken(cpdParser.THEN, 0);
};

Else_if_stmtContext.prototype.EOL = function() {
    return this.getToken(cpdParser.EOL, 0);
};

Else_if_stmtContext.prototype.ELSEIF = function() {
    return this.getToken(cpdParser.ELSEIF, 0);
};

Else_if_stmtContext.prototype.stmt = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(StmtContext);
    } else {
        return this.getTypedRuleContext(StmtContext,i);
    }
};

Else_if_stmtContext.prototype.ELSE = function() {
    return this.getToken(cpdParser.ELSE, 0);
};

Else_if_stmtContext.prototype.IF = function() {
    return this.getToken(cpdParser.IF, 0);
};

Else_if_stmtContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterElse_if_stmt(this);
	}
};

Else_if_stmtContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitElse_if_stmt(this);
	}
};

Else_if_stmtContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitElse_if_stmt(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.Else_if_stmtContext = Else_if_stmtContext;

cpdParser.prototype.else_if_stmt = function() {

    var localctx = new Else_if_stmtContext(this, this._ctx, this.state);
    this.enterRule(localctx, 12, cpdParser.RULE_else_if_stmt);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 108;
        this._errHandler.sync(this);
        switch(this._input.LA(1)) {
        case cpdParser.ELSEIF:
            this.state = 105;
            this.match(cpdParser.ELSEIF);
            break;
        case cpdParser.ELSE:
            this.state = 106;
            this.match(cpdParser.ELSE);
            this.state = 107;
            this.match(cpdParser.IF);
            break;
        default:
            throw new antlr4.error.NoViableAltException(this);
        }
        this.state = 110;
        this.match(cpdParser.LPAR);
        this.state = 111;
        this.expr();
        this.state = 112;
        this.match(cpdParser.RPAR);
        this.state = 113;
        this.match(cpdParser.THEN);
        this.state = 114;
        this.match(cpdParser.EOL);
        this.state = 116; 
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        do {
            this.state = 115;
            this.stmt();
            this.state = 118; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << cpdParser.IF) | (1 << cpdParser.DO) | (1 << cpdParser.VARNAME))) !== 0));
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function Else_stmtContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_else_stmt;
    return this;
}

Else_stmtContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
Else_stmtContext.prototype.constructor = Else_stmtContext;

Else_stmtContext.prototype.ELSE = function() {
    return this.getToken(cpdParser.ELSE, 0);
};

Else_stmtContext.prototype.EOL = function() {
    return this.getToken(cpdParser.EOL, 0);
};

Else_stmtContext.prototype.stmt = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(StmtContext);
    } else {
        return this.getTypedRuleContext(StmtContext,i);
    }
};

Else_stmtContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterElse_stmt(this);
	}
};

Else_stmtContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitElse_stmt(this);
	}
};

Else_stmtContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitElse_stmt(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.Else_stmtContext = Else_stmtContext;

cpdParser.prototype.else_stmt = function() {

    var localctx = new Else_stmtContext(this, this._ctx, this.state);
    this.enterRule(localctx, 14, cpdParser.RULE_else_stmt);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 120;
        this.match(cpdParser.ELSE);
        this.state = 121;
        this.match(cpdParser.EOL);
        this.state = 123; 
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        do {
            this.state = 122;
            this.stmt();
            this.state = 125; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << cpdParser.IF) | (1 << cpdParser.DO) | (1 << cpdParser.VARNAME))) !== 0));
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function Assign_stmtContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_assign_stmt;
    return this;
}

Assign_stmtContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
Assign_stmtContext.prototype.constructor = Assign_stmtContext;

Assign_stmtContext.prototype.array = function() {
    return this.getTypedRuleContext(ArrayContext,0);
};

Assign_stmtContext.prototype.ASSIGN = function() {
    return this.getToken(cpdParser.ASSIGN, 0);
};

Assign_stmtContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

Assign_stmtContext.prototype.VARNAME = function() {
    return this.getToken(cpdParser.VARNAME, 0);
};

Assign_stmtContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterAssign_stmt(this);
	}
};

Assign_stmtContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitAssign_stmt(this);
	}
};

Assign_stmtContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitAssign_stmt(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.Assign_stmtContext = Assign_stmtContext;

cpdParser.prototype.assign_stmt = function() {

    var localctx = new Assign_stmtContext(this, this._ctx, this.state);
    this.enterRule(localctx, 16, cpdParser.RULE_assign_stmt);
    try {
        this.state = 134;
        this._errHandler.sync(this);
        var la_ = this._interp.adaptivePredict(this._input,14,this._ctx);
        switch(la_) {
        case 1:
            this.enterOuterAlt(localctx, 1);
            this.state = 127;
            this.array();
            this.state = 128;
            this.match(cpdParser.ASSIGN);
            this.state = 129;
            this.expr();
            break;

        case 2:
            this.enterOuterAlt(localctx, 2);
            this.state = 131;
            this.match(cpdParser.VARNAME);
            this.state = 132;
            this.match(cpdParser.ASSIGN);
            this.state = 133;
            this.expr();
            break;

        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function ExprContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_expr;
    return this;
}

ExprContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ExprContext.prototype.constructor = ExprContext;

ExprContext.prototype.or_expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(Or_exprContext);
    } else {
        return this.getTypedRuleContext(Or_exprContext,i);
    }
};

ExprContext.prototype.OR = function(i) {
	if(i===undefined) {
		i = null;
	}
    if(i===null) {
        return this.getTokens(cpdParser.OR);
    } else {
        return this.getToken(cpdParser.OR, i);
    }
};


ExprContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterExpr(this);
	}
};

ExprContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitExpr(this);
	}
};

ExprContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitExpr(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.ExprContext = ExprContext;

cpdParser.prototype.expr = function() {

    var localctx = new ExprContext(this, this._ctx, this.state);
    this.enterRule(localctx, 18, cpdParser.RULE_expr);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 136;
        this.or_expr();
        this.state = 141;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===cpdParser.OR) {
            this.state = 137;
            this.match(cpdParser.OR);
            this.state = 138;
            this.or_expr();
            this.state = 143;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function Or_exprContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_or_expr;
    return this;
}

Or_exprContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
Or_exprContext.prototype.constructor = Or_exprContext;

Or_exprContext.prototype.and_expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(And_exprContext);
    } else {
        return this.getTypedRuleContext(And_exprContext,i);
    }
};

Or_exprContext.prototype.AND = function(i) {
	if(i===undefined) {
		i = null;
	}
    if(i===null) {
        return this.getTokens(cpdParser.AND);
    } else {
        return this.getToken(cpdParser.AND, i);
    }
};


Or_exprContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterOr_expr(this);
	}
};

Or_exprContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitOr_expr(this);
	}
};

Or_exprContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitOr_expr(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.Or_exprContext = Or_exprContext;

cpdParser.prototype.or_expr = function() {

    var localctx = new Or_exprContext(this, this._ctx, this.state);
    this.enterRule(localctx, 20, cpdParser.RULE_or_expr);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 144;
        this.and_expr();
        this.state = 149;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===cpdParser.AND) {
            this.state = 145;
            this.match(cpdParser.AND);
            this.state = 146;
            this.and_expr();
            this.state = 151;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function And_exprContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_and_expr;
    return this;
}

And_exprContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
And_exprContext.prototype.constructor = And_exprContext;

And_exprContext.prototype.eq_expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(Eq_exprContext);
    } else {
        return this.getTypedRuleContext(Eq_exprContext,i);
    }
};

And_exprContext.prototype.LT = function() {
    return this.getToken(cpdParser.LT, 0);
};

And_exprContext.prototype.LE = function() {
    return this.getToken(cpdParser.LE, 0);
};

And_exprContext.prototype.EQ = function() {
    return this.getToken(cpdParser.EQ, 0);
};

And_exprContext.prototype.NE = function() {
    return this.getToken(cpdParser.NE, 0);
};

And_exprContext.prototype.GT = function() {
    return this.getToken(cpdParser.GT, 0);
};

And_exprContext.prototype.GE = function() {
    return this.getToken(cpdParser.GE, 0);
};

And_exprContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterAnd_expr(this);
	}
};

And_exprContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitAnd_expr(this);
	}
};

And_exprContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitAnd_expr(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.And_exprContext = And_exprContext;

cpdParser.prototype.and_expr = function() {

    var localctx = new And_exprContext(this, this._ctx, this.state);
    this.enterRule(localctx, 22, cpdParser.RULE_and_expr);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 152;
        this.eq_expr();
        this.state = 155;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        if((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << cpdParser.LT) | (1 << cpdParser.LE) | (1 << cpdParser.GT) | (1 << cpdParser.GE) | (1 << cpdParser.NE) | (1 << cpdParser.EQ))) !== 0)) {
            this.state = 153;
            _la = this._input.LA(1);
            if(!((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << cpdParser.LT) | (1 << cpdParser.LE) | (1 << cpdParser.GT) | (1 << cpdParser.GE) | (1 << cpdParser.NE) | (1 << cpdParser.EQ))) !== 0))) {
            this._errHandler.recoverInline(this);
            }
            else {
            	this._errHandler.reportMatch(this);
                this.consume();
            }
            this.state = 154;
            this.eq_expr();
        }

    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function Eq_exprContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_eq_expr;
    return this;
}

Eq_exprContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
Eq_exprContext.prototype.constructor = Eq_exprContext;

Eq_exprContext.prototype.add_expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(Add_exprContext);
    } else {
        return this.getTypedRuleContext(Add_exprContext,i);
    }
};

Eq_exprContext.prototype.PLUS = function(i) {
	if(i===undefined) {
		i = null;
	}
    if(i===null) {
        return this.getTokens(cpdParser.PLUS);
    } else {
        return this.getToken(cpdParser.PLUS, i);
    }
};


Eq_exprContext.prototype.MINUS = function(i) {
	if(i===undefined) {
		i = null;
	}
    if(i===null) {
        return this.getTokens(cpdParser.MINUS);
    } else {
        return this.getToken(cpdParser.MINUS, i);
    }
};


Eq_exprContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterEq_expr(this);
	}
};

Eq_exprContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitEq_expr(this);
	}
};

Eq_exprContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitEq_expr(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.Eq_exprContext = Eq_exprContext;

cpdParser.prototype.eq_expr = function() {

    var localctx = new Eq_exprContext(this, this._ctx, this.state);
    this.enterRule(localctx, 24, cpdParser.RULE_eq_expr);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 157;
        this.add_expr();
        this.state = 162;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===cpdParser.MINUS || _la===cpdParser.PLUS) {
            this.state = 158;
            _la = this._input.LA(1);
            if(!(_la===cpdParser.MINUS || _la===cpdParser.PLUS)) {
            this._errHandler.recoverInline(this);
            }
            else {
            	this._errHandler.reportMatch(this);
                this.consume();
            }
            this.state = 159;
            this.add_expr();
            this.state = 164;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function Add_exprContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_add_expr;
    return this;
}

Add_exprContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
Add_exprContext.prototype.constructor = Add_exprContext;

Add_exprContext.prototype.mult_expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(Mult_exprContext);
    } else {
        return this.getTypedRuleContext(Mult_exprContext,i);
    }
};

Add_exprContext.prototype.STAR = function(i) {
	if(i===undefined) {
		i = null;
	}
    if(i===null) {
        return this.getTokens(cpdParser.STAR);
    } else {
        return this.getToken(cpdParser.STAR, i);
    }
};


Add_exprContext.prototype.DIV = function(i) {
	if(i===undefined) {
		i = null;
	}
    if(i===null) {
        return this.getTokens(cpdParser.DIV);
    } else {
        return this.getToken(cpdParser.DIV, i);
    }
};


Add_exprContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterAdd_expr(this);
	}
};

Add_exprContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitAdd_expr(this);
	}
};

Add_exprContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitAdd_expr(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.Add_exprContext = Add_exprContext;

cpdParser.prototype.add_expr = function() {

    var localctx = new Add_exprContext(this, this._ctx, this.state);
    this.enterRule(localctx, 26, cpdParser.RULE_add_expr);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 165;
        this.mult_expr();
        this.state = 170;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===cpdParser.DIV || _la===cpdParser.STAR) {
            this.state = 166;
            _la = this._input.LA(1);
            if(!(_la===cpdParser.DIV || _la===cpdParser.STAR)) {
            this._errHandler.recoverInline(this);
            }
            else {
            	this._errHandler.reportMatch(this);
                this.consume();
            }
            this.state = 167;
            this.mult_expr();
            this.state = 172;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function Mult_exprContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_mult_expr;
    return this;
}

Mult_exprContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
Mult_exprContext.prototype.constructor = Mult_exprContext;

Mult_exprContext.prototype.final_expr = function() {
    return this.getTypedRuleContext(Final_exprContext,0);
};

Mult_exprContext.prototype.PLUS = function(i) {
	if(i===undefined) {
		i = null;
	}
    if(i===null) {
        return this.getTokens(cpdParser.PLUS);
    } else {
        return this.getToken(cpdParser.PLUS, i);
    }
};


Mult_exprContext.prototype.MINUS = function(i) {
	if(i===undefined) {
		i = null;
	}
    if(i===null) {
        return this.getTokens(cpdParser.MINUS);
    } else {
        return this.getToken(cpdParser.MINUS, i);
    }
};


Mult_exprContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterMult_expr(this);
	}
};

Mult_exprContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitMult_expr(this);
	}
};

Mult_exprContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitMult_expr(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.Mult_exprContext = Mult_exprContext;

cpdParser.prototype.mult_expr = function() {

    var localctx = new Mult_exprContext(this, this._ctx, this.state);
    this.enterRule(localctx, 28, cpdParser.RULE_mult_expr);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 176;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===cpdParser.MINUS || _la===cpdParser.PLUS) {
            this.state = 173;
            _la = this._input.LA(1);
            if(!(_la===cpdParser.MINUS || _la===cpdParser.PLUS)) {
            this._errHandler.recoverInline(this);
            }
            else {
            	this._errHandler.reportMatch(this);
                this.consume();
            }
            this.state = 178;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 179;
        this.final_expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function Final_exprContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_final_expr;
    return this;
}

Final_exprContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
Final_exprContext.prototype.constructor = Final_exprContext;

Final_exprContext.prototype.array = function() {
    return this.getTypedRuleContext(ArrayContext,0);
};

Final_exprContext.prototype.VARNAME = function() {
    return this.getToken(cpdParser.VARNAME, 0);
};

Final_exprContext.prototype.INT = function() {
    return this.getToken(cpdParser.INT, 0);
};

Final_exprContext.prototype.LPAR = function() {
    return this.getToken(cpdParser.LPAR, 0);
};

Final_exprContext.prototype.eq_expr = function() {
    return this.getTypedRuleContext(Eq_exprContext,0);
};

Final_exprContext.prototype.RPAR = function() {
    return this.getToken(cpdParser.RPAR, 0);
};

Final_exprContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterFinal_expr(this);
	}
};

Final_exprContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitFinal_expr(this);
	}
};

Final_exprContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitFinal_expr(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.Final_exprContext = Final_exprContext;

cpdParser.prototype.final_expr = function() {

    var localctx = new Final_exprContext(this, this._ctx, this.state);
    this.enterRule(localctx, 30, cpdParser.RULE_final_expr);
    try {
        this.state = 188;
        this._errHandler.sync(this);
        var la_ = this._interp.adaptivePredict(this._input,21,this._ctx);
        switch(la_) {
        case 1:
            this.enterOuterAlt(localctx, 1);
            this.state = 181;
            this.array();
            break;

        case 2:
            this.enterOuterAlt(localctx, 2);
            this.state = 182;
            this.match(cpdParser.VARNAME);
            break;

        case 3:
            this.enterOuterAlt(localctx, 3);
            this.state = 183;
            this.match(cpdParser.INT);
            break;

        case 4:
            this.enterOuterAlt(localctx, 4);
            this.state = 184;
            this.match(cpdParser.LPAR);
            this.state = 185;
            this.eq_expr();
            this.state = 186;
            this.match(cpdParser.RPAR);
            break;

        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function ArrayContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_array;
    return this;
}

ArrayContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ArrayContext.prototype.constructor = ArrayContext;

ArrayContext.prototype.VARNAME = function() {
    return this.getToken(cpdParser.VARNAME, 0);
};

ArrayContext.prototype.LPAR = function() {
    return this.getToken(cpdParser.LPAR, 0);
};

ArrayContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};

ArrayContext.prototype.RPAR = function() {
    return this.getToken(cpdParser.RPAR, 0);
};

ArrayContext.prototype.COMMA = function(i) {
	if(i===undefined) {
		i = null;
	}
    if(i===null) {
        return this.getTokens(cpdParser.COMMA);
    } else {
        return this.getToken(cpdParser.COMMA, i);
    }
};


ArrayContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterArray(this);
	}
};

ArrayContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitArray(this);
	}
};

ArrayContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitArray(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.ArrayContext = ArrayContext;

cpdParser.prototype.array = function() {

    var localctx = new ArrayContext(this, this._ctx, this.state);
    this.enterRule(localctx, 32, cpdParser.RULE_array);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 190;
        this.match(cpdParser.VARNAME);
        this.state = 191;
        this.match(cpdParser.LPAR);
        this.state = 192;
        this.expr();
        this.state = 197;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===cpdParser.COMMA) {
            this.state = 193;
            this.match(cpdParser.COMMA);
            this.state = 194;
            this.expr();
            this.state = 199;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 200;
        this.match(cpdParser.RPAR);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


exports.cpdParser = cpdParser;
