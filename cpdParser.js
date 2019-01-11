// Generated from cpd.g4 by ANTLR 4.7.2
// jshint ignore: start
var antlr4 = require('antlr4/index');
var cpdListener = require('./cpdListener').cpdListener;
var cpdVisitor = require('./cpdVisitor').cpdVisitor;

var grammarFileName = "cpd.g4";


var serializedATN = ["\u0003\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964",
    "\u0003\"\u00dd\u0004\u0002\t\u0002\u0004\u0003\t\u0003\u0004\u0004\t",
    "\u0004\u0004\u0005\t\u0005\u0004\u0006\t\u0006\u0004\u0007\t\u0007\u0004",
    "\b\t\b\u0004\t\t\t\u0004\n\t\n\u0004\u000b\t\u000b\u0004\f\t\f\u0004",
    "\r\t\r\u0004\u000e\t\u000e\u0004\u000f\t\u000f\u0004\u0010\t\u0010\u0004",
    "\u0011\t\u0011\u0004\u0012\t\u0012\u0004\u0013\t\u0013\u0004\u0014\t",
    "\u0014\u0004\u0015\t\u0015\u0003\u0002\u0006\u0002,\n\u0002\r\u0002",
    "\u000e\u0002-\u0003\u0002\u0003\u0002\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0005\u0003>\n\u0003\u0003",
    "\u0004\u0003\u0004\u0003\u0004\u0003\u0004\u0003\u0004\u0003\u0004\u0003",
    "\u0004\u0003\u0004\u0005\u0004H\n\u0004\u0005\u0004J\n\u0004\u0003\u0004",
    "\u0003\u0004\u0006\u0004N\n\u0004\r\u0004\u000e\u0004O\u0003\u0004\u0003",
    "\u0004\u0003\u0004\u0005\u0004U\n\u0004\u0003\u0005\u0003\u0005\u0003",
    "\u0005\u0003\u0005\u0003\u0005\u0003\u0005\u0005\u0005]\n\u0005\u0003",
    "\u0006\u0003\u0006\u0007\u0006a\n\u0006\f\u0006\u000e\u0006d\u000b\u0006",
    "\u0003\u0006\u0005\u0006g\n\u0006\u0003\u0006\u0003\u0006\u0003\u0006",
    "\u0005\u0006l\n\u0006\u0003\u0007\u0003\u0007\u0003\u0007\u0006\u0007",
    "q\n\u0007\r\u0007\u000e\u0007r\u0003\b\u0003\b\u0003\b\u0005\bx\n\b",
    "\u0003\b\u0003\b\u0003\b\u0003\b\u0003\b\u0003\b\u0006\b\u0080\n\b\r",
    "\b\u000e\b\u0081\u0003\t\u0003\t\u0003\t\u0006\t\u0087\n\t\r\t\u000e",
    "\t\u0088\u0003\n\u0003\n\u0003\n\u0003\n\u0003\n\u0003\n\u0003\n\u0003",
    "\n\u0005\n\u0093\n\n\u0003\u000b\u0003\u000b\u0003\f\u0003\f\u0003\f",
    "\u0007\f\u009a\n\f\f\f\u000e\f\u009d\u000b\f\u0003\r\u0003\r\u0003\r",
    "\u0007\r\u00a2\n\r\f\r\u000e\r\u00a5\u000b\r\u0003\u000e\u0003\u000e",
    "\u0003\u000e\u0005\u000e\u00aa\n\u000e\u0003\u000f\u0003\u000f\u0003",
    "\u000f\u0007\u000f\u00af\n\u000f\f\u000f\u000e\u000f\u00b2\u000b\u000f",
    "\u0003\u0010\u0003\u0010\u0003\u0010\u0007\u0010\u00b7\n\u0010\f\u0010",
    "\u000e\u0010\u00ba\u000b\u0010\u0003\u0011\u0007\u0011\u00bd\n\u0011",
    "\f\u0011\u000e\u0011\u00c0\u000b\u0011\u0003\u0011\u0003\u0011\u0003",
    "\u0012\u0003\u0012\u0003\u0012\u0003\u0012\u0003\u0012\u0003\u0012\u0003",
    "\u0012\u0005\u0012\u00cb\n\u0012\u0003\u0013\u0003\u0013\u0003\u0013",
    "\u0003\u0013\u0003\u0013\u0007\u0013\u00d2\n\u0013\f\u0013\u000e\u0013",
    "\u00d5\u000b\u0013\u0003\u0013\u0003\u0013\u0003\u0014\u0003\u0014\u0003",
    "\u0015\u0003\u0015\u0003\u0015\u0002\u0002\u0016\u0002\u0004\u0006\b",
    "\n\f\u000e\u0010\u0012\u0014\u0016\u0018\u001a\u001c\u001e \"$&(\u0002",
    "\u0005\u0003\u0002\u0018\u001d\u0003\u0002\u0010\u0011\u0003\u0002\u0012",
    "\u0014\u0002\u00e3\u0002+\u0003\u0002\u0002\u0002\u0004=\u0003\u0002",
    "\u0002\u0002\u0006?\u0003\u0002\u0002\u0002\bV\u0003\u0002\u0002\u0002",
    "\n^\u0003\u0002\u0002\u0002\fm\u0003\u0002\u0002\u0002\u000ew\u0003",
    "\u0002\u0002\u0002\u0010\u0083\u0003\u0002\u0002\u0002\u0012\u0092\u0003",
    "\u0002\u0002\u0002\u0014\u0094\u0003\u0002\u0002\u0002\u0016\u0096\u0003",
    "\u0002\u0002\u0002\u0018\u009e\u0003\u0002\u0002\u0002\u001a\u00a6\u0003",
    "\u0002\u0002\u0002\u001c\u00ab\u0003\u0002\u0002\u0002\u001e\u00b3\u0003",
    "\u0002\u0002\u0002 \u00be\u0003\u0002\u0002\u0002\"\u00ca\u0003\u0002",
    "\u0002\u0002$\u00cc\u0003\u0002\u0002\u0002&\u00d8\u0003\u0002\u0002",
    "\u0002(\u00da\u0003\u0002\u0002\u0002*,\u0005\u0004\u0003\u0002+*\u0003",
    "\u0002\u0002\u0002,-\u0003\u0002\u0002\u0002-+\u0003\u0002\u0002\u0002",
    "-.\u0003\u0002\u0002\u0002./\u0003\u0002\u0002\u0002/0\u0007\u0002\u0002",
    "\u00030\u0003\u0003\u0002\u0002\u000212\u0005\u0006\u0004\u000223\u0007",
    "!\u0002\u00023>\u0003\u0002\u0002\u000245\u0005\b\u0005\u000256\u0007",
    "!\u0002\u00026>\u0003\u0002\u0002\u000278\u0005\u0012\n\u000289\u0007",
    "!\u0002\u00029>\u0003\u0002\u0002\u0002:;\u0005\u0014\u000b\u0002;<",
    "\u0007!\u0002\u0002<>\u0003\u0002\u0002\u0002=1\u0003\u0002\u0002\u0002",
    "=4\u0003\u0002\u0002\u0002=7\u0003\u0002\u0002\u0002=:\u0003\u0002\u0002",
    "\u0002>\u0005\u0003\u0002\u0002\u0002?@\u0007\t\u0002\u0002@I\u0005",
    "&\u0014\u0002AB\u0007\u000f\u0002\u0002BC\u0005\u001c\u000f\u0002CD",
    "\u0007\u000b\u0002\u0002DG\u0005\u001c\u000f\u0002EF\u0007\u000b\u0002",
    "\u0002FH\u0005\u001c\u000f\u0002GE\u0003\u0002\u0002\u0002GH\u0003\u0002",
    "\u0002\u0002HJ\u0003\u0002\u0002\u0002IA\u0003\u0002\u0002\u0002IJ\u0003",
    "\u0002\u0002\u0002JK\u0003\u0002\u0002\u0002KM\u0007!\u0002\u0002LN",
    "\u0005\u0004\u0003\u0002ML\u0003\u0002\u0002\u0002NO\u0003\u0002\u0002",
    "\u0002OM\u0003\u0002\u0002\u0002OP\u0003\u0002\u0002\u0002PT\u0003\u0002",
    "\u0002\u0002QU\u0007\n\u0002\u0002RS\u0007\u0003\u0002\u0002SU\u0007",
    "\t\u0002\u0002TQ\u0003\u0002\u0002\u0002TR\u0003\u0002\u0002\u0002U",
    "\u0007\u0003\u0002\u0002\u0002VW\u0007\u0004\u0002\u0002WX\u0007\f\u0002",
    "\u0002XY\u0005\u0016\f\u0002Y\\\u0007\r\u0002\u0002Z]\u0005\n\u0006",
    "\u0002[]\u0005\u0012\n\u0002\\Z\u0003\u0002\u0002\u0002\\[\u0003\u0002",
    "\u0002\u0002]\t\u0003\u0002\u0002\u0002^b\u0005\f\u0007\u0002_a\u0005",
    "\u000e\b\u0002`_\u0003\u0002\u0002\u0002ad\u0003\u0002\u0002\u0002b",
    "`\u0003\u0002\u0002\u0002bc\u0003\u0002\u0002\u0002cf\u0003\u0002\u0002",
    "\u0002db\u0003\u0002\u0002\u0002eg\u0005\u0010\t\u0002fe\u0003\u0002",
    "\u0002\u0002fg\u0003\u0002\u0002\u0002gk\u0003\u0002\u0002\u0002hl\u0007",
    "\u0007\u0002\u0002ij\u0007\u0003\u0002\u0002jl\u0007\u0004\u0002\u0002",
    "kh\u0003\u0002\u0002\u0002ki\u0003\u0002\u0002\u0002l\u000b\u0003\u0002",
    "\u0002\u0002mn\u0007\u0005\u0002\u0002np\u0007!\u0002\u0002oq\u0005",
    "\u0004\u0003\u0002po\u0003\u0002\u0002\u0002qr\u0003\u0002\u0002\u0002",
    "rp\u0003\u0002\u0002\u0002rs\u0003\u0002\u0002\u0002s\r\u0003\u0002",
    "\u0002\u0002tx\u0007\b\u0002\u0002uv\u0007\u0006\u0002\u0002vx\u0007",
    "\u0004\u0002\u0002wt\u0003\u0002\u0002\u0002wu\u0003\u0002\u0002\u0002",
    "xy\u0003\u0002\u0002\u0002yz\u0007\f\u0002\u0002z{\u0005\u0016\f\u0002",
    "{|\u0007\r\u0002\u0002|}\u0007\u0005\u0002\u0002}\u007f\u0007!\u0002",
    "\u0002~\u0080\u0005\u0004\u0003\u0002\u007f~\u0003\u0002\u0002\u0002",
    "\u0080\u0081\u0003\u0002\u0002\u0002\u0081\u007f\u0003\u0002\u0002\u0002",
    "\u0081\u0082\u0003\u0002\u0002\u0002\u0082\u000f\u0003\u0002\u0002\u0002",
    "\u0083\u0084\u0007\u0006\u0002\u0002\u0084\u0086\u0007!\u0002\u0002",
    "\u0085\u0087\u0005\u0004\u0003\u0002\u0086\u0085\u0003\u0002\u0002\u0002",
    "\u0087\u0088\u0003\u0002\u0002\u0002\u0088\u0086\u0003\u0002\u0002\u0002",
    "\u0088\u0089\u0003\u0002\u0002\u0002\u0089\u0011\u0003\u0002\u0002\u0002",
    "\u008a\u008b\u0005$\u0013\u0002\u008b\u008c\u0007\u000f\u0002\u0002",
    "\u008c\u008d\u0005\u0016\f\u0002\u008d\u0093\u0003\u0002\u0002\u0002",
    "\u008e\u008f\u0005&\u0014\u0002\u008f\u0090\u0007\u000f\u0002\u0002",
    "\u0090\u0091\u0005\u0016\f\u0002\u0091\u0093\u0003\u0002\u0002\u0002",
    "\u0092\u008a\u0003\u0002\u0002\u0002\u0092\u008e\u0003\u0002\u0002\u0002",
    "\u0093\u0013\u0003\u0002\u0002\u0002\u0094\u0095\u0003\u0002\u0002\u0002",
    "\u0095\u0015\u0003\u0002\u0002\u0002\u0096\u009b\u0005\u0018\r\u0002",
    "\u0097\u0098\u0007\u0017\u0002\u0002\u0098\u009a\u0005\u0018\r\u0002",
    "\u0099\u0097\u0003\u0002\u0002\u0002\u009a\u009d\u0003\u0002\u0002\u0002",
    "\u009b\u0099\u0003\u0002\u0002\u0002\u009b\u009c\u0003\u0002\u0002\u0002",
    "\u009c\u0017\u0003\u0002\u0002\u0002\u009d\u009b\u0003\u0002\u0002\u0002",
    "\u009e\u00a3\u0005\u001a\u000e\u0002\u009f\u00a0\u0007\u0016\u0002\u0002",
    "\u00a0\u00a2\u0005\u001a\u000e\u0002\u00a1\u009f\u0003\u0002\u0002\u0002",
    "\u00a2\u00a5\u0003\u0002\u0002\u0002\u00a3\u00a1\u0003\u0002\u0002\u0002",
    "\u00a3\u00a4\u0003\u0002\u0002\u0002\u00a4\u0019\u0003\u0002\u0002\u0002",
    "\u00a5\u00a3\u0003\u0002\u0002\u0002\u00a6\u00a9\u0005\u001c\u000f\u0002",
    "\u00a7\u00a8\t\u0002\u0002\u0002\u00a8\u00aa\u0005\u001c\u000f\u0002",
    "\u00a9\u00a7\u0003\u0002\u0002\u0002\u00a9\u00aa\u0003\u0002\u0002\u0002",
    "\u00aa\u001b\u0003\u0002\u0002\u0002\u00ab\u00b0\u0005\u001e\u0010\u0002",
    "\u00ac\u00ad\t\u0003\u0002\u0002\u00ad\u00af\u0005\u001e\u0010\u0002",
    "\u00ae\u00ac\u0003\u0002\u0002\u0002\u00af\u00b2\u0003\u0002\u0002\u0002",
    "\u00b0\u00ae\u0003\u0002\u0002\u0002\u00b0\u00b1\u0003\u0002\u0002\u0002",
    "\u00b1\u001d\u0003\u0002\u0002\u0002\u00b2\u00b0\u0003\u0002\u0002\u0002",
    "\u00b3\u00b8\u0005 \u0011\u0002\u00b4\u00b5\t\u0004\u0002\u0002\u00b5",
    "\u00b7\u0005 \u0011\u0002\u00b6\u00b4\u0003\u0002\u0002\u0002\u00b7",
    "\u00ba\u0003\u0002\u0002\u0002\u00b8\u00b6\u0003\u0002\u0002\u0002\u00b8",
    "\u00b9\u0003\u0002\u0002\u0002\u00b9\u001f\u0003\u0002\u0002\u0002\u00ba",
    "\u00b8\u0003\u0002\u0002\u0002\u00bb\u00bd\t\u0003\u0002\u0002\u00bc",
    "\u00bb\u0003\u0002\u0002\u0002\u00bd\u00c0\u0003\u0002\u0002\u0002\u00be",
    "\u00bc\u0003\u0002\u0002\u0002\u00be\u00bf\u0003\u0002\u0002\u0002\u00bf",
    "\u00c1\u0003\u0002\u0002\u0002\u00c0\u00be\u0003\u0002\u0002\u0002\u00c1",
    "\u00c2\u0005\"\u0012\u0002\u00c2!\u0003\u0002\u0002\u0002\u00c3\u00cb",
    "\u0005$\u0013\u0002\u00c4\u00cb\u0005&\u0014\u0002\u00c5\u00cb\u0005",
    "(\u0015\u0002\u00c6\u00c7\u0007\f\u0002\u0002\u00c7\u00c8\u0005\u001c",
    "\u000f\u0002\u00c8\u00c9\u0007\r\u0002\u0002\u00c9\u00cb\u0003\u0002",
    "\u0002\u0002\u00ca\u00c3\u0003\u0002\u0002\u0002\u00ca\u00c4\u0003\u0002",
    "\u0002\u0002\u00ca\u00c5\u0003\u0002\u0002\u0002\u00ca\u00c6\u0003\u0002",
    "\u0002\u0002\u00cb#\u0003\u0002\u0002\u0002\u00cc\u00cd\u0007\u001f",
    "\u0002\u0002\u00cd\u00ce\u0007\f\u0002\u0002\u00ce\u00d3\u0005\u0016",
    "\f\u0002\u00cf\u00d0\u0007\u000b\u0002\u0002\u00d0\u00d2\u0005\u0016",
    "\f\u0002\u00d1\u00cf\u0003\u0002\u0002\u0002\u00d2\u00d5\u0003\u0002",
    "\u0002\u0002\u00d3\u00d1\u0003\u0002\u0002\u0002\u00d3\u00d4\u0003\u0002",
    "\u0002\u0002\u00d4\u00d6\u0003\u0002\u0002\u0002\u00d5\u00d3\u0003\u0002",
    "\u0002\u0002\u00d6\u00d7\u0007\r\u0002\u0002\u00d7%\u0003\u0002\u0002",
    "\u0002\u00d8\u00d9\u0007\u001f\u0002\u0002\u00d9\'\u0003\u0002\u0002",
    "\u0002\u00da\u00db\u0007 \u0002\u0002\u00db)\u0003\u0002\u0002\u0002",
    "\u0019-=GIOT\\bfkrw\u0081\u0088\u0092\u009b\u00a3\u00a9\u00b0\u00b8",
    "\u00be\u00ca\u00d3"].join("");


var atn = new antlr4.atn.ATNDeserializer().deserialize(serializedATN);

var decisionsToDFA = atn.decisionToState.map( function(ds, index) { return new antlr4.dfa.DFA(ds, index); });

var sharedContextCache = new antlr4.PredictionContextCache();

var literalNames = [ null, null, null, null, null, null, null, null, null, 
                     "','", "'('", "')'", "':'", "'='", null, "'+'", "'/'", 
                     "'*'", "'%'", null, null, null, null, null, null, null, 
                     null, null, "'TO'" ];

var symbolicNames = [ null, "END", "IF", "THEN", "ELSE", "ENDIF", "ELSEIF", 
                      "DO", "ENDDO", "COMMA", "LPAR", "RPAR", "COLON", "ASSIGN", 
                      "MINUS", "PLUS", "DIV", "STAR", "MOD", "NOT", "AND", 
                      "OR", "LT", "LE", "GT", "GE", "NE", "EQ", "TO", "VARNAME", 
                      "INT", "EOL", "WS" ];

var ruleNames =  [ "compileUnit", "stmt", "do_stmt", "if_stmt", "block_if_stmt", 
                   "first_if_block", "else_if_stmt", "else_stmt", "assign_stmt", 
                   "empty_stmt", "expr", "or_expr", "and_expr", "eq_expr", 
                   "add_expr", "mult_expr", "final_expr", "array", "varname", 
                   "value" ];

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
cpdParser.MOD = 18;
cpdParser.NOT = 19;
cpdParser.AND = 20;
cpdParser.OR = 21;
cpdParser.LT = 22;
cpdParser.LE = 23;
cpdParser.GT = 24;
cpdParser.GE = 25;
cpdParser.NE = 26;
cpdParser.EQ = 27;
cpdParser.TO = 28;
cpdParser.VARNAME = 29;
cpdParser.INT = 30;
cpdParser.EOL = 31;
cpdParser.WS = 32;

cpdParser.RULE_compileUnit = 0;
cpdParser.RULE_stmt = 1;
cpdParser.RULE_do_stmt = 2;
cpdParser.RULE_if_stmt = 3;
cpdParser.RULE_block_if_stmt = 4;
cpdParser.RULE_first_if_block = 5;
cpdParser.RULE_else_if_stmt = 6;
cpdParser.RULE_else_stmt = 7;
cpdParser.RULE_assign_stmt = 8;
cpdParser.RULE_empty_stmt = 9;
cpdParser.RULE_expr = 10;
cpdParser.RULE_or_expr = 11;
cpdParser.RULE_and_expr = 12;
cpdParser.RULE_eq_expr = 13;
cpdParser.RULE_add_expr = 14;
cpdParser.RULE_mult_expr = 15;
cpdParser.RULE_final_expr = 16;
cpdParser.RULE_array = 17;
cpdParser.RULE_varname = 18;
cpdParser.RULE_value = 19;


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
        this.state = 41; 
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        do {
            this.state = 40;
            this.stmt();
            this.state = 43; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << cpdParser.IF) | (1 << cpdParser.DO) | (1 << cpdParser.VARNAME) | (1 << cpdParser.EOL))) !== 0));
        this.state = 45;
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

StmtContext.prototype.empty_stmt = function() {
    return this.getTypedRuleContext(Empty_stmtContext,0);
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
        this.state = 59;
        this._errHandler.sync(this);
        switch(this._input.LA(1)) {
        case cpdParser.DO:
            this.enterOuterAlt(localctx, 1);
            this.state = 47;
            this.do_stmt();
            this.state = 48;
            this.match(cpdParser.EOL);
            break;
        case cpdParser.IF:
            this.enterOuterAlt(localctx, 2);
            this.state = 50;
            this.if_stmt();
            this.state = 51;
            this.match(cpdParser.EOL);
            break;
        case cpdParser.VARNAME:
            this.enterOuterAlt(localctx, 3);
            this.state = 53;
            this.assign_stmt();
            this.state = 54;
            this.match(cpdParser.EOL);
            break;
        case cpdParser.EOL:
            this.enterOuterAlt(localctx, 4);
            this.state = 56;
            this.empty_stmt();
            this.state = 57;
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


Do_stmtContext.prototype.varname = function() {
    return this.getTypedRuleContext(VarnameContext,0);
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
        this.state = 61;
        this.match(cpdParser.DO);
        this.state = 62;
        this.varname();
        this.state = 71;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        if(_la===cpdParser.ASSIGN) {
            this.state = 63;
            this.match(cpdParser.ASSIGN);
            this.state = 64;
            this.eq_expr();
            this.state = 65;
            this.match(cpdParser.COMMA);
            this.state = 66;
            this.eq_expr();
            this.state = 69;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===cpdParser.COMMA) {
                this.state = 67;
                this.match(cpdParser.COMMA);
                this.state = 68;
                this.eq_expr();
            }

        }

        this.state = 73;
        this.match(cpdParser.EOL);
        this.state = 75; 
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        do {
            this.state = 74;
            this.stmt();
            this.state = 77; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << cpdParser.IF) | (1 << cpdParser.DO) | (1 << cpdParser.VARNAME) | (1 << cpdParser.EOL))) !== 0));
        this.state = 82;
        this._errHandler.sync(this);
        switch(this._input.LA(1)) {
        case cpdParser.ENDDO:
            this.state = 79;
            this.match(cpdParser.ENDDO);
            break;
        case cpdParser.END:
            this.state = 80;
            this.match(cpdParser.END);
            this.state = 81;
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
        this.state = 84;
        this.match(cpdParser.IF);
        this.state = 85;
        this.match(cpdParser.LPAR);
        this.state = 86;
        this.expr();
        this.state = 87;
        this.match(cpdParser.RPAR);
        this.state = 90;
        this._errHandler.sync(this);
        switch(this._input.LA(1)) {
        case cpdParser.THEN:
            this.state = 88;
            this.block_if_stmt();
            break;
        case cpdParser.VARNAME:
            this.state = 89;
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
        this.state = 92;
        this.first_if_block();
        this.state = 96;
        this._errHandler.sync(this);
        var _alt = this._interp.adaptivePredict(this._input,7,this._ctx)
        while(_alt!=2 && _alt!=antlr4.atn.ATN.INVALID_ALT_NUMBER) {
            if(_alt===1) {
                this.state = 93;
                this.else_if_stmt(); 
            }
            this.state = 98;
            this._errHandler.sync(this);
            _alt = this._interp.adaptivePredict(this._input,7,this._ctx);
        }

        this.state = 100;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        if(_la===cpdParser.ELSE) {
            this.state = 99;
            this.else_stmt();
        }

        this.state = 105;
        this._errHandler.sync(this);
        switch(this._input.LA(1)) {
        case cpdParser.ENDIF:
            this.state = 102;
            this.match(cpdParser.ENDIF);
            break;
        case cpdParser.END:
            this.state = 103;
            this.match(cpdParser.END);
            this.state = 104;
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
        this.state = 107;
        this.match(cpdParser.THEN);
        this.state = 108;
        this.match(cpdParser.EOL);
        this.state = 110; 
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        do {
            this.state = 109;
            this.stmt();
            this.state = 112; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << cpdParser.IF) | (1 << cpdParser.DO) | (1 << cpdParser.VARNAME) | (1 << cpdParser.EOL))) !== 0));
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
        this.state = 117;
        this._errHandler.sync(this);
        switch(this._input.LA(1)) {
        case cpdParser.ELSEIF:
            this.state = 114;
            this.match(cpdParser.ELSEIF);
            break;
        case cpdParser.ELSE:
            this.state = 115;
            this.match(cpdParser.ELSE);
            this.state = 116;
            this.match(cpdParser.IF);
            break;
        default:
            throw new antlr4.error.NoViableAltException(this);
        }
        this.state = 119;
        this.match(cpdParser.LPAR);
        this.state = 120;
        this.expr();
        this.state = 121;
        this.match(cpdParser.RPAR);
        this.state = 122;
        this.match(cpdParser.THEN);
        this.state = 123;
        this.match(cpdParser.EOL);
        this.state = 125; 
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        do {
            this.state = 124;
            this.stmt();
            this.state = 127; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << cpdParser.IF) | (1 << cpdParser.DO) | (1 << cpdParser.VARNAME) | (1 << cpdParser.EOL))) !== 0));
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
        this.state = 129;
        this.match(cpdParser.ELSE);
        this.state = 130;
        this.match(cpdParser.EOL);
        this.state = 132; 
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        do {
            this.state = 131;
            this.stmt();
            this.state = 134; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << cpdParser.IF) | (1 << cpdParser.DO) | (1 << cpdParser.VARNAME) | (1 << cpdParser.EOL))) !== 0));
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

Assign_stmtContext.prototype.varname = function() {
    return this.getTypedRuleContext(VarnameContext,0);
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
        this.state = 144;
        this._errHandler.sync(this);
        var la_ = this._interp.adaptivePredict(this._input,14,this._ctx);
        switch(la_) {
        case 1:
            this.enterOuterAlt(localctx, 1);
            this.state = 136;
            this.array();
            this.state = 137;
            this.match(cpdParser.ASSIGN);
            this.state = 138;
            this.expr();
            break;

        case 2:
            this.enterOuterAlt(localctx, 2);
            this.state = 140;
            this.varname();
            this.state = 141;
            this.match(cpdParser.ASSIGN);
            this.state = 142;
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


function Empty_stmtContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_empty_stmt;
    return this;
}

Empty_stmtContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
Empty_stmtContext.prototype.constructor = Empty_stmtContext;


Empty_stmtContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterEmpty_stmt(this);
	}
};

Empty_stmtContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitEmpty_stmt(this);
	}
};

Empty_stmtContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitEmpty_stmt(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.Empty_stmtContext = Empty_stmtContext;

cpdParser.prototype.empty_stmt = function() {

    var localctx = new Empty_stmtContext(this, this._ctx, this.state);
    this.enterRule(localctx, 18, cpdParser.RULE_empty_stmt);
    try {
        this.enterOuterAlt(localctx, 1);

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
    this.enterRule(localctx, 20, cpdParser.RULE_expr);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 148;
        this.or_expr();
        this.state = 153;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===cpdParser.OR) {
            this.state = 149;
            this.match(cpdParser.OR);
            this.state = 150;
            this.or_expr();
            this.state = 155;
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
    this.enterRule(localctx, 22, cpdParser.RULE_or_expr);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 156;
        this.and_expr();
        this.state = 161;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===cpdParser.AND) {
            this.state = 157;
            this.match(cpdParser.AND);
            this.state = 158;
            this.and_expr();
            this.state = 163;
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
    this.enterRule(localctx, 24, cpdParser.RULE_and_expr);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 164;
        this.eq_expr();
        this.state = 167;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        if((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << cpdParser.LT) | (1 << cpdParser.LE) | (1 << cpdParser.GT) | (1 << cpdParser.GE) | (1 << cpdParser.NE) | (1 << cpdParser.EQ))) !== 0)) {
            this.state = 165;
            _la = this._input.LA(1);
            if(!((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << cpdParser.LT) | (1 << cpdParser.LE) | (1 << cpdParser.GT) | (1 << cpdParser.GE) | (1 << cpdParser.NE) | (1 << cpdParser.EQ))) !== 0))) {
            this._errHandler.recoverInline(this);
            }
            else {
            	this._errHandler.reportMatch(this);
                this.consume();
            }
            this.state = 166;
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
    this.enterRule(localctx, 26, cpdParser.RULE_eq_expr);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 169;
        this.add_expr();
        this.state = 174;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===cpdParser.MINUS || _la===cpdParser.PLUS) {
            this.state = 170;
            _la = this._input.LA(1);
            if(!(_la===cpdParser.MINUS || _la===cpdParser.PLUS)) {
            this._errHandler.recoverInline(this);
            }
            else {
            	this._errHandler.reportMatch(this);
                this.consume();
            }
            this.state = 171;
            this.add_expr();
            this.state = 176;
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


Add_exprContext.prototype.MOD = function(i) {
	if(i===undefined) {
		i = null;
	}
    if(i===null) {
        return this.getTokens(cpdParser.MOD);
    } else {
        return this.getToken(cpdParser.MOD, i);
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
    this.enterRule(localctx, 28, cpdParser.RULE_add_expr);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 177;
        this.mult_expr();
        this.state = 182;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << cpdParser.DIV) | (1 << cpdParser.STAR) | (1 << cpdParser.MOD))) !== 0)) {
            this.state = 178;
            _la = this._input.LA(1);
            if(!((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << cpdParser.DIV) | (1 << cpdParser.STAR) | (1 << cpdParser.MOD))) !== 0))) {
            this._errHandler.recoverInline(this);
            }
            else {
            	this._errHandler.reportMatch(this);
                this.consume();
            }
            this.state = 179;
            this.mult_expr();
            this.state = 184;
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
    this.enterRule(localctx, 30, cpdParser.RULE_mult_expr);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 188;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===cpdParser.MINUS || _la===cpdParser.PLUS) {
            this.state = 185;
            _la = this._input.LA(1);
            if(!(_la===cpdParser.MINUS || _la===cpdParser.PLUS)) {
            this._errHandler.recoverInline(this);
            }
            else {
            	this._errHandler.reportMatch(this);
                this.consume();
            }
            this.state = 190;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 191;
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

Final_exprContext.prototype.varname = function() {
    return this.getTypedRuleContext(VarnameContext,0);
};

Final_exprContext.prototype.value = function() {
    return this.getTypedRuleContext(ValueContext,0);
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
    this.enterRule(localctx, 32, cpdParser.RULE_final_expr);
    try {
        this.state = 200;
        this._errHandler.sync(this);
        var la_ = this._interp.adaptivePredict(this._input,21,this._ctx);
        switch(la_) {
        case 1:
            this.enterOuterAlt(localctx, 1);
            this.state = 193;
            this.array();
            break;

        case 2:
            this.enterOuterAlt(localctx, 2);
            this.state = 194;
            this.varname();
            break;

        case 3:
            this.enterOuterAlt(localctx, 3);
            this.state = 195;
            this.value();
            break;

        case 4:
            this.enterOuterAlt(localctx, 4);
            this.state = 196;
            this.match(cpdParser.LPAR);
            this.state = 197;
            this.eq_expr();
            this.state = 198;
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
    this.enterRule(localctx, 34, cpdParser.RULE_array);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 202;
        this.match(cpdParser.VARNAME);
        this.state = 203;
        this.match(cpdParser.LPAR);
        this.state = 204;
        this.expr();
        this.state = 209;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===cpdParser.COMMA) {
            this.state = 205;
            this.match(cpdParser.COMMA);
            this.state = 206;
            this.expr();
            this.state = 211;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 212;
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


function VarnameContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_varname;
    return this;
}

VarnameContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
VarnameContext.prototype.constructor = VarnameContext;

VarnameContext.prototype.VARNAME = function() {
    return this.getToken(cpdParser.VARNAME, 0);
};

VarnameContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterVarname(this);
	}
};

VarnameContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitVarname(this);
	}
};

VarnameContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitVarname(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.VarnameContext = VarnameContext;

cpdParser.prototype.varname = function() {

    var localctx = new VarnameContext(this, this._ctx, this.state);
    this.enterRule(localctx, 36, cpdParser.RULE_varname);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 214;
        this.match(cpdParser.VARNAME);
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


function ValueContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = cpdParser.RULE_value;
    return this;
}

ValueContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ValueContext.prototype.constructor = ValueContext;

ValueContext.prototype.INT = function() {
    return this.getToken(cpdParser.INT, 0);
};

ValueContext.prototype.enterRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.enterValue(this);
	}
};

ValueContext.prototype.exitRule = function(listener) {
    if(listener instanceof cpdListener ) {
        listener.exitValue(this);
	}
};

ValueContext.prototype.accept = function(visitor) {
    if ( visitor instanceof cpdVisitor ) {
        return visitor.visitValue(this);
    } else {
        return visitor.visitChildren(this);
    }
};




cpdParser.ValueContext = ValueContext;

cpdParser.prototype.value = function() {

    var localctx = new ValueContext(this, this._ctx, this.state);
    this.enterRule(localctx, 38, cpdParser.RULE_value);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 216;
        this.match(cpdParser.INT);
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
