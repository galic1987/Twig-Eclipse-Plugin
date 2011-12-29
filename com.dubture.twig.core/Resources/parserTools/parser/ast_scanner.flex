/*******************************************************************************
 * Copyright (c) 2006 Zend Corporation and IBM Corporation.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Zend and IBM - Initial implementation
 *******************************************************************************/

package com.dubture.twig.core.parser.ast.scanner;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.php.internal.core.ast.nodes.IDocumentorLexer;
import org.eclipse.php.internal.core.ast.nodes.Comment;
import java_cup.sym;
import org.eclipse.php.internal.core.ast.nodes.AST;
import java_cup.runtime.Symbol;
import org.eclipse.php.internal.core.ast.scanner.StateStack;
import org.eclipse.php.internal.core.PHPVersion;

%%

%class TwigAstLexer
%public
%unicode
%line

/* %cup */
%implements org.eclipse.php.internal.core.ast.scanner.AstLexer
%function next_token
%type java_cup.runtime.Symbol
%eofval{
    return createSymbol(sym.EOF);
%eofval}
%eofclose

%caseless

%standalone
%state ST_IN_SCRIPTING
%state ST_DOUBLE_QUOTES
%state ST_BACKQUOTE
%state ST_LOOKING_FOR_PROPERTY
%state ST_LOOKING_FOR_VARNAME
%state ST_VAR_OFFSET
%state ST_COMMENT
%state ST_DOCBLOCK
%state ST_ONE_LINE_COMMENT
%{
	private final LinkedList commentList = new LinkedList();	
    private StateStack stack = new StateStack();
    private char yy_old_buffer[] = new char[ZZ_BUFFERSIZE];
    private int yy_old_pushbackPos;
    protected int commentStartPosition;

	private AST ast;

    public void setAST(AST ast) {
    	this.ast = ast;
    }
    
	public PHPVersion getPHPVersion() {
		return PHPVersion.PHP5_3;
	}
    
	public void setInScriptingState() {
		yybegin(ST_IN_SCRIPTING);
	}

	public void resetCommentList() {
		commentList.clear();
	}
	
	public LinkedList getCommentList() {
		return commentList;
	}	
	
	protected void addComment(int type) {
		int leftPosition = getTokenStartPosition();
		Comment comment = new Comment(commentStartPosition, leftPosition + getTokenLength(), ast, type);
		commentList.add(comment);
	}	
	
	public void setUseAspTagsAsPhp(boolean useAspTagsAsPhp) {

	}
	
	public void setUseShortTags(boolean useShortTags) {

	}
	
	
    private void pushState(int state) {
        stack.pushStack(zzLexicalState);
        yybegin(state);
    }

    private void popState() {
        yybegin(stack.popStack());
    }

    public int getCurrentLine() {
        return yyline;
    }

    protected int getTokenStartPosition() {
        return zzStartRead - zzPushbackPos;
    }

    protected int getTokenLength() {
        return zzMarkedPos - zzStartRead;
    }

    public int getLength() {
        return zzEndRead - zzPushbackPos;
    }
    
    private void handleCommentStart() {
		commentStartPosition = getTokenStartPosition();
	}
	
	private void handleLineCommentEnd() {
         addComment(Comment.TYPE_SINGLE_LINE);
    }
    
    private void handleMultilineCommentEnd() {
    	addComment(Comment.TYPE_MULTILINE);
    }

    private void handlePHPDocEnd() {
		addComment(Comment.TYPE_PHPDOC);
    }
    
    protected void handleVarComment() {
    	commentStartPosition = zzStartRead;
    	addComment(Comment.TYPE_MULTILINE);
    }
        
    private Symbol createFullSymbol(int symbolNumber) {
        Symbol symbol = createSymbol(symbolNumber);
        symbol.value = yytext();
        return symbol;
    }

    protected Symbol createSymbol(int symbolNumber) {
        int leftPosition = getTokenStartPosition();
        return new Symbol(symbolNumber, leftPosition, leftPosition + getTokenLength());
    }

    public int[] getParamenters(){
    	return new int[]{zzMarkedPos, zzPushbackPos, zzCurrentPos, zzStartRead, zzEndRead, yyline};
    }
    
	protected boolean parsePHPDoc(){	
		final IDocumentorLexer documentorLexer = getDocumentorLexer(zzReader);
		if(documentorLexer == null){
			return false;
		}
		yypushback(zzMarkedPos - zzStartRead);
		int[] parameters = getParamenters();
		documentorLexer.reset(zzReader, zzBuffer, parameters);
		Object phpDocBlock = documentorLexer.parse();
		commentList.add(phpDocBlock);
		reset(zzReader, documentorLexer.getBuffer(), documentorLexer.getParamenters());
		return true;
	}
	
	
	protected IDocumentorLexer getDocumentorLexer(java.io.Reader  reader) {
		return null;
	}
	
	public void reset(java.io.Reader  reader, char[] buffer, int[] parameters){
		this.zzReader = reader;
		this.zzBuffer = buffer;
		this.zzMarkedPos = parameters[0];
		this.zzPushbackPos = parameters[1];
		this.zzCurrentPos = parameters[2];
		this.zzStartRead = parameters[3];
		this.zzEndRead = parameters[4];
		this.yyline = parameters[5];  
		this.yychar = this.zzStartRead - this.zzPushbackPos;
	}

%}

LNUM=[0-9]+
DNUM=([0-9]*"."[0-9]+)|([0-9]+"."[0-9]*)
EXPONENT_DNUM=(({LNUM}|{DNUM})[eE][+-]?{LNUM})
HNUM="0x"[0-9a-fA-F]+
LABEL=[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*
WHITESPACE=[ \n\r\t]+
TABS_AND_SPACES=[ \t]*
ANY_CHAR=[^]
NEWLINE=("\r"|"\n"|"\r\n")
DOUBLE_QUOTES_LITERAL_DOLLAR=("$"+([^a-zA-Z_\x7f-\xff$\"\\{]|("\\"{ANY_CHAR})))
BACKQUOTE_LITERAL_DOLLAR=("$"+([^a-zA-Z_\x7f-\xff$`\\{]|("\\"{ANY_CHAR})))
DOUBLE_QUOTES_CHARS=("{"*([^$\"\\{]|("\\"{ANY_CHAR}))|{DOUBLE_QUOTES_LITERAL_DOLLAR})
BACKQUOTE_CHARS=("{"*([^$`\\{]|("\\"{ANY_CHAR}))|{BACKQUOTE_LITERAL_DOLLAR})

%%

<ST_IN_SCRIPTING>"as" {
	return createSymbol(ParserConstants.T_AS);
}


<ST_IN_SCRIPTING>"->" {
    pushState(ST_LOOKING_FOR_PROPERTY);
    return createSymbol(ParserConstants.T_OBJECT_OPERATOR);
}

<ST_IN_SCRIPTING,ST_LOOKING_FOR_PROPERTY>{WHITESPACE}+ {
}

<ST_LOOKING_FOR_PROPERTY>"->" {
	return createSymbol(ParserConstants.T_OBJECT_OPERATOR);
}

<ST_LOOKING_FOR_PROPERTY>{LABEL} {
    popState();
    return createFullSymbol(ParserConstants.T_STRING);
}

<ST_LOOKING_FOR_PROPERTY>{ANY_CHAR} {
    yypushback(yylength());
    popState();
}

<ST_IN_SCRIPTING>"OR" {
	return createSymbol(ParserConstants.T_LOGICAL_OR);
}

<ST_IN_SCRIPTING>"AND" {
	return createSymbol(ParserConstants.T_LOGICAL_AND);
}

// TOKENS
<ST_IN_SCRIPTING> {
    ";"                     {return createSymbol(ParserConstants.T_SEMICOLON);}
    ":"                     {return createSymbol(ParserConstants.T_NEKUDOTAIM);}
    ","                     {return createSymbol(ParserConstants.T_COMMA);}
    "."                     {return createSymbol(ParserConstants.T_NEKUDA);}
    "["                     {return createSymbol(ParserConstants.T_OPEN_RECT);}
    "]"                     {return createSymbol(ParserConstants.T_CLOSE_RECT);}
    "("                     {return createSymbol(ParserConstants.T_OPEN_PARENTHESE);}
    ")"                     {return createSymbol(ParserConstants.T_CLOSE_PARENTHESE);}
    "|"                     {return createSymbol(ParserConstants.T_OR);}
    "^"                     {return createSymbol(ParserConstants.T_KOVA);}
    "&"                     {return createSymbol(ParserConstants.T_REFERENCE);}
    "+"                     {return createSymbol(ParserConstants.T_PLUS);}
    "-"                     {return createSymbol(ParserConstants.T_MINUS);}
    "/"                     {return createSymbol(ParserConstants.T_DIV);}
    "*"                     {return createSymbol(ParserConstants.T_TIMES);}
    "="                     {return createSymbol(ParserConstants.T_EQUAL);}
    "%"                     {return createSymbol(ParserConstants.T_PRECENT);}
    "!"                     {return createSymbol(ParserConstants.T_NOT);}
    "~"                     {return createSymbol(ParserConstants.T_TILDA);}
    "$"                     {return createSymbol(ParserConstants.T_DOLLAR);}
    "<"                     {return createSymbol(ParserConstants.T_RGREATER);}
    ">"                     {return createSymbol(ParserConstants.T_LGREATER);}
    "?"                     {return createSymbol(ParserConstants.T_QUESTION_MARK);}
    "@"                     {return createSymbol(ParserConstants.T_AT);}
}

<ST_IN_SCRIPTING>"{" {
    pushState(ST_IN_SCRIPTING);
    return createSymbol(ParserConstants.T_CURLY_OPEN);

}

<ST_DOUBLE_QUOTES,ST_BACKQUOTE>"${" {
    pushState(ST_LOOKING_FOR_VARNAME);
    return createSymbol(ParserConstants.T_DOLLAR_OPEN_CURLY_BRACES);
}

<ST_IN_SCRIPTING>"}" {
	/* This is a temporary fix which is dependant on flex and it's implementation */
    if (!stack.isEmpty()) {
        popState();
    }
    return createSymbol(ParserConstants.T_CURLY_CLOSE);
}

<ST_LOOKING_FOR_VARNAME>{LABEL} {
    popState();
    pushState(ST_IN_SCRIPTING);
    return createFullSymbol(ParserConstants.T_STRING_VARNAME);
}

<ST_LOOKING_FOR_VARNAME>{ANY_CHAR} {
    yypushback(yylength());
    popState();
    pushState(ST_IN_SCRIPTING);
}

<ST_IN_SCRIPTING>{LNUM} {
    return createFullSymbol(ParserConstants.T_LNUMBER);
}

<ST_IN_SCRIPTING>{HNUM} {
    return createFullSymbol(ParserConstants.T_DNUMBER);
}

<ST_VAR_OFFSET>[0]|([1-9][0-9]*) { /* Offset could be treated as a long */
	return createFullSymbol(ParserConstants.T_NUM_STRING);
}

<ST_VAR_OFFSET>{LNUM}|{HNUM} { /* treat numbers (almost) as strings inside encapsulated strings */
    return createFullSymbol(ParserConstants.T_NUM_STRING);
}

<ST_IN_SCRIPTING>{DNUM}|{EXPONENT_DNUM} {
    return createFullSymbol(ParserConstants.T_DNUMBER);
}

<YYINITIAL>(([^<]|"<"[^?%s<])+)|"<s"|"<" {
    return createSymbol(ParserConstants.T_INLINE_HTML);
}

<YYINITIAL>"<?"|"<script"{WHITESPACE}+"language"{WHITESPACE}*"="{WHITESPACE}*("php"|"\"php\""|"\'php\'"){WHITESPACE}*">" {
    if (yylength()>2) { /* yyleng>2 means it's not <? but <script> */
        yybegin(ST_IN_SCRIPTING);
        //return T_OPEN_TAG;
    } else {
        return createSymbol(ParserConstants.T_INLINE_HTML);
    }
}

<YYINITIAL>"<?php"([ \t]|{NEWLINE}) {
    yybegin(ST_IN_SCRIPTING);
	//return T_OPEN_TAG;
}

<ST_IN_SCRIPTING,ST_DOUBLE_QUOTES,ST_BACKQUOTE,ST_VAR_OFFSET>"$"{LABEL} {
    return createFullSymbol(ParserConstants.T_VARIABLE);
}

<ST_DOUBLE_QUOTES,ST_BACKQUOTE>"$"{LABEL}"->"[a-zA-Z_\x7f-\xff] {
	yypushback(3);
	pushState(ST_LOOKING_FOR_PROPERTY);
	return createFullSymbol(ParserConstants.T_VARIABLE);
}

<ST_DOUBLE_QUOTES,ST_BACKQUOTE>"$"{LABEL}"[" {
	yypushback(1);
	pushState(ST_VAR_OFFSET);
	return createFullSymbol(ParserConstants.T_VARIABLE);
}

<ST_VAR_OFFSET>"]" {
	popState();
	return createSymbol(ParserConstants.T_CLOSE_RECT);
}

//this is instead {TOKENS}|[{}"`]
<ST_VAR_OFFSET> {
    ";"                     {return createSymbol(ParserConstants.T_SEMICOLON);}
    ":"                     {return createSymbol(ParserConstants.T_NEKUDOTAIM);}
    ","                     {return createSymbol(ParserConstants.T_COMMA);}
    "."                     {return createSymbol(ParserConstants.T_NEKUDA);}
    "["                     {return createSymbol(ParserConstants.T_OPEN_RECT);}
//    "]"                     {return createSymbol(ParserConstants.T_CLOSE_RECT);} //we dont need this line because the rule before deals with it
    "("                     {return createSymbol(ParserConstants.T_OPEN_PARENTHESE);}
    ")"                     {return createSymbol(ParserConstants.T_CLOSE_PARENTHESE);}
    "|"                     {return createSymbol(ParserConstants.T_OR);}
    "^"                     {return createSymbol(ParserConstants.T_KOVA);}
    "&"                     {return createSymbol(ParserConstants.T_REFERENCE);}
    "+"                     {return createSymbol(ParserConstants.T_PLUS);}
    "-"                     {return createSymbol(ParserConstants.T_MINUS);}
    "/"                     {return createSymbol(ParserConstants.T_DIV);}
    "*"                     {return createSymbol(ParserConstants.T_TIMES);}
    "="                     {return createSymbol(ParserConstants.T_EQUAL);}
    "%"                     {return createSymbol(ParserConstants.T_PRECENT);}
    "!"                     {return createSymbol(ParserConstants.T_NOT);}
    "~"                     {return createSymbol(ParserConstants.T_TILDA);}
    "$"                     {return createSymbol(ParserConstants.T_DOLLAR);}
    "<"                     {return createSymbol(ParserConstants.T_RGREATER);}
    ">"                     {return createSymbol(ParserConstants.T_LGREATER);}
    "?"                     {return createSymbol(ParserConstants.T_QUESTION_MARK);}
    "@"                     {return createSymbol(ParserConstants.T_AT);}
    "{"                     {return createSymbol(ParserConstants.T_CURLY_OPEN);}
    "}"                     {return createSymbol(ParserConstants.T_CURLY_CLOSE);}
    "\""                     {return createSymbol(ParserConstants.T_QUATE);}
    "`"                     {return createSymbol(ParserConstants.T_BACKQUATE);}
}

<ST_VAR_OFFSET>[ \n\r\t\\'#] {
	yypushback(1);
	popState();
	return createSymbol(ParserConstants.T_ENCAPSED_AND_WHITESPACE);
}

<ST_IN_SCRIPTING,ST_VAR_OFFSET>{LABEL} {
    return createFullSymbol(ParserConstants.T_STRING);
}

<ST_IN_SCRIPTING>{WHITESPACE} {
}

<ST_IN_SCRIPTING>"#"|"//" {
	handleCommentStart();
	yybegin(ST_ONE_LINE_COMMENT);
//	yymore();
}

<ST_ONE_LINE_COMMENT>"?"|"%"|">" {
	//	yymore();
}

<ST_ONE_LINE_COMMENT>[^\n\r?%>]*(.|{NEWLINE}) {
	String yytext = yytext();
	switch (yytext.charAt(yytext.length() - 1)) {
		case '?':
		case '%':
		case '>':
			yypushback(1);
			break;
		default:
			handleLineCommentEnd();
			yybegin(ST_IN_SCRIPTING);
	}
//	yymore();
}

<ST_IN_SCRIPTING>"/*"{WHITESPACE}*"@var"{WHITESPACE}("$"?){LABEL}{WHITESPACE}(("\\"|{LABEL}|"|")+)*{WHITESPACE}?"*/" {
    handleVarComment();
    //return createFullSymbol(ParserConstants.T_VAR_COMMENT);
}

<ST_IN_SCRIPTING>"/**" {
if (!parsePHPDoc()) {
handleCommentStart();
yybegin(ST_DOCBLOCK);
}
}

<ST_DOCBLOCK>"*/" {
     handlePHPDocEnd();
     yybegin(ST_IN_SCRIPTING);
}

<ST_DOCBLOCK>{NEWLINE} {
}

<ST_DOCBLOCK>{ANY_CHAR} {
}

<ST_IN_SCRIPTING>"/**/" {
	handleCommentStart();
}

<ST_IN_SCRIPTING>"/*" {
	handleCommentStart();
    yybegin(ST_COMMENT);
}

<ST_COMMENT>[^*]+ {
}

<ST_COMMENT>"*/" {
	handleMultilineCommentEnd();
    yybegin(ST_IN_SCRIPTING);
}

<ST_COMMENT>"*" {
//	yymore();
}

<ST_IN_SCRIPTING>("?>"|"</script"{WHITESPACE}*">"){NEWLINE}? {
    yybegin(YYINITIAL);
    return createSymbol(ParserConstants.T_SEMICOLON);  /* implicit ';' at php-end tag */
}


<ST_IN_SCRIPTING>(b?[\"]{DOUBLE_QUOTES_CHARS}*("{"*|"$"*)[\"]) {
    return createFullSymbol(ParserConstants.T_CONSTANT_ENCAPSED_STRING);
}

<ST_IN_SCRIPTING>(b?[']([^'\\]|("\\"{ANY_CHAR}))*[']) {
    return createFullSymbol(ParserConstants.T_CONSTANT_ENCAPSED_STRING);
}

<ST_IN_SCRIPTING>b?[\"] {
    yybegin(ST_DOUBLE_QUOTES);
    return createSymbol(ParserConstants.T_QUATE);
}


<ST_IN_SCRIPTING>[`] {
    yybegin(ST_BACKQUOTE);
    return createSymbol(ParserConstants.T_BACKQUATE);
}


<ST_DOUBLE_QUOTES,ST_BACKQUOTE>"{$" {
    pushState(ST_IN_SCRIPTING);
    yypushback(yylength()-1);
    return createSymbol(ParserConstants.T_CURLY_OPEN_WITH_DOLAR);
}

<ST_DOUBLE_QUOTES>{DOUBLE_QUOTES_CHARS}+ {
	return createFullSymbol(ParserConstants.T_ENCAPSED_AND_WHITESPACE);
}

/*
The original parsing rule was {DOUBLE_QUOTES_CHARS}*("{"{2,}|"$"{2,}|(("{"+|"$"+)[\"]))
but jflex doesn't support a{n,} so we changed a{2,} to aa+
*/
<ST_DOUBLE_QUOTES>{DOUBLE_QUOTES_CHARS}*("{""{"+|"$""$"+|(("{"+|"$"+)[\"])) {
    yypushback(1);
    return createFullSymbol(ParserConstants.T_ENCAPSED_AND_WHITESPACE);
}

<ST_BACKQUOTE>{BACKQUOTE_CHARS}+ {
	return createFullSymbol(ParserConstants.T_ENCAPSED_AND_WHITESPACE);
}

/*
The original parsing rule was {BACKQUOTE_CHARS}*("{"{2,}|"$"{2,}|(("{"+|"$"+)[`]))
but jflex doesn't support a{n,} so we changed a{2,} to aa+
*/
<ST_BACKQUOTE>{BACKQUOTE_CHARS}*("{""{"+|"$""$"+|(("{"+|"$"+)[`])) {
	yypushback(1);
	return createFullSymbol(ParserConstants.T_ENCAPSED_AND_WHITESPACE);
}

<ST_DOUBLE_QUOTES>[\"] {
    yybegin(ST_IN_SCRIPTING);
    return createSymbol(ParserConstants.T_QUATE);
}

<ST_BACKQUOTE>[`] {
    yybegin(ST_IN_SCRIPTING);
    return createSymbol(ParserConstants.T_BACKQUATE);
}

<ST_IN_SCRIPTING,YYINITIAL,ST_DOUBLE_QUOTES,ST_BACKQUOTE,ST_VAR_OFFSET>{ANY_CHAR} {
	// do nothing
}