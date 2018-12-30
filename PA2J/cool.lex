/*
 *  The scanner definition for COOL.
 */

import java_cup.runtime.Symbol;


%%

%state COMMENT,UNCOMMENT, NESTEDCOMMENTSTART,STRINGSTART
RIGHTNESTEDBRACKET = [*][)]
LEFTNESTEDBRACKET = [(][*]
%{

/*  Stuff enclosed in %{ %} is copied verbatim to the lexer class
 *  definition, all the extra variables/functions you want to use in the
 *  lexer actions should go here.  Don't remove or modify anything that
 *  was there initially.  */

    // Max size of string constants
    static int MAX_STR_CONST = 1025;

    // For assembling string constants
    StringBuffer string_buf = new StringBuffer();

    private int curr_lineno = 1;
    int get_curr_lineno() {
	return curr_lineno;
    }

    private AbstractSymbol filename;
    private int leftBracket = 0;

    void set_filename(String fname) {
	filename = AbstractTable.stringtable.addString(fname);
    }

    AbstractSymbol curr_filename() {
	return filename;
    }
%}

%eof{

%eof}

%init{

/*  Stuff enclosed in %init{ %init} is copied verbatim to the lexer
 *  class constructor, all the extra initialization you want to do should
 *  go here.  Don't remove or modify anything that was there initially. */
	leftBracket = 0;
    // empty for now
%init}

%eofval{

/*  Stuff enclosed in %eofval{ %eofval} specifies java code that is
 *  executed when end-of-file is reached.  If you use multiple lexical
 *  states and want to do something special if an EOF is encountered in
 *  one of those states, place your code in the switch statement.
 *  Ultimately, you should return the EOF symbol, or your lexer won't
 *  work.  */

    switch(yy_lexical_state) {
    case YYINITIAL:
	/* nothing special to do in the initial state */
	break;
	case COMMENT:
		break;

	/* If necessary, add code for other states here, e.g:
	   case COMMENT:
	   ...
	   break;
	*/
    }
    return new Symbol(TokenConstants.EOF);
%eofval}

%class CoolLexer
%cup



%%

<YYINITIAL>"=>"			{ /* Sample lexical rule for "=>" arrow.
                                     Further lexical rules should be defined
                                     here, after the last %% separator */
                                  return new Symbol(TokenConstants.DARROW); }


<YYINITIAL>[cC][lL][aA][sS][sS]		{ return new Symbol(TokenConstants.CLASS); }
<YYINITIAL>[eE][lL][sS][eE]		{ return new Symbol(TokenConstants.ELSE);  } 
<YYINITIAL>[fF][iI]			{ return new Symbol(TokenConstants.FI);    }
<YYINITIAL>[iI][fF]			{ return new Symbol(TokenConstants.IF);    }
<YYINITIAL>[iI][nN]			{ return new Symbol(TokenConstants.IN);    }
<YYINITIAL>[iI][nN][hH][eE][rR][iI][tT][sS] { return new Symbol(TokenConstants.INHERITS);}
<YYINITIAL>[iI][sS][vV][oO][iI][dD]	{ return new Symbol(TokenConstants.ISVOID); }
<YYINITIAL>[lL][eE][tT]			{ return new Symbol(TokenConstants.LET);    }
<YYINITIAL>[lL][oO][oO][pP]		{ return new Symbol(TokenConstants.LOOP); }
<YYINITIAL>[pP][oO][oO][lL]		{ return new Symbol(TokenConstants.POOL);}
<YYINITIAL>[tT][hH][eE][nN]		{ return new Symbol(TokenConstants.THEN);}
<YYINITIAL>[wW][hH][iI][lL][eE]		{ return new Symbol(TokenConstants.WHILE);}
<YYINITIAL>[cC][aA][sS][eE]		{ return new Symbol(TokenConstants.CASE);}
<YYINITIAL>[eE][sS][aA][cC]		{ return new Symbol(TokenConstants.ESAC);}
<YYINITIAL>[nN][eE][wW]			{ return new Symbol(TokenConstants.NEW);}
<YYINITIAL>[oO][fF]			{ return new Symbol(TokenConstants.OF);}
<YYINITIAL>[nN][oO][tT]			{ return new Symbol(TokenConstants.NOT);}
<YYINITIAL>[\n]				{ curr_lineno = curr_lineno + 1;}
<YYINITIAL>[\t\f\r\013 ]+			{ }
<YYINITIAL>[t][rR][uU][eE]			{ return new Symbol(TokenConstants.BOOL_CONST,true);}
<YYINITIAL>[f][aA][lL][sS][eE]		{ return new Symbol(TokenConstants.BOOL_CONST,false);}
<YYINITIAL>[0-9]+				{ return new Symbol(TokenConstants.INT_CONST, AbstractTable.inttable.addString(yytext()));}
<YYINITIAL>[;]				{ return new Symbol(TokenConstants.SEMI);}
<YYINITIAL>[*]				{ return new Symbol(TokenConstants.MULT);}
<YYINITIAL>[+]				{ return new Symbol(TokenConstants.PLUS);}
<YYINITIAL>[-]				{ return new Symbol(TokenConstants.MINUS);}
<YYINITIAL>[~]				{ return new Symbol(TokenConstants.NEG);}
<YYINITIAL>[(]				{ return new Symbol(TokenConstants.LPAREN);}
<YYINITIAL>[)]				{ return new Symbol(TokenConstants.RPAREN);}
<YYINITIAL>[<]				{ return new Symbol(TokenConstants.LT);}

<YYINITIAL>[/]				{ return new Symbol(TokenConstants.DIV);}
<YYINITIAL>[,]				{ return new Symbol(TokenConstants.COMMA);}
<YYINITIAL>[.]				{ return new Symbol(TokenConstants.DOT);}
<YYINITIAL>[<][=]				{ return new Symbol(TokenConstants.LE);}
<YYINITIAL>[=]				{ return new Symbol(TokenConstants.EQ);}
<YYINITIAL>[:]				{ return new Symbol(TokenConstants.COLON);}
<YYINITIAL>[\173]				{ return new Symbol(TokenConstants.LBRACE);}
<YYINITIAL>[\175]				{ return new Symbol(TokenConstants.RBRACE);}

[\EOF]					{
						if ( yy_lexical_state == NESTEDCOMMENTSTART)
							return new Symbol(TokenConstants.ERROR,"EOF in comment");
						if ( yy_lexical_state == STRINGSTART)
							return new Symbol(TokenConstants.ERROR,"EOF in string constant");
						return new Symbol(TokenConstants.EOF);
					 }

<YYINITIAL>[*][)]			{ 
					  return new Symbol(TokenConstants.ERROR,"Unmatched *)");
					  //leftBracket = leftBracket - 1 ;
					  //if ( leftBracket < 0 ){
						//return new Symbol(TokenConstants.NOT);
					  //}
					}
<YYINITIAL>[-][-]			{  
					  //System.out.println("Begining of the -- comment");
						yy_lexical_state = COMMENT;}


<YYINITIAL>[A-Z][A-Za-z_0-9]*		{  
					      //if ( yytext().length() > MAX_STR_CONST)
						//return new Symbol( TokenConstants.ERROR,"String constant too long");
					     return new Symbol( TokenConstants.TYPEID, AbstractTable.stringtable.addString(yytext()) ); }

<YYINITIAL>[a-z][A-Za-z_0-9]*		{  
					    //if( yytext().length() > MAX_STR_CONST )
						//return new Symbol(TokenConstants.ERROR,"String constant too long");
					   return new Symbol( TokenConstants.OBJECTID, AbstractTable.stringtable.addString(yytext()) ); 
					}



<YYINITIAL>[\"\"]			{
						//System.out.println("Matching the \"");
						string_buf.append(yytext());
						yy_lexical_state = STRINGSTART;
					}

<STRINGSTART>.|\n                         {
						int len = string_buf.length();
						char ch = yytext().charAt(0);
						if ( ch == '\n'){
						  curr_lineno = curr_lineno + 1;
						  if ( string_buf.charAt(len-1) != '\\' ) {
							string_buf = new StringBuffer();
							//System.out.println("Encountered the new line "+ yytext());
							yy_lexical_state = YYINITIAL;							
							return new Symbol(TokenConstants.ERROR,"Unterminated string constant");
						  }
						  else{
						      string_buf.append('\n');
						  }
						}
						else if ( ch == 'c'){
							if ( string_buf.charAt(len-1)=='\\'){
								string_buf.deleteCharAt(len-1);
								string_buf.append(ch);
							}else{	
								string_buf.append(ch);
							}
						}
						else if ( ch == 'b'){
							if( string_buf.charAt(len-1) == '\\'){
								System.out.println("Deleting the char at index len -1");

								string_buf.deleteCharAt(len-1);
								System.out.println(new String(string_buf));
								char c = (char)8;
								//System.out.println("The char that is going to be printed is"+ c);
								string_buf.append(c);
							}else{
								string_buf.append('b');
							}
						}
						else if ( ch =='t' ){
							if( string_buf.charAt(len-1) == '\\'){
								string_buf.deleteCharAt(len-1);string_buf.append('\t');
							}else{
								string_buf.append('t');
							}

						}
						else if ( ch =='f' ){
							if( string_buf.charAt(len-1) == '\\'){
								string_buf.deleteCharAt(len-1);string_buf.append('\f');
							}else{
								string_buf.append('f');
							}

						}
						else if( ch =='\"'){
							//System.out.println("Matching the other \"");
							//System.out.println( string_buf.substring(1));
							yy_lexical_state = YYINITIAL;
							if ( string_buf.length() > MAX_STR_CONST + 1 ){
								string_buf = new StringBuffer();
								return new Symbol(TokenConstants.ERROR,"String constant too long");
							}
			String st = new String(string_buf.substring(1));
			string_buf = new StringBuffer();
		return new Symbol(TokenConstants.STR_CONST,AbstractTable.stringtable.addString(st));
						}
						else if (ch == '\0'){
							string_buf = new StringBuffer();
							return new Symbol(TokenConstants.ERROR,"String contains null character");
						}
						else if (  ch == '0'){
							if( string_buf.charAt(len-1) == '\\'){
								string_buf.deleteCharAt(len-1);string_buf.append('0');
							}else{
								string_buf.append('0');
							}
						}
						else {
							string_buf.append(yytext());
						}
					
					}

<COMMENT>[^\n\EOF]+			{ }
<COMMENT>[\EOF\n]			{// System.out.println("Comments Ended "); 
						yy_lexical_state = YYINITIAL; 
						if( yytext().charAt(0) == '\n')
							curr_lineno = curr_lineno + 1;
					}

<YYINITIAL,NESTEDCOMMENTSTART>[(][*]	{  
				         	//System.out.println("Begining of the nested comment");
					   yy_lexical_state = NESTEDCOMMENTSTART;
					   leftBracket = leftBracket + 1; 
					}

<NESTEDCOMMENTSTART>[*][)]		{ 
					   //System.out.println("Encountered the right bracket");
					   leftBracket = leftBracket - 1;
					   if(leftBracket <0 ){ 
						return new Symbol(TokenConstants.NOT);  
					   }
					   if(leftBracket == 0 ) yy_lexical_state = YYINITIAL;
					}

<NESTEDCOMMENTSTART>[^\EOF(*]+ 		{  //System.out.println("Printing the matched text"+yytext());
					}



.                               { /* This rule should be the very last
                                     in your lexical specification and
                                     will match match everything not
                                     matched by other lexical rules. */
	                                  System.err.println("LEXER BUG - UNMATCHED: " + yytext());
				     return new Symbol(TokenConstants.ERROR, yytext());					
 }




