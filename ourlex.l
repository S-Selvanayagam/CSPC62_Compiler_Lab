%{    
   #include "y.tab.h"
   int line_no=1;
%}

%option yylineno
%option noinput nounput
alphabet        [a-zA-Z]
digit           [0-9]

%%

"announcing"        { strcpy(yylval.nd_obj.name, (yytext)); return PRINTFF; }
"batao"             { strcpy(yylval.nd_obj.name, (yytext)); return SCANFF; }

"maybe"             { strcpy(yylval.nd_obj.name, (yytext)); return IF; }
"otherwise"         { strcpy(yylval.nd_obj.name, (yytext)); return ELSE; }
"poka"              { strcpy(yylval.nd_obj.name, (yytext)); return WHILE; }
"kaerimasu"         { strcpy(yylval.nd_obj.name, (yytext)); return RETURN; }

"adhavadhu"         { strcpy(yylval.nd_obj.name, (yytext)); return DECLARE; }
"+"                 { strcpy(yylval.nd_obj.name, (yytext)); return ADD; }
"-"                 { strcpy(yylval.nd_obj.name, (yytext)); return SUB; }
"*"                 { strcpy(yylval.nd_obj.name, (yytext)); return MULT; }
"/"                 { strcpy(yylval.nd_obj.name, (yytext)); return DIV; }
"log"               { strcpy(yylval.nd_obj.name, (yytext)); return LOG; }
"exp"               { strcpy(yylval.nd_obj.name, (yytext)); return POW; }


">="                { strcpy(yylval.nd_obj.name, (yytext)); return GE; }
"<="                { strcpy(yylval.nd_obj.name, (yytext)); return LE; }
">"                 { strcpy(yylval.nd_obj.name, (yytext)); return GT; }
"<"                 { strcpy(yylval.nd_obj.name, (yytext)); return LT; }
"samana"            { strcpy(yylval.nd_obj.name, (yytext)); return EQ; }
"samanana"          { strcpy(yylval.nd_obj.name, (yytext)); return NE; }

"oui"               { strcpy(yylval.nd_obj.name, (yytext)); return TRUE; }
"non"               { strcpy(yylval.nd_obj.name, (yytext)); return FALSE; }
"&&"                { strcpy(yylval.nd_obj.name, (yytext)); return AND; }
"||"                { strcpy(yylval.nd_obj.name, (yytext)); return OR; }


"number"            { strcpy(yylval.nd_obj.name, (yytext)); return INT; }
"decimal"           { strcpy(yylval.nd_obj.name, (yytext)); return FLOAT; }
"letter"            { strcpy(yylval.nd_obj.name, (yytext)); return CHAR; }
"boolean"           { strcpy(yylval.nd_obj.name, (yytext)); return BOOL; }

\#.*                { ; }
[ \t]*              { ; }
[\n]                { line_no++; }


[-]?{digit}+                        { strcpy(yylval.nd_obj.name, (yytext)); return NUMBER; }
[-]?{digit}+\.{digit}{1,6}          { strcpy(yylval.nd_obj.name, (yytext)); return FLOAT_NUM; }
{alphabet}({alphabet}|{digit})*     { strcpy(yylval.nd_obj.name, (yytext)); return ID; }



["].*["]                             { strcpy(yylval.nd_obj.name, (yytext)); return STR; }
['].[']                              { strcpy(yylval.nd_obj.name, (yytext)); return CHARACTER; }

"."|"="|","|"{"|"}"|"("|")"|"["|"]"|"*"|"+"|"-"|"/"|"?"|":"|"&"|"|"|"^"|"!"|"~"|"%"|"<"|">" { strcpy(yylval.nd_obj.name, (yytext)); return yytext[0]; }

%%

int yywrap(void) {
   return 1;
}