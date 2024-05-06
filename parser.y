%{
    #include<stdio.h>
    #include<string.h>
    #include<stdlib.h>
    #include<ctype.h>
    #include<math.h>
    #include"lex.yy.c"
    
    int yyerror(const char *s);
    int yylex(void);
    int yywrap();

    float get_value(char *var);
    void set_value(char *var, float value);
    struct node* create_node(struct node *left, struct node *right, char *token, float value);
    void outputTree(struct node*);
    struct node *head;
    struct node { 
        struct node *left; 
        struct node *right; 
        char *token;
        float value;
    };
    int success = 1; // For checking if parsing was successful or not

    void check_declaration(char *c);
    struct dataType {
        char * id_name;
        char * data_type;
        char * type;
        float value;
        int line_no;
    } symbol_table[40];

    void insert_type();
    void add(char c);
    void check_return_type(char *);
	int check_types(char *, char *);
	char *get_type(char *);
    char *get_ftype(char *);
    int search (char* type);
    void optimize();
    char type[10];
    int count = 0;

    extern int line_no;

    int q;
    int sem_errors=0;
	int label=0;
	char buff[100];
	char errors[10][100];
	char reserved[16][20] = {"announcing", "batao", "maybe", "otherwise", "poka", "kaerimasu", "log", "exp", "oui", "non", "adhavadhu", "number", "decimal", "letter", "boolean"};

    int ic_idx=0;
    int temp_var=0;
    int is_for=0;
    int is_array=0;
    int temp_arr_var = 0;
    
    char icg[100][100];
    char currentFuncName[100];
%}

%union { 
	struct var_name { 
		char name[100]; 
		struct node* nd;
        float value;
	} nd_obj;

    struct var_name2 { 
        char name[100]; 
        struct node* nd;
        int temp_arr_var;
        char type[10];
        float value;
    } nd_obj2; 

    struct var_name3 { 
        char name[100]; 
        struct node* nd;
        char type[10];
        int temp_arr_var;

        int tlist[100];
        int tlistsize;
        int flist[100];
        int flistsize;

        int label_for_while_start;

        float value;

    } nd_obj3;

    struct var_name4 {
        int next_quad;
    } nd_obj4;
}

%token <nd_obj> PRINTFF SCANFF IF ELSE WHILE RETURN DECLARE ADD SUB MULT DIV LOG POW GE LE GT LT EQ NE TRUE FALSE AND OR INT FLOAT CHAR BOOL NUMBER FLOAT_NUM ID STR CHARACTER
%type <nd_obj> program entry datatype body block else statement exponent mulops addops relop return input
%type <nd_obj2> init value expression term factor base charbool valcharbool printparam bools array assign
%type <nd_obj3> condition args typeArgs
%type <nd_obj4> M
%define parse.error verbose
%%

program: entry '(' typeArgs ')' {
    sprintf(icg[ic_idx++], "\nFunction: %s\n", $1.name);
} '{' body return '}' program { 
	struct node *main = create_node($7.nd, $8.nd, $1.name, 0); 
	struct node *args = create_node(NULL, $3.nd, "F-PARAMS", 0); 
	struct node *wrapper = create_node(args, main, "FUNCTION", 0);
	$$.nd = create_node($10.nd, wrapper, "SOURCE-CODE", 0);
	head = $$.nd; 
}
| { $$.nd = NULL; }
;


entry: datatype ID { 
    add('F');
    strcpy($$.name, $2.name); strcpy(currentFuncName, $2.name);
}
;

datatype: INT { insert_type(); $$.nd = create_node(NULL, NULL, $1.name, 0); }
| FLOAT { insert_type(); $$.nd = create_node(NULL, NULL, $1.name, 0); }
| CHAR { insert_type(); $$.nd = create_node(NULL, NULL, $1.name, 0); }
| BOOL { insert_type(); $$.nd = create_node(NULL, NULL, $1.name, 0); }
;

body: block body {$$.nd = create_node($1.nd, $2.nd, "body", 0);}
| { $$.nd = NULL; }
;

block: WHILE {add('K'); is_for = 1;} '(' condition ')' {
    sprintf(icg[ic_idx++], "LABEL L%d:\n", label++);
    for(int i=0; i < $4.tlistsize; i++){
        char temp[50];
        sprintf(temp, "GOTO L%d\n", label-1);
        strcat(icg[$4.tlist[i]], temp);
    }
} '{' body '}' { 
    $$.nd = create_node($4.nd, $8.nd, $1.name, 0); 
    sprintf(icg[ic_idx++], "JUMP TO L%d\n", $4.label_for_while_start);
    for(int i=0; i < $4.flistsize; i++){
        char temp[50];
        sprintf(temp, "GOTO L%d\n", label);
        strcat(icg[$4.flist[i]], temp);
    }
	sprintf(icg[ic_idx++], "LABEL L%d:\n", label++);
}

| IF {add('K'); is_for = 0;} '(' condition ')' {
    sprintf(icg[ic_idx++], "\nLABEL L%d:\n", label++);
    for (int i=0; i < $4.tlistsize; i++) {
        char temp[50];
        sprintf(temp, "GOTO L%d\n", label-1);
        strcat(icg[$4.tlist[i]], temp);
    }
} '{' body '}' {
    sprintf(icg[ic_idx++],"JUMP TO L%d\n", label+1);
    sprintf(icg[ic_idx++], "LABEL L%d:\n", label++);
    for(int i=0; i<$4.flistsize; i++){
        char temp[50];
        sprintf(temp, "GOTO L%d\n", label-1);
        sprintf(icg[$4.flist[i]], "%s", temp);
    } 
} else { 
    struct node *iff = create_node($4.nd, $8.nd, $1.name, 0); 
    $$.nd = create_node(iff, $11.nd, "IF-ELSE-BLOCK", 0); 
    sprintf(icg[ic_idx++], "\nLABEL L%d:\n", label++);
}

| statement '.' { $$.nd = $1.nd; }

| PRINTFF {add('K');} '(' printparam ')' '.' { 
    $$.nd = create_node($4.nd, NULL, $1.name, 0); 
    sprintf(icg[ic_idx++], "\nFunc-Call: print \n");
}

| SCANFF {add('K');} '(' STR ',' '&' input ')' '.' { 
    struct node* n1 = create_node(NULL,NULL,$4.name,0); 
    struct node* n2 = create_node(NULL,NULL,$7.name,0); 
    $$.nd = create_node(n1, n2, $1.name,0); 
    sprintf(icg[ic_idx++], "t%d = %s\n", temp_var, $4.name);
    sprintf(icg[ic_idx++], "PARAM t%d\n", temp_var++);
    sprintf(icg[ic_idx++], "PARAM %s\n", $7.name);
    sprintf(icg[ic_idx++], "Func-Call: scanf\n");
}
| ID '(' args ')' '.' {
	if (strcmp(get_ftype($1.name) ,"Function") != 0 ){
		sprintf(errors[sem_errors], "Line %d: Unpermitted operation on not Function \"%s \"!\n", count+1, $1.name);
		sem_errors++;	
	}
	struct node *args = create_node(NULL, $3.nd, "F-PARAMS", 0); 
	$$.nd = create_node($1.nd, args, "FUNC-CALL", 0);
	sprintf(icg[ic_idx++], "Func-Call: %s\n", $1.name);
}
;

input : ID {
    strcpy($$.name,$1.name);
}
| array {
    is_array=0;
    sprintf($$.name,"u%d",$1.temp_arr_var);
} 
;

typeArgs : datatype ID {

    // Add variable param to symbol table
    symbol_table[count].id_name=strdup($2.name);
    symbol_table[count].data_type=strdup($1.name);
    symbol_table[count].line_no=line_no;
    symbol_table[count].type=strdup("Variable");   
    count++; 

} ',' typeArgs  { 
	sprintf(icg[ic_idx++], "\nArg: %s:%s\n", $1.name,$2.name);
	char temp[200];
	sprintf(temp, "%s:%s", $1.name,$2.name);
	struct node *tempNode = create_node(NULL,NULL,temp,0); 
	$$.nd = create_node(tempNode, $5.nd, "args", 0); 
}
| datatype ID {

    // Add variable param to symbol table
    symbol_table[count].id_name=strdup($2.name);
    symbol_table[count].data_type=strdup($1.name);
    symbol_table[count].line_no=line_no;
    symbol_table[count].type=strdup("Variable");   
    count++; 

	sprintf(icg[ic_idx++], "\nArg: %s:%s\n", $1.name,$2.name);
	char temp[200]; 
	sprintf(temp, "%s:%s", $1.name,$2.name);
	$$.nd = create_node(NULL,NULL,temp,0); 
}
| { $$.nd = NULL; }
;

args : expression ',' args { 
	sprintf(icg[ic_idx++], "PARAM %s\n", $1.name); 
	$$.nd = create_node($1.nd, $3.nd, "args", 0); 
}
| value ',' args { 
	sprintf(icg[ic_idx++], "PARAM %s\n", $1.name); 
	$$.nd = create_node($1.nd, $3.nd, "args", 0); 
}
| ID ',' args { 
	sprintf(icg[ic_idx++], "PARAM %s\n", $1.name); 
	$$.nd = create_node($1.nd, $3.nd, "args", 0); 
}
| value { 
	sprintf(icg[ic_idx++], "PARAM %s\n", $1.name); 
	char temp[100]; sprintf(temp, "%s", $1.name);
	$$.nd = create_node(NULL,NULL,temp,0); 
}
| expression { 
	sprintf(icg[ic_idx++], "PARAM %s\n", $1.name); 
	$$.nd = $1.nd; 
} 
| ID { 
	sprintf(icg[ic_idx++], "PARAM %s\n", $1.name); 
	char temp[100]; sprintf(temp, "%s", $1.name);
	$$.nd = create_node(NULL,NULL,temp,0); 
}
| { $$.nd = NULL; }
;

printparam: valcharbool { 
    $$.nd = $1.nd; 
    sprintf(icg[ic_idx++], "\nPARAM %s\n", $1.name);
}
| STR { 
    $$.nd = create_node(NULL, NULL, $1.name, 0); 
    sprintf(icg[ic_idx++], "\nt%d = %s\nPARAM t%d\n", temp_var++, $1.name, temp_var);
}

else: ELSE {add('K');} '{' body '}' { $$.nd = create_node(NULL, $4.nd, $1.name, 0); }
|  { $$.nd = NULL; }
;

M: { 
    $$.next_quad = ic_idx; 
    char new1[100]; sprintf(new1, "%d:\n", ic_idx);
    char new2[100]; sprintf(new2, "LABEL S");
    strcat(new2, new1);
    strcpy(icg[ic_idx], new2);
 };

condition: condition AND M condition {
    $$.nd = create_node($1.nd, $4.nd, "AND", 0); 
    // Backpatch
    for (int i = 0; i < $1.tlistsize; i++) {
        char temp[50]; sprintf(temp, "%d\n", $3.next_quad);
        char temp2[50]; sprintf(temp2, "GOTO S");
        strcat(temp2, temp);
        strcat(icg[$1.tlist[i]], temp2);
    }
    $$.tlistsize = 0;
    $$.flistsize = 0;
    // Assign Tlist
    for (int i=0; i < $4.tlistsize; i++) {
        $$.tlist[$$.tlistsize++] = $4.tlist[i];
    }
    // Merge Flists
    for (int i=0; i < $1.flistsize; i++) {
        $$.flist[$$.flistsize++] = $1.flist[i];
    }
    for(int i=0; i < $4.flistsize; i++){
        $$.flist[$$.flistsize++] = $4.flist[i];
    }
    $$.label_for_while_start = $1.label_for_while_start;
}
| condition OR M condition {
    $$.nd = create_node($1.nd, $4.nd, "OR", 0); 
    // Backpatch
    for (int i = 0; i < $1.flistsize; i++) {
        char temp[50]; sprintf(temp, "%d\n", $3.next_quad);
        char temp2[50]; sprintf(temp2, "GOTO S");
        strcat(temp2, temp);
        strcat(icg[$1.flist[i]], temp2);
    }
    $$.tlistsize = 0;
    $$.flistsize = 0;
    // Merge Tlists
    for (int i = 0; i < $1.tlistsize; i++) {
        $$.tlist[$$.tlistsize++] = $1.tlist[i];
    }
    for (int i = 0; i < $4.tlistsize; i++) {
        $$.tlist[$$.tlistsize++] = $4.tlist[i];
    }
    // Assign Flist
    for(int i=0;i<$4.flistsize;i++){
        $$.flist[$$.flistsize++] = $4.flist[i];
    }
    $$.label_for_while_start = $1.label_for_while_start;
}
| expression relop charbool {
    if(strcmp($1.type, $3.type)){
        success=0;
        sprintf(errors[sem_errors], "Line %d: Conflicting type operations \n", line_no);
        sem_errors++;
    }
    $$.nd = create_node($1.nd, $3.nd, $2.name, 0); 
    if (is_for) {
		$$.label_for_while_start = label;
        sprintf(icg[ic_idx++], "LABEL L%d:\n", label++);
        is_for=0;
	}
    char ifstt[400];
    if (is_array == 0) {
        if (strcmp($2.name, "samana") == 0) {
            sprintf(ifstt, "if %s %s %s ", $1.name, "==", $3.name);
        } else if (strcmp($2.name, "samanana") == 0) {
            sprintf(ifstt, "if %s %s %s ", $1.name, "!=", $3.name);
        } else {
            sprintf(ifstt, "if %s %s %s ", $1.name, $2.name, $3.name);
        }
    } else {
        if (strcmp($2.name, "samana") == 0) {
            sprintf(ifstt, "if u%d %s u%d ", $1.temp_arr_var, "==", $3.temp_arr_var);
        } else if (strcmp($2.name, "samanana") == 0) {
            sprintf(ifstt, "if u%d %s u%d ", $1.temp_arr_var, "!=", $3.temp_arr_var);
        } else {
            sprintf(ifstt, "if u%d %s u%d ", $1.temp_arr_var, $2.name, $3.temp_arr_var);
        }
        is_array = 0;
    }
    strcat(icg[ic_idx++], ifstt);
    $$.tlistsize = 0;
    $$.flistsize = 0;
    $$.tlist[$$.tlistsize++] = ic_idx-1;
    $$.flist[$$.flistsize++] = ic_idx++;
}
| expression relop expression { 
    if(strcmp($1.type, $3.type)){
        success=0;
        sprintf(errors[sem_errors], "Line %d: Conflicting type operations \n", line_no);
        sem_errors++;
    }
    $$.nd = create_node($1.nd, $3.nd, $2.name, 0); 
    if (is_for) {
		$$.label_for_while_start = label;
        sprintf(icg[ic_idx++], "LABEL L%d:\n", label++);
        is_for=0;
	}
    char ifstt[400];
    if (is_array == 0) {
        if (strcmp($2.name, "samana") == 0) {
            sprintf(ifstt, "if %s %s %s ", $1.name, "==", $3.name);
        } else if (strcmp($2.name, "samanana") == 0) {
            sprintf(ifstt, "if %s %s %s ", $1.name, "!=", $3.name);
        } else {
            sprintf(ifstt, "if %s %s %s ", $1.name, $2.name, $3.name);
        }
    } else {
        if (strcmp($2.name, "samana") == 0) {
            sprintf(ifstt, "if u%d %s u%d ", $1.temp_arr_var, "==", $3.temp_arr_var);
        } else if (strcmp($2.name, "samanana") == 0) {
            sprintf(ifstt, "if u%d %s u%d ", $1.temp_arr_var, "!=", $3.temp_arr_var);
        } else {
            sprintf(ifstt, "if u%d %s u%d ", $1.temp_arr_var, $2.name, $3.temp_arr_var);
        }
        is_array = 0;
    }
    strcat(icg[ic_idx++], ifstt);
    $$.tlistsize = 0;
    $$.flistsize = 0;
    $$.tlist[$$.tlistsize++] = ic_idx-1;
    $$.flist[$$.flistsize++] = ic_idx++;
}
| '(' condition ')' { 
    $$.nd = $2.nd; 
    $$.tlistsize = $2.tlistsize;
    $$.flistsize = $2.flistsize;
    for(int i=0; i < $2.tlistsize; i++){
        $$.tlist[i] = $2.tlist[i];
    }
    for(int i=0; i < $2.flistsize; i++){
        $$.flist[i] = $2.flist[i];
    }
    $$.label_for_while_start = $2.label_for_while_start;
}
| TRUE {add('K');} { $$.nd = NULL; }
| FALSE {add('K');}  { $$.nd = NULL; }
;

statement: DECLARE datatype ID { add('V'); } init { 
    $3.nd = create_node(NULL, NULL, $3.name, 0);
    int t = check_types($2.name, $5.type); 
    if(t == 1) { 
        success=0;
        sprintf(errors[sem_errors], "Line %d: Variable \"%s\" has a different type than expected!\n", line_no, $3.name);
        sem_errors++;
    } else {
        $1.nd = create_node($2.nd, $3.nd, $1.name, 0);
        $3.value = $5.value;
        $3.nd->value = $5.value;
        set_value($3.name, $5.value);
        if (t == 2) {
            struct node *temp = create_node(NULL, $5.nd, "inttofloat", 0);
            $$.nd = create_node($1.nd, temp, "DECLARE-BLOCK", 0);
            sprintf(icg[ic_idx++], "%s = %s\n", $3.name, $5.name);
        } else {
            $$.nd = create_node($1.nd, $5.nd, "DECLARE-BLOCK", 0); 
            sprintf(icg[ic_idx++], "%s = %s\n", $3.name, $5.name);
        }
    }
} 
| ID { check_declaration($1.name); } '=' assign { 
    if (is_array == 0) {
        char *id_type = get_type($1.name);
        if (id_type != NULL && check_types(id_type, $4.type) == 1) {
            success=0;
            sprintf(errors[sem_errors], "Line %d: Variable \"%s\" has a different type than expected!\n", line_no, $1.name);
            sem_errors++;
        }
        if (id_type != NULL && check_types(id_type, "decimal") && check_types($4.type, "number")) {
            $1.nd = create_node(NULL, NULL, $1.name, 0); 
            struct node *temp = create_node(NULL, $4.nd, "inttofloat", 0);
            $$.nd = create_node(NULL, temp, "=", 0);
            $1.value = $4.value;
            $1.nd->value = $4.value;
            set_value($1.name, $4.value);
            sprintf(icg[ic_idx++], "%s = %s\n", $1.name, $4.name);
        } else {
            $1.nd = create_node(NULL, NULL, $1.name, 0); 
            $$.nd = create_node(NULL, $4.nd, "=", 0);
            $1.value = $4.value;
            $1.nd->value = $4.value;
            set_value($1.name, $4.value);
            sprintf(icg[ic_idx++], "%s = %s\n", $1.name, $4.name);
        }
    } else {
        is_array = 0;
        $$.nd = create_node($1.nd,$4.nd,"=",0);
        sprintf(icg[ic_idx++],"%s = u%d\n", $1.name,$4.temp_arr_var);
    }
}
| array '=' expression {
    is_array=0;
    $$.nd = create_node($1.nd,$3.nd,"=",0);
    sprintf(icg[ic_idx++],"u%d = %s\n", $1.temp_arr_var, $3.name);
}
| array '=' array {
    is_array = 0;
    $$.nd = create_node($1.nd,$3.nd,"=",0);
    sprintf(icg[ic_idx++],"u%d = u%d\n", $1.temp_arr_var,$3.temp_arr_var);
}
;

assign: expression {
    strcpy($$.type,$1.type);
    $$.nd = $1.nd;
    $$.value = $1.value;
}
| array {
    $$.nd = $1.nd;
    $$.temp_arr_var = $1.temp_arr_var;
}
| ID '(' args ')' {
    strcpy($$.type,"number");
    $$.nd = create_node($1.nd,$3.nd,"call",0);
    $$.value = 0;
}

init: '=' charbool  { $$.nd = $2.nd; strcpy($$.type, $2.type); $$.value = $2.value; $$.nd->value = $2.value; }
| '=' expression { 
    $$.nd = $2.nd; 
    strcpy($$.type, $2.type); 
    strcpy($$.name, $2.name);
    $$.value = $2.value;
    $$.nd->value = $2.value;
}
| '[' NUMBER ']' {
    char temp[100] = "";
    char temp2[2];
    char temp3[2];
    sprintf(temp2, "[");
    sprintf(temp3, "]");
    strcat(temp, temp2);
    strcat(temp, $2.name);
    strcat(temp, temp3);
    strcpy($$.name, temp);
    $$.nd = create_node(NULL, NULL, $$.name, 0);
    sprintf($$.type, "number");
    is_array=1;
}
;


expression : expression addops term { 
    if (check_types($1.type, $3.type) == 1) {
        success=0;
        sprintf(errors[sem_errors], "Line %d: Conflicting type operations \n", line_no);
        sem_errors++;
    } else {
        sprintf($$.name, "t%d", temp_var);
        temp_var++;
        if ((!strcmp($1.type, "decimal" ) && !strcmp($3.type, "number"))) {
            struct node *temp = create_node(NULL, $3.nd, "inttofloat", 0);
            strcpy($$.type, "decimal");
            $$.nd = create_node($1.nd, temp, $2.name, 0);
            $$.value = $1.value + $3.value;
            $$.nd->value = $1.value + $3.value;
            sprintf(icg[ic_idx++], "%s = %s %s %s\n", $$.name, $1.name, $2.name, $3.name);

        } else if ((!strcmp($1.type, "number") && !strcmp($3.type, "decimal"))) {
            struct node *temp = create_node(NULL, $1.nd, "inttofloat", 0);
            strcpy($$.type, "decimal");
            $$.nd = create_node(temp, $3.nd, $2.name, 0);
            $$.value = $1.value + $3.value;
            $$.nd->value = $1.value + $3.value;
            sprintf(icg[ic_idx++], "%s = %s %s %s\n", $$.name, $1.name, $2.name, $3.name);
            
        } else {
            strcpy($$.type, $1.type);
            $$.nd = create_node($1.nd, $3.nd, $2.name, 0);
            $$.value = $1.value + $3.value;
            $$.nd->value = $1.value + $3.value;
            sprintf(icg[ic_idx++], "%s = %s %s %s\n", $$.name, $1.name, $2.name, $3.name);
        }
    }
} 
| term { $$.nd = $1.nd; strcpy($$.type, $1.type); }
;

term : term mulops factor { 
    if (check_types($1.type, $3.type) == 1) {
        success=0;
        sprintf(errors[sem_errors], "Line %d: Conflicting type operations \n", line_no);
        sem_errors++;
    } else {
        sprintf($$.name, "t%d", temp_var);
        temp_var++;
        if ((!strcmp($1.type, "decimal" ) && !strcmp($3.type, "number"))) {
            struct node *temp = create_node(NULL, $3.nd, "inttofloat", 0);
            strcpy($$.type, "decimal");
            $$.nd = create_node($1.nd, temp, $2.name, 0);
            $$.value = $1.value * $3.value;
            $$.nd->value = $1.value * $3.value;
            sprintf(icg[ic_idx++], "%s = %s %s %s\n", $$.name, $1.name, $2.name, $3.name);

        } else if ((!strcmp($1.type, "number") && !strcmp($3.type, "decimal"))) {
            struct node *temp = create_node(NULL, $1.nd, "inttofloat", 0);
            strcpy($$.type, "decimal");
            $$.nd = create_node(temp, $3.nd, $2.name, 0);
            $$.value = $1.value * $3.value;
            $$.nd->value = $1.value * $3.value;
            sprintf(icg[ic_idx++], "%s = %s %s %s\n", $$.name, $1.name, $2.name, $3.name);
            
        } else {
            strcpy($$.type, $1.type);
            $$.nd = create_node($1.nd, $3.nd, $2.name, 0);
            $$.value = $1.value * $3.value;
            $$.nd->value = $1.value * $3.value;
            sprintf(icg[ic_idx++], "%s = %s %s %s\n", $$.name, $1.name, $2.name, $3.name);
        }
    }
} 
| factor { $$.nd = $1.nd; strcpy($$.type, $1.type); }
; 

factor : base exponent base { 
    if (check_types($1.type, $3.type) == 1) {
        success=0;
        sprintf(errors[sem_errors], "Line %d: Conflicting type operations \n", line_no);
        sem_errors++;
    } else {
        sprintf($$.name, "t%d", temp_var);
        temp_var++;
        if ((!strcmp($1.type, "decimal" ) && !strcmp($3.type, "number"))) {
            struct node *temp = create_node(NULL, $3.nd, "inttofloat", 0);
            strcpy($$.type, "decimal");
            $$.nd = create_node($1.nd, temp, $2.name, 0);
            $$.value = pow($1.value, $3.value);
            $$.nd->value = pow($1.value, $3.value);
            sprintf(icg[ic_idx++], "%s = %s %s %s\n", $$.name, $1.name, $2.name, $3.name);

        } else if ((!strcmp($1.type, "number") && !strcmp($3.type, "decimal"))) {
            struct node *temp = create_node(NULL, $1.nd, "inttofloat", 0);
            strcpy($$.type, "decimal");
            $$.nd = create_node(temp, $3.nd, $2.name, 0);
            $$.value = pow($1.value, $3.value);
            $$.nd->value = pow($1.value, $3.value);
            sprintf(icg[ic_idx++], "%s = %s %s %s\n", $$.name, $1.name, $2.name, $3.name);

        } else {
            strcpy($$.type, $1.type);
            $$.nd = create_node($1.nd, $3.nd, $2.name, 0);
            $$.value = pow($1.value, $3.value);
            $$.nd->value = pow($1.value, $3.value);
            sprintf(icg[ic_idx++], "%s = %s %s %s\n", $$.name, $1.name, $2.name, $3.name);
        }
    } 
} 
| LOG '(' value ',' value ')' { $$.nd = create_node($3.nd, $5.nd, $1.name, 0); }
| base { $$.nd = $1.nd; strcpy($$.type, $1.type); $$.value = $1.value; $$.nd->value = $1.value; }
;

base : value { $$.nd = $1.nd; strcpy($$.type, $1.type); $$.value = $1.value; $$.nd->value = $1.value;}
| '(' expression ')' { $$.nd = $2.nd; strcpy($$.type, $2.type); }
;


exponent: POW
;

mulops: MULT
| DIV
;

addops: ADD 
| SUB 
;

relop: LT
| GT
| LE
| GE
| EQ
| NE
;

value: NUMBER { 
    strcpy(type, "number");
    add('C'); 
    strcpy($$.name, $1.name);
    strcpy($$.type, "number");
    $$.nd = create_node(NULL, NULL, $1.name, atoi($1.name));
    $$.value = atoi($1.name);
}
| FLOAT_NUM { 
    strcpy(type, "decimal");
    add('C');
    strcpy($$.name, $1.name);
    strcpy($$.type, "decimal");
    $$.nd = create_node(NULL, NULL, $1.name, atof($1.name)); 
    $$.value = atof($1.name);
}
| ID {
    check_declaration($1.name);
    strcpy($$.name, $1.name);
    char *id_type = get_type($1.name);
    if (id_type != NULL) strcpy($$.type, id_type);
    $$.nd = create_node(NULL, NULL, $1.name, 0); 
    q = search($1.name);
    if(q==-1) {
        $$.value = get_value($1.name);
        $$.nd->value = $$.value;
    }
} 
| array {
    $$.nd = $1.nd; strcpy($$.type,$1.type); strcpy($$.name,$1.name); $$.temp_arr_var = $1.temp_arr_var;
}
;

array: ID '[' expression ']' { 
    check_declaration($1.name);
    char *id_type = get_type($1.name);
    if(id_type!=NULL) strcpy($$.type,id_type); 
    char temp[100] = "";
    strcat(temp, $1.name);
    strcat(temp, "[");
    strcat(temp, $3.name);
    strcat(temp, "]");
    strcpy($$.name, temp);
    $$.nd = create_node(NULL, NULL, $$.name,0); 
    is_array = 1;
    $$.temp_arr_var = temp_arr_var++;
    sprintf(icg[ic_idx++],"t%d = 4 * %s\n",temp_var++,$3.name);
    sprintf(icg[ic_idx++],"u%d = %s[t%d]\n",$$.temp_arr_var,$1.name,temp_var-1);
}

charbool: bools { $$.nd = $1.nd; strcpy($$.type, "boolean"); }
| CHARACTER { add('C'); $$.nd = create_node(NULL, NULL, $1.name, 0); strcpy($$.type,"letter"); }
;

valcharbool: value { $$.nd = $1.nd; strcpy($$.type, $1.type); strcpy($$.name,$1.name); $$.temp_arr_var = $1.temp_arr_var; }
| charbool { $$.nd = create_node(NULL, NULL, $1.name, 0); strcpy($$.type, $1.type); }
;

bools: TRUE { strcpy(type, "boolean"); add('C'); $$.nd = create_node(NULL, NULL, $1.name, 0);}
| FALSE { strcpy(type, "boolean"); add('C'); $$.nd = create_node(NULL, NULL, $1.name, 0); }
;

return: RETURN {add('K');} valcharbool '.' { 
    check_return_type($3.type);
    $$.nd = create_node($3.nd, NULL, $1.name, 0);
    sprintf(icg[ic_idx++], "\nReturn: %s\n", $3.name);
}
| { $$.nd = NULL; } 
;

%%

int yyerror(const char *msg)
{
    extern int yylineno;
    printf("Parsing Failed\nLine Number: %d %s\n",yylineno,msg);
    success = 0;
    return 0;
}

int main() {   
    extern FILE *yyin, *yyout;
    int p = 0;
    p = yyparse();
    printf("\nSYMBOL   DATATYPE   TYPE   LINE NUMBER \n");
	printf("_______________________________________\n\n");
	int i=0;
	for(i=0; i<count; i++) {
		printf("%s\t%s\t%s\t%d\t\n", symbol_table[i].id_name, symbol_table[i].data_type, symbol_table[i].type, symbol_table[i].line_no);
	}
	for(i=0;i<count;i++) {
		free(symbol_table[i].id_name);
		free(symbol_table[i].type);
	}
	printf("\n\n");
    if(sem_errors>0) {
		printf("Semantic analysis completed with %d errors\n", sem_errors);
		for (int i=0; i<sem_errors; i++) {
			printf("\t - %s", errors[i]);
        }
	} else { 
        printf("Parsing Successful\n\n\n");
        printf("PARSE TREE\n\n");
        outputTree(head);
        printf("\n");

        printf("\n");
        printf("INTERMEDIATE CODE GENERATION \n\n");

        FILE *fptr;
        fptr = fopen("icg.txt", "w");
        for (int i=0; i<ic_idx; i++){
            printf("%s", icg[i]);
            fprintf(fptr, "%s", icg[i]);
        }
        /* printf("\n");
        optimize();
        printf("\n");
        printf("OPTIMIZED INTERMEDIATE CODE GENERATION \n\n");
        for (int i=0; i<ic_idx; i++){
            printf("%s", icg[i]);
        } */
        fclose(fptr);
    }
    return p;
}


void check_declaration(char *c) {  
    q = search(c);    
    if (!q) {
        sprintf(errors[sem_errors], "Line %d: Variable \"%s\" not declared before usage!\n", line_no, c);  
        sem_errors++;
    }
}

int check_types(char *type1, char *type2) {
    if(!strcmp(type1, "decimal") && !strcmp(type2, "number")) return 2;
    if(!strcmp(type1, "number") && !strcmp(type2, "decimal")) return 3;
    if(strcmp(type1, type2)){
        return 1;
    }
    return 0;
}

void check_return_type(char *value) {
	char *main_datatype = get_type(currentFuncName);
	if (strcmp(main_datatype, value)) {
		sprintf(errors[sem_errors], "Line %d: Return type mismatch\n", line_no);
		sem_errors++;
    }
}

char *get_type(char *var){
	for (int i=0; i<count; i++) {
		if (!strcmp(symbol_table[i].id_name, var)) {
			return symbol_table[i].data_type;
		}
	}
    return NULL;
}

char *get_ftype(char *var){
	for(int i=0; i<count; i++) {
		// Handle case of use before declaration
		if(!strcmp(symbol_table[i].id_name, var)) {
			return symbol_table[i].type;
		}
	}
}

float get_value(char *var){
    for(int i=0; i<count; i++) {
        if(!strcmp(symbol_table[i].id_name, var)) {
            return symbol_table[i].value;
        }
    }
    return 0;
}

void set_value(char *var, float value){
    for(int i=0; i<count; i++) {
        if(!strcmp(symbol_table[i].id_name, var)) {
            symbol_table[i].value = value;
            return;
        }
    }
}

int search(char *type) { 
    int i; 
    for (i=count-1; i>=0; i--) {
        if (strcmp(symbol_table[i].id_name, type) == 0) {   
            return -1;
            break;  
        }
    } 
    return 0;
}

void insert_type() {
    strcpy(type, yytext);
}

void add(char c) {
    q = search(yytext);
    if(c == 'V' && q) {
        sprintf(errors[sem_errors], "Line %d: Multiple declarations of \"%s\" not allowed!\n", line_no, yytext);  
        sem_errors++;    
    }
    if(c == 'V') {  
        for(int i=0; i<10; i++) {   
            if(!strcmp(reserved[i], strdup(yytext))) {
                sprintf(errors[sem_errors], "Line %d: Variable name  \"%s\" is a reserved keyword!\n", line_no, yytext);
                sem_errors++;    
                return;
            }  
        } 
    }
    if(!q) {
        if (c == 'K') {
            symbol_table[count].id_name=strdup(yytext);
            symbol_table[count].data_type=strdup("N/A");
            symbol_table[count].line_no=line_no;
            symbol_table[count].type=strdup("Keyword\t");   
            count++;  
        } else if (c == 'V') {
            symbol_table[count].id_name=strdup(yytext);
            symbol_table[count].data_type=strdup(type);
            symbol_table[count].line_no=line_no;
            symbol_table[count].type=strdup("Variable");   
            count++;  
        } else if (c == 'C') {
            symbol_table[count].id_name=strdup(yytext);
            symbol_table[count].data_type=strdup(type);
            symbol_table[count].line_no=line_no;
            symbol_table[count].type=strdup("Constant");   
            count++;  
        } else if (c == 'F') {
            symbol_table[count].id_name=strdup(yytext);
            symbol_table[count].data_type=strdup(type);
            symbol_table[count].line_no=line_no;
            symbol_table[count].type=strdup("Function");   
            count++;  
        }
    }
}


void outputTreeHelper(char* prefix, struct node* ptr, int isLeft) {
    if( ptr != NULL ) {
        printf("%s",prefix);
        if(isLeft) { 
            printf("+─── "); 
        } 
		else { 
            printf("`─── "); 
        }
        printf("%s (%0.2f)",ptr->token,ptr->value);
		printf("\n");
		char* addon = isLeft ? "│    " : "     ";
    	int len2 = strlen(addon);
    	int len1 = strlen(prefix);
    	char* result = (char*) malloc(len1 + len2 + 1);
    	strcpy(result, prefix);
    	strcpy(result + len1, addon);
		outputTreeHelper(result, ptr->left, 1);
		outputTreeHelper(result, ptr->right, 0);
    	free(result);
    }
}

void outputTree(struct node* ptr) {
	printf("\n");
    outputTreeHelper("", ptr, 0);
}

struct node* create_node(struct node *left, struct node *right, char *token, float value) {	
	struct node *newnode = (struct node *) malloc(sizeof(struct node)); // Allocate memory for new node
	char *newstr = (char *) malloc(strlen(token)+1); // Allocate memory for new string
	strcpy(newstr, token); // Copy token to new string
	newnode->left = left; // Set left child
	newnode->right = right; // Set right child
	newnode->token = newstr; // Set token
    newnode->value = value; // Set value
	return newnode;
}

void optimize() {
    for (int i=0; i<ic_idx; i++) {
        // Simplify algebraic expressions
        if (strstr(icg[i], "+ 0") || strstr(icg[i], "- 0") || strstr(icg[i], "+ 0.0") || strstr(icg[i], "- 0.0") ||
            strstr(icg[i], "* 1") || strstr(icg[i], "* 1.0") || strstr(icg[i], "/ 1") || strstr(icg[i], "/ 1.0")) {
            char *token = strtok(icg[i], " ");
            char *new_icg = (char *) malloc(100);
            int cnt = 0;
            while (cnt < 3) {
                strcat(new_icg, token);
                strcat(new_icg, " ");
                token = strtok(NULL, " ");
                cnt++;
            }
            char *newline = (char *) malloc(1);
            sprintf(newline, "\n");
            strcat(new_icg, newline);
            sprintf(icg[i], "%s", new_icg);
        }
        if (strstr(icg[i], "0 +") || strstr(icg[i], "1 *") || strstr(icg[i], "0.0 +") || strstr(icg[i], "1.0 *")) {
            char *token = strtok(icg[i], " ");
            char *new_icg = (char *) malloc(100);
            int cnt = 0;
            while (cnt < 5) {
                if (cnt == 2 || cnt == 3) {
                    token = strtok(NULL, " ");
                    cnt++;
                    continue;
                }
                strcat(new_icg, token);
                strcat(new_icg, " ");
                token = strtok(NULL, " ");
                cnt++;
            }
            sprintf(icg[i], "%s", new_icg);
        }
        // Strength Reduction
        if (strstr(icg[i], "* 2") || strstr(icg[i], "* 2.0")) {
            char *token = strtok(icg[i], " ");
            char *new_icg = (char *) malloc(100);
            int cnt = 0;
            while (cnt < 2) {
                strcat(new_icg, token);
                strcat(new_icg, " ");
                token = strtok(NULL, " ");
                cnt++;
            }
            strcat(new_icg, token);
            strcat(new_icg, " + ");
            strcat(new_icg, token);
            strcat(new_icg, " ");   
            char *newline = (char *) malloc(1);
            sprintf(newline, "\n");
            strcat(new_icg, newline);
            sprintf(icg[i], "%s", new_icg);
        }
        if (strstr(icg[i], "2 *") || strstr(icg[i], "2.0 *")) {
            char *token = strtok(icg[i], " ");
            char *new_icg = (char *) malloc(100);
            int cnt = 0;
            while (cnt < 4) {
                if (cnt == 2 || cnt == 3) {
                    token = strtok(NULL, " ");
                    cnt++;
                    continue;
                }
                strcat(new_icg, token);
                strcat(new_icg, " ");
                token = strtok(NULL, " ");
                cnt++;
            }
            token[strcspn(token, "\n")] = 0; // Strip newline
            strcat(new_icg, token);
            strcat(new_icg, " + ");
            strcat(new_icg, token);
            char *newline = (char *) malloc(1);
            sprintf(newline, "\n");
            strcat(new_icg, newline);
            sprintf(icg[i], "%s", new_icg);
        }
	}
}