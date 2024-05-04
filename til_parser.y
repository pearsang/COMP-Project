%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type>               type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                                            i;           /* integer value */
  double                                         d;           /* double value */
  std::string                                   *s;           /* symbol name or string literal */
  cdk::basic_node                               *node;        /* node pointer */
  cdk::sequence_node                            *sequence;
  cdk::expression_node                          *expression;  /* expression nodes */
  cdk::lvalue_node                              *lvalue;
  til::block_node                               *block;
  std::vector<std::shared_ptr<cdk::basic_type>> *type_vec;
};

%token <i> tINTEGER
%token <d> tDOUBLE
%token <s> tIDENTIFIER tSTRING
%token tTYPE_INT tTYPE_DOUBLE tTYPE_STRING tTYPE_VOID
%token tEXTERNAL tFORWARD tPUBLIC tVAR
%token tBLOCK tIF tLOOP tSTOP tNEXT tRETURN tPRINT tPRINTLN
%token tINPUT tNULL tSET tINDEX tOBJECTS tSIZEOF tFUNCTION
%token tPROGRAM

%nonassoc tIFX
%nonassoc tELIF tELSE

%right '='
%left tOR
%left tAND
%nonassoc '~'
%left tEQ tNE
%left tGE tLE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY
%nonassoc '('

%type <sequence> fdecls decls instrs exprs func_args
%type <node> fdecl program decl instr ifotherwise func_arg
%type <type> type referable_type func_return_type func_type ref_type void_ref_type
%type <type_vec> types
%type <block> decls_instrs blk
%type <expression> expr func_definition
%type <lvalue> lval
%type <s> string

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file : fdecls program    { compiler->ast(new cdk::sequence_node(LINE, $2, $1)); }
     | fdecls            { compiler->ast($1); }
     |        program    { compiler->ast(new cdk::sequence_node(LINE, $1)); }
     | /* empty */       { compiler->ast(new cdk::sequence_node(LINE)); }
     ;

fdecls : fdecls fdecl    { $$ = new cdk::sequence_node(LINE, $2, $1); }
       |        fdecl    { $$ = new cdk::sequence_node(LINE, $1); }
       ;

fdecl : tFOREIGN type  tIDENTIFIER          ';'    { $$ = new til::declaration_node(LINE, tFOREIGN, $2, *$3, nullptr); delete $3; }
      | tFORWARD type  tIDENTIFIER          ';'    { $$ = new til::declaration_node(LINE, tFORWARD, $2, *$3, nullptr); delete $3; }
      | tPUBLIC  type  tIDENTIFIER          ';'    { $$ = new til::declaration_node(LINE, tPUBLIC, $2, *$3, nullptr); delete $3; }
      | tPUBLIC  type  tIDENTIFIER '=' expr ';'    { $$ = new til::declaration_node(LINE, tPUBLIC, $2, *$3, $5); delete $3; }
      | tPUBLIC  tAUTO tIDENTIFIER '=' expr ';'    { $$ = new til::declaration_node(LINE, tPUBLIC, nullptr, *$3, $5); delete $3; }
      | tPUBLIC        tIDENTIFIER '=' expr ';'    { $$ = new til::declaration_node(LINE, tPUBLIC, nullptr, *$2, $4); delete $2; }
      |          decl /* no qual; "private" */     { $$ = $1; }
      ;

type : referable_type    { $$ = $1; }
     | void_ref_type     { $$ = $1; }
     ;

referable_type : tTYPE_INT       { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
               | tTYPE_DOUBLE    { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
               | tTYPE_STRING    { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
               | func_type       { $$ = $1; }
               | ref_type        { $$ = $1; }
               ;

func_type : func_return_type '<'       '>'    { $$ = cdk::functional_type::create($1); }
          | func_return_type '<' types '>'    { $$ = cdk::functional_type::create(*$3, $1); delete $3; }
          ;

func_return_type : type          { $$ = $1; }
                 | tTYPE_VOID    { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
                 ;

types : types ',' type    { $$ = $1; $$->push_back($3); }
      | type              { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(1, $1); }
      ;

ref_type : '[' referable_type ']'     { $$ = cdk::reference_type::create(4, $2); }
         ;

void_ref_type : '[' void_ref_type ']'    { $$ = $2; }
              | '[' tTYPE_VOID ']'       { $$ = cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_VOID)); }
              ;

program : tBEGIN decls_instrs tEND    { $$ = new til::function_def_node(LINE, $2); }
        ;

decls_instrs : decls instrs    { $$ = new til::block_node(LINE, $1, $2); }
             | decls           { $$ = new til::block_node(LINE, $1, new cdk::sequence_node(LINE)); }
             |       instrs    { $$ = new til::block_node(LINE, new cdk::sequence_node(LINE), $1); }
             | /* empty */     { $$ = new til::block_node(LINE, new cdk::sequence_node(LINE), new cdk::sequence_node(LINE)); }
             ;

decls : decls decl    { $$ = new cdk::sequence_node(LINE, $2, $1); }
      |       decl    { $$ = new cdk::sequence_node(LINE, $1); }
      ;

decl : type  tIDENTIFIER          ';'    { $$ = new til::declaration_node(LINE, tPRIVATE, $1, *$2, nullptr); delete $2; }
     | type  tIDENTIFIER '=' expr ';'    { $$ = new til::declaration_node(LINE, tPRIVATE, $1, *$2, $4); delete $2; }
     | tAUTO tIDENTIFIER '=' expr ';'    { $$ = new til::declaration_node(LINE, tPRIVATE, nullptr, *$2, $4); delete $2; }
     ;

instrs : instrs instr    { $$ = new cdk::sequence_node(LINE, $2, $1); }
       |        instr    { $$ = new cdk::sequence_node(LINE, $1); }
       ;

instr : expr ';'                              { $$ = new til::evaluation_node(LINE, $1); }
      | exprs tPRINT                          { $$ = new til::print_node(LINE, $1, false); }
      | exprs tPRINTLN                        { $$ = new til::print_node(LINE, $1, true); }
      | tIF '(' expr ')' instr %prec tIFX     { $$ = new til::if_node(LINE, $3, $5); }
      | tIF '(' expr ')' instr ifotherwise    { $$ = new til::if_else_node(LINE, $3, $5, $6); }
      | tWHILE '(' expr ')' instr             { $$ = new til::while_node(LINE, $3, $5); }
      | tSTOP tINTEGER ';'                    { $$ = new til::stop_node(LINE, $2); }
      | tSTOP ';'                             { $$ = new til::stop_node(LINE, 1); }
      | tNEXT tINTEGER ';'                    { $$ = new til::next_node(LINE, $2); }
      | tNEXT ';'                             { $$ = new til::next_node(LINE, 1); }
      | tRETURN expr ';'                      { $$ = new til::return_node(LINE, $2); }
      | tRETURN ';'                           { $$ = new til::return_node(LINE, nullptr); }
      | blk                                   { $$ = $1; }
      ;

ifotherwise : tELSE instr                             { $$ = $2; }
            | tELIF '(' expr ')' instr %prec tIFX     { $$ = new til::if_node(LINE, $3, $5); }
            | tELIF '(' expr ')' instr ifotherwise    { $$ = new til::if_else_node(LINE, $3, $5, $6); }
            ;

blk : '{' decls_instrs '}'    { $$ = $2; }
    ;

exprs : exprs ',' expr    { $$ = new cdk::sequence_node(LINE, $3, $1); }
      |           expr    { $$ = new cdk::sequence_node(LINE, $1); }
      ;

expr : tINTEGER                 { $$ = new cdk::integer_node(LINE, $1); }
     | tDOUBLE                  { $$ = new cdk::double_node(LINE, $1); }
     | string                   { $$ = new cdk::string_node(LINE, *$1); delete $1; }
     | tNULL                    { $$ = new til::nullptr_node(LINE); }
     | '+' expr %prec tUNARY    { $$ = new cdk::unary_plus_node(LINE, $2); }
     | '-' expr %prec tUNARY    { $$ = new cdk::unary_minus_node(LINE, $2); }
     | '~' expr                 { $$ = new cdk::not_node(LINE, $2); }
     | expr '+' expr            { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr            { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '*' expr            { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr            { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr            { $$ = new cdk::mod_node(LINE, $1, $3); }
     | expr '<' expr            { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr '>' expr            { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tGE expr            { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr tLE expr            { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tNE expr            { $$ = new cdk::ne_node(LINE, $1, $3); }
     | expr tEQ expr            { $$ = new cdk::eq_node(LINE, $1, $3); }
     | expr tAND expr           { $$ = new cdk::and_node(LINE, $1, $3); }
     | expr tOR expr            { $$ = new cdk::or_node(LINE, $1, $3); }
     | '[' expr ']'             { $$ = new til::stack_alloc_node(LINE, $2); }
     | '(' expr ')'             { $$ = $2; }
     | tSIZEOF '(' expr ')'     { $$ = new til::sizeof_node(LINE, $3); }
     | lval                     { $$ = new cdk::rvalue_node(LINE, $1); }
     | lval tSET expr            { $$ = new cdk::assignment_node(LINE, $1, $3); }
     | lval '?'                 { $$ = new til::address_node(LINE, $1); }
     | expr '(' exprs ')'       { $$ = new til::function_call_node(LINE, $1, $3); }
     | expr '(' ')'             { $$ = new til::function_call_node(LINE, $1, new cdk::sequence_node(LINE)); }
     | '@'  '(' exprs ')'       { $$ = new til::function_call_node(LINE, nullptr, $3); }
     | '@'  '(' ')'             { $$ = new til::function_call_node(LINE, nullptr, new cdk::sequence_node(LINE)); }
     | func_definition          { $$ = $1; }
     ;

lval : tIDENTIFIER          { $$ = new cdk::variable_node(LINE, $1); }
     | expr '[' expr ']'    { $$ = new til::indexptr_node(LINE, $1, $3); }
     ;

string : string tSTRING    { $$ = $1; $$->append(*$2); delete $2; }
       | tSTRING           { $$ = $1; }
       ;

func_definition : '(' func_args ')'  func_return_type blk    { $$ = new til::function_def_node(LINE, $2, $5, $6); }
                | '('           ')'  func_return_type blk    { $$ = new til::function_def_node(LINE, new cdk::sequence_node(LINE), $4, $5); }
                ;

func_args : func_args ',' func_arg    { $$ = new cdk::sequence_node(LINE, $3, $1); }
          | func_arg                  { $$ = new cdk::sequence_node(LINE, $1); }
          ;

func_arg : type tIDENTIFIER    { $$ = new til::declaration_node(LINE, tPRIVATE, $1, *$2, nullptr); delete $2; }
         ;

%%