#ifndef COOL_TREE_H
#define COOL_TREE_H
//////////////////////////////////////////////////////////
//
// file: cool-tree.h
//
// This file defines classes for each phylum and constructor
//
//////////////////////////////////////////////////////////


#include "tree.h"
#include "cool-tree.handcode.h"
#include <stdlib.h>

#include <stdio.h>

#include <symtab.h>

#include<string>
// define the class for phylum
// define simple phylum - Program
typedef class Program_class *Program;
class method_class;
class attr_class;
template <class SYM, class DAT>
class SymbolTable;
class block_class;
// SymbolTable::SymbolTable(): tbl(NULL) { }

// Defining the constants for the type string
#define COND_TYPE 9
#define LOOP_TYPE 10
#define TYPCASE_TYPE 11
#define BLOCK_TYPE 12
#define LET_TYPE 13
#define PLUS_TYPE 14
#define SUB_TYPE 15
#define MUL_TYPE 16
#define DIVIDE_TYPE 17
#define ASSIGN_TYPE 18
#define STATIC_DISPATCH_TYPE 19
#define DISPATCH_TYPE 20
#define NEG_TYPE 21
#define LT_TYPE 22
#define COMP_TYPE 23
#define LOOP_TPE 24
#define INT_CONST_TYPE 25
#define BOOL_CONST_TYPE 26
#define STRING_CONST_TYPE 27
#define NEW_TYPE 28
#define ISVOID_TYPE 29
#define NO_EXPR_TYPE 30
#define OBJECT_TYPE 31
#define LEQ_CLASS_TYPE 32
#define EQ_CLASS_TYPE 33
// typedef class_ class_

class Program_class : public tree_node {
public:
   tree_node *copy()		 { return copy_Program(); }
   virtual Program copy_Program() = 0;

#ifdef Program_EXTRAS
   Program_EXTRAS
#endif
};


// define simple phylum - Class_
typedef class Class__class *Class_;

class Class__class : public tree_node {
public:
   tree_node *copy()		 { return copy_Class_(); }
   virtual Class_ copy_Class_() = 0;
   virtual Symbol GetName() = 0;
   virtual Symbol GetParent() = 0;
   virtual Features GetFeatures() = 0;

#ifdef Class__EXTRAS
   Class__EXTRAS
#endif
};


// define simple phylum - Feature
typedef class Feature_class *Feature;

class Feature_class : public tree_node {
public:
   bool method;
   tree_node *copy()		 { return copy_Feature(); }
   virtual Feature copy_Feature() = 0;
   virtual Symbol GetName() = 0;

   bool isMethod(){ return method;}
   // virtual Symbol getReturnType() = 0;
   virtual Expression getExpression() = 0;
#ifdef Feature_EXTRAS
   Feature_EXTRAS
#endif
};


// define simple phylum - Formal
typedef class Formal_class *Formal;

class Formal_class : public tree_node {
public:
   tree_node *copy()		 { return copy_Formal(); }
   virtual Formal copy_Formal() = 0;
   virtual Symbol GetName() = 0;
   virtual Symbol GetTypeDecl() = 0;

#ifdef Formal_EXTRAS
   Formal_EXTRAS
#endif
};


// define simple phylum - Expression
typedef class Expression_class *Expression;

class Expression_class : public tree_node {
public:
   int types;
   tree_node *copy()		 { return copy_Expression(); }
   virtual Expression copy_Expression() = 0;
   int getType() { return types;}

#ifdef Expression_EXTRAS
   Expression_EXTRAS
#endif
};


// define simple phylum - Case
typedef class Case_class *Case;

class Case_class : public tree_node {
public:
   tree_node *copy()		 { return copy_Case(); }
   virtual Case copy_Case() = 0;

#ifdef Case_EXTRAS
   Case_EXTRAS
#endif
};


// define the class for phylum - LIST
// define list phlyum - Classes
typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;


// define list phlyum - Features
typedef list_node<Feature> Features_class;
typedef Features_class *Features;


// define list phlyum - Formals
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;


// define list phlyum - Expressions
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;


// define list phlyum - Cases
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;


// define the class for constructors
// define constructor - program
class program_class : public Program_class {
protected:
   Classes classes;
public:
   program_class(Classes a1) {
      classes = a1;
   }
   Program copy_Program();


   void dump(ostream& stream, int n);
   // void checkUnique(ClassTable classtable);
   Class_ getParent(Class_ child);
   bool isCycle();
   bool checkUnique();
   bool checkParentExist();
   bool checkMethodExprValid(Feature method);
   bool checkMethodSignatureValid(Feature method);
   bool methodReturnTypeValid(method_class* method);
   bool checkAttr(attr_class* attr);
   bool checkFeature();
   bool isMethodArgValid(method_class* method);
   bool methodArgUnique(method_class* method);
   // Symbol className = class->GetName();
   template<class SYM, class DAT>
   bool checkMethod(method_class* method, SymbolTable<SYM, DAT>* symbolTable);
   bool checkTypeValid(Symbol type);
   template <class SYM, class DAT>
   Symbol setTypeOfMethod(method_class* method, SymbolTable<SYM,DAT>* symbolTable);
   template<class SYM, class DAT>
   Symbol setTypeInBlock(block_class* body, SymbolTable<SYM,DAT> *symbolTable);
   template<class SYM, class DAT>
   Symbol setTypeForExpression(Expression_class* expression, SymbolTable<SYM, DAT> *symbolTable);
   template<class SYM, class DAT>
   Symbol setTypeForAssign(Expression_class* expression, SymbolTable<SYM,DAT> *symbolTable);
   template<class SYM, class DAT>
   Symbol setTypeForObject(Expression_class* expression, SymbolTable<SYM,DAT> *symbolTable);
   template<class SYM, class DAT>
   Symbol setTypeForPlus(Expression_class* expression, SymbolTable<SYM,DAT> *symbolTable);
   // void checkParentExist(ClassTable classtable);
   // bool isSymbolTypeValid(Symbol type);

#ifdef Program_SHARED_EXTRAS
   Program_SHARED_EXTRAS
#endif
#ifdef program_EXTRAS
   program_EXTRAS
#endif
};


// define constructor - class_
class class__class : public Class__class {
protected:
   Symbol name;
   Symbol parent;
   Features features;
   Symbol filename;
public:
   class__class(Symbol a1, Symbol a2, Features a3, Symbol a4) {
      name = a1;
      parent = a2;
      features = a3;
      filename = a4;
   }
   Class_ copy_Class_();
   void dump(ostream& stream, int n);
   Symbol GetName(){ return name;}
   Symbol GetParent(){ return parent;}
   Features GetFeatures(){ return features;}

#ifdef Class__SHARED_EXTRAS
   Class__SHARED_EXTRAS
#endif
#ifdef class__EXTRAS
   class__EXTRAS
#endif
};


// define constructor - method
class method_class : public Feature_class {
protected:
   Symbol name;
   Formals formals;
   Symbol return_type;
   Expression expr;
public:
   method_class(Symbol a1, Formals a2, Symbol a3, Expression a4) {
      name = a1;
      formals = a2;
      return_type = a3;
      expr = a4;
      method = true;
   }
   Feature copy_Feature();
   Symbol getReturnType() { return return_type;}
   Formals getFormals() {return formals;}
   void dump(ostream& stream, int n);
   Symbol GetName(){ return name;}
   Expression getExpression(){ return expr;}

#ifdef Feature_SHARED_EXTRAS
   Feature_SHARED_EXTRAS
#endif
#ifdef method_EXTRAS
   method_EXTRAS
#endif
};


// define constructor - attr
class attr_class : public Feature_class {
protected:
   Symbol name;
   Symbol type_decl;
   Expression init;
public:
   attr_class(Symbol a1, Symbol a2, Expression a3) {
      name = a1;
      type_decl = a2;
      init = a3;
      method = false;
   }
   Feature copy_Feature();
   void dump(ostream& stream, int n);
   Expression getExpression() { return init;}
   Symbol GetName() { return name; }
   // virtual Symbol getReturnType() = 0 ;

#ifdef Feature_SHARED_EXTRAS
   Feature_SHARED_EXTRAS
#endif
#ifdef attr_EXTRAS
   attr_EXTRAS
#endif
};


// define constructor - formal
class formal_class : public Formal_class {
protected:
   Symbol name;
   Symbol type_decl;
public:
   formal_class(Symbol a1, Symbol a2) {
      name = a1;
      type_decl = a2;
   }
   Formal copy_Formal();
   void dump(ostream& stream, int n);
   Symbol GetName() { return name;}
   Symbol GetTypeDecl(){ return type_decl;}

#ifdef Formal_SHARED_EXTRAS
   Formal_SHARED_EXTRAS
#endif
#ifdef formal_EXTRAS
   formal_EXTRAS
#endif
};


// define constructor - branch
class branch_class : public Case_class {
protected:
   Symbol name;
   Symbol type_decl;
   Expression expr;
public:
   branch_class(Symbol a1, Symbol a2, Expression a3) {
      name = a1;
      type_decl = a2;
      expr = a3;
   }
   Case copy_Case();
   void dump(ostream& stream, int n);

#ifdef Case_SHARED_EXTRAS
   Case_SHARED_EXTRAS
#endif
#ifdef branch_EXTRAS
   branch_EXTRAS
#endif
};


// define constructor - assign
class assign_class : public Expression_class {
protected:
   Symbol name;
   Expression expr;
   // ErrorMsg, type, else etc
public:
   assign_class(Symbol a1, Expression a2) {
      name = a1;
      expr = a2;
      types = ASSIGN_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);
   Symbol getLHS(){ return name;}
   Expression getExpression(){ return expr;}

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef assign_EXTRAS
   assign_EXTRAS
#endif
};


// define constructor - static_dispatch
class static_dispatch_class : public Expression_class {
protected:
   Expression expr;
   Symbol type_name;
   Symbol name;
   Expressions actual;
public:
   static_dispatch_class(Expression a1, Symbol a2, Symbol a3, Expressions a4) {
      expr = a1;
      type_name = a2;
      name = a3;
      actual = a4;
      types = STATIC_DISPATCH_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef static_dispatch_EXTRAS
   static_dispatch_EXTRAS
#endif
};


// define constructor - dispatch
class dispatch_class : public Expression_class {
protected:
   Expression expr;
   Symbol name;
   Expressions actual;
public:
   dispatch_class(Expression a1, Symbol a2, Expressions a3) {
      expr = a1;
      name = a2;
      actual = a3;
      types = DISPATCH_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef dispatch_EXTRAS
   dispatch_EXTRAS
#endif
};


// define constructor - cond
class cond_class : public Expression_class {
protected:
   Expression pred;
   Expression then_exp;
   Expression else_exp;

public:
   cond_class(Expression a1, Expression a2, Expression a3) {
      pred = a1;
      then_exp = a2;
      else_exp = a3;
      types = COND_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef cond_EXTRAS
   cond_EXTRAS
#endif
};


// define constructor - loop
class loop_class : public Expression_class {
protected:
   Expression pred;
   Expression body;

public:
   loop_class(Expression a1, Expression a2) {
      pred = a1;
      body = a2;
      types = LOOP_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef loop_EXTRAS
   loop_EXTRAS
#endif
};


// define constructor - typcase
class typcase_class : public Expression_class {
protected:
   Expression expr;
   Cases cases;

public:
   typcase_class(Expression a1, Cases a2) {
      expr = a1;
      cases = a2;
      types = TYPCASE_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef typcase_EXTRAS
   typcase_EXTRAS
#endif
};


// define constructor - block
class block_class : public Expression_class {
protected:
   Expressions body;
public:
   block_class(Expressions a1) {
      body = a1;
      types = BLOCK_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);
   Expressions getExpressions(){ return body;}

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef block_EXTRAS
   block_EXTRAS
#endif
};


// define constructor - let
class let_class : public Expression_class {
protected:
   Symbol identifier;
   Symbol type_decl;
   Expression init;
   Expression body;

public:
   let_class(Symbol a1, Symbol a2, Expression a3, Expression a4) {
      identifier = a1;
      type_decl = a2;
      init = a3;
      body = a4;
      types = LET_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef let_EXTRAS
   let_EXTRAS
#endif
};


// define constructor - plus
class plus_class : public Expression_class {
protected:
   Expression e1;
   Expression e2;

public:
   plus_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
      types = PLUS_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);
   Expression getE1(){
      return e1;
   }
   Expression getE2(){
     return e2;
   }

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef plus_EXTRAS
   plus_EXTRAS
#endif
};


// define constructor - sub
class sub_class : public Expression_class {
protected:
   Expression e1;
   Expression e2;

public:
   sub_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
      types = SUB_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef sub_EXTRAS
   sub_EXTRAS
#endif
};


// define constructor - mul
class mul_class : public Expression_class {
protected:
   Expression e1;
   Expression e2;

public:
   mul_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
      types = MUL_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef mul_EXTRAS
   mul_EXTRAS
#endif
};


// define constructor - divide
class divide_class : public Expression_class {
protected:
   Expression e1;
   Expression e2;

public:
   divide_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
      types = DIVIDE_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef divide_EXTRAS
   divide_EXTRAS
#endif
};


// define constructor - neg
class neg_class : public Expression_class {
protected:
   Expression e1;

public:
   neg_class(Expression a1) {
      e1 = a1;
      types = NEG_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef neg_EXTRAS
   neg_EXTRAS
#endif
};


// define constructor - lt
class lt_class : public Expression_class {
protected:
   Expression e1;
   Expression e2;
public:
   lt_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
      types = LT_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef lt_EXTRAS
   lt_EXTRAS
#endif
};


// define constructor - eq
class eq_class : public Expression_class {
protected:
   Expression e1;
   Expression e2;
public:
   eq_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
      types = EQ_CLASS_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef eq_EXTRAS
   eq_EXTRAS
#endif
};


// define constructor - leq
class leq_class : public Expression_class {
protected:
   Expression e1;
   Expression e2;
public:
   leq_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
      types = LEQ_CLASS_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef leq_EXTRAS
   leq_EXTRAS
#endif
};


// define constructor - comp
class comp_class : public Expression_class {
protected:
   Expression e1;
public:
   comp_class(Expression a1) {
      e1 = a1;
      types = COMP_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef comp_EXTRAS
   comp_EXTRAS
#endif
};


// define constructor - int_const
class int_const_class : public Expression_class {
protected:
   Symbol token;
public:
   int_const_class(Symbol a1) {
      token = a1;
      types = INT_CONST_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef int_const_EXTRAS
   int_const_EXTRAS
#endif
};


// define constructor - bool_const
class bool_const_class : public Expression_class {
protected:
   Boolean val;
public:
   bool_const_class(Boolean a1) {
      val = a1;
      types = BOOL_CONST_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef bool_const_EXTRAS
   bool_const_EXTRAS
#endif
};


// define constructor - std::string_const
class string_const_class : public Expression_class {
protected:
   Symbol token;
public:
   string_const_class(Symbol a1) {
      token = a1;
      types = STRING_CONST_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef string_const_EXTRAS
   string_const_EXTRAS
#endif
};


// define constructor - new_
class new__class : public Expression_class {
protected:
   Symbol type_name;
public:
   new__class(Symbol a1) {
      type_name = a1;
      types =  NEW_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef new__EXTRAS
   new__EXTRAS
#endif
};


// define constructor - isvoid
class isvoid_class : public Expression_class {
protected:
   Expression e1;
public:
   isvoid_class(Expression a1) {
      e1 = a1;
      types = ISVOID_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef isvoid_EXTRAS
   isvoid_EXTRAS
#endif
};


// define constructor - no_expr
class no_expr_class : public Expression_class {
protected:
public:
   no_expr_class() {
     types = NO_EXPR_TYPE;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef no_expr_EXTRAS
   no_expr_EXTRAS
#endif
};


// define constructor - object
class object_class : public Expression_class {
protected:
   Symbol name;
public:
   object_class(Symbol a1) {
      name = a1;
      types = OBJECT_TYPE;
   }
   Symbol getObjectName(){ return name;}
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef object_EXTRAS
   object_EXTRAS
#endif
};


// define the prototypes of the interface
Classes nil_Classes();
Classes single_Classes(Class_);
Classes append_Classes(Classes, Classes);
Features nil_Features();
Features single_Features(Feature);
Features append_Features(Features, Features);
Formals nil_Formals();
Formals single_Formals(Formal);
Formals append_Formals(Formals, Formals);
Expressions nil_Expressions();
Expressions single_Expressions(Expression);
Expressions append_Expressions(Expressions, Expressions);
Cases nil_Cases();
Cases single_Cases(Case);
Cases append_Cases(Cases, Cases);
Program program(Classes);
Class_ class_(Symbol, Symbol, Features, Symbol);
Feature method(Symbol, Formals, Symbol, Expression);
Feature attr(Symbol, Symbol, Expression);
Formal formal(Symbol, Symbol);
Case branch(Symbol, Symbol, Expression);
Expression assign(Symbol, Expression);
Expression static_dispatch(Expression, Symbol, Symbol, Expressions);
Expression dispatch(Expression, Symbol, Expressions);
Expression cond(Expression, Expression, Expression);
Expression loop(Expression, Expression);
Expression typcase(Expression, Cases);
Expression block(Expressions);
Expression let(Symbol, Symbol, Expression, Expression);
Expression plus(Expression, Expression);
Expression sub(Expression, Expression);
Expression mul(Expression, Expression);
Expression divide(Expression, Expression);
Expression neg(Expression);
Expression lt(Expression, Expression);
Expression eq(Expression, Expression);
Expression leq(Expression, Expression);
Expression comp(Expression);
Expression int_const(Symbol);
Expression bool_const(Boolean);
Expression string_const(Symbol);
Expression new_(Symbol);
Expression isvoid(Expression);
Expression no_expr();
Expression object(Symbol);


#endif
