

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <iostream>
#include <iterator>
#include <map>
#include <symtab.h>
#include "semant.h"
#include "utilities.h"

extern int semant_debug;
extern char *curr_filename;

// #define COND 9
// #define LOOP 10
// #define TYPCASE 11
// #define BLOCK 12
// #define LET 13
// #define PLUS 14
// #define SUB 15
// #define MUL 16
// #define DIVIDE 17
// #define ASSIGN 18
// #define STATIC_DISPATCH 19
// #define DISPATCH 20
// typedef class_ class_

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static std::map<Symbol, Class_> tree_classes;
static std::map<Symbol, Class_> parent_classes;

static Symbol
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//


static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    /* Fill this in */
    install_basic_classes();

}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object,
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    tree_classes[Object] = Object_class;

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class =
	class_(IO,
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);
    tree_classes[IO] = IO_class;
    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
	class_(Int,
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);
         tree_classes[Int] = Int_class;
    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
  tree_classes[Bool] = Bool_class;

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class =
	class_(Str,
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat,
								      single_Formals(formal(arg, Str)),
								      Str,
								      no_expr()))),
			       single_Features(method(substr,
						      append_Formals(single_Formals(formal(arg, Int)),
								     single_Formals(formal(arg2, Int))),
						      Str,
						      no_expr()))),
	       filename);
         tree_classes[Str] = Str_class;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{
    return semant_error(c->get_filename(),c);
}

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
}



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */


bool program_class::checkParentExist() {
  for( int i = classes->first(); classes->more(i); i = classes->next(i) ){
    if(semant_debug){
      cout << "The parent of the class is " <<endl;
      cout << classes->nth(i)->GetParent() <<endl;
    }

    if( tree_classes.find( classes->nth(i)->GetParent() )  == tree_classes.end() ){
      if(semant_debug){
        cout << "Parent of this class does not exist" <<  endl;
      }
      return false;
    }
   }
   return true;
}


 bool program_class::checkUnique() {
    // map<Symbol, Class_> tree_classes;

   for( int i = classes->first(); classes->more(i); i = classes->next(i) ){

     if( tree_classes.find( classes->nth(i)->GetName() )  == tree_classes.end() ){
       tree_classes[ classes->nth(i)->GetName() ] = classes->nth(i);
     }else{
       // mark classtable as the code contains duplicate class
       if (semant_debug)
        cout << "Oops there is duplicate class over here" <<endl;
       return false;
       // class is already present in the declaration.
       // Raise an error
          // classtable->semant_error()
        }
    }
    return true;
}

// static std::map<Symbol, Class_> parent_classes;
//

Class_ program_class::getParent(Class_ child) {
  if(semant_debug)
    cout << "In the getParent class for" << child->GetName() << endl;
  if (child->GetName() == Object)
    return child;
  if(semant_debug)
    cout << "The value of the GetParent is " << child->GetParent() <<endl;
  if (child->GetParent() == child->GetName()) {
    return child;
  }
  if ( child->GetParent() == Object )
    return tree_classes[Object];

  Class_ parent = tree_classes[child->GetParent()];
     // ( parent_classes.find(parent->GetName()) != tree_classes[Object] )

  while( parent_classes.find( parent->GetName()) != parent_classes.end()  ){
    // if ( parent_classes.find(parent->GetName()) == tree_classes[Object] ){
    // }
    // rtur
    // cout << "Inside the while loop" <<endl;
    parent = parent_classes[parent->GetName()];
    if ( parent ==  tree_classes[Object])
      break;
  }
  return parent;
}



bool program_class::isCycle() {
  for( int i = classes->first(); classes->more(i); i = classes->next(i) ){

    if (semant_debug)
      cout << "looping over the class" << classes->nth(i)->GetName() << endl;
    // if (  classes->nth(i)->GetParent() == classes->nth(i)->GetName()){
    //   cout << "Fuck there is an error" <<endl;
    //   return true;
    // }
    if (semant_debug)
      cout <<  getParent(classes->nth(i))->GetName() <<endl;
    if ( getParent(classes->nth(i))->GetName() == classes->nth(i)->GetName() ){
      if(semant_debug)
        cout << "Fuck there is cycle in the classes" <<endl;
      return true;
    }
    else{
      parent_classes[classes->nth(i)->GetName()] = getParent(classes->nth(i));
    }
  }
  return false;
}

// return type of methods most be declared

// method will be some kind of Feature lets seee

// Check if the method return type is valid or not


bool program_class::methodReturnTypeValid(method_class* method){
  // cout << "Checking the return type of the method" <<endl;
  return tree_classes.find(method->getReturnType()) != tree_classes.end();
}

// Can there be multilpe body in the body, lets see
// Symbol will be the return type of the object and I have to make sure that certain values
// Symbol program_class::setTypeInLet(){
//
// }
//

// write the code for returning the type of the expression
template<class SYM, class DAT>
Symbol program_class::setTypeForExpression(Expression_class* expression, SymbolTable<SYM,DAT> *symbolTable){
  symbolTable->enterscope();
  // std:: string type = expression->getType();
  cout << "Set type for expression" << endl;
  cout << "Printing the expresion " << expression->getType() << endl;
  Symbol returnType;
  switch( expression->getType()){
    case BLOCK_TYPE:
      returnType =  setTypeInBlock((block_class*)expression, symbolTable);
      break;
    case ASSIGN_TYPE:
      returnType = setTypeForAssign(expression, symbolTable);
      break;
    case OBJECT_TYPE:
      returnType = setTypeForObject(expression, symbolTable);
  }

  symbolTable->exitscope();
  return returnType;
}

template<class SYM, class DAT>
Symbol program_class::setTypeForObject(Expression_class*  expression, SymbolTable<SYM,DAT> *symbolTable){
  cout << "In the Object class " <<endl;
  object_class* assign = (object_class *)expression;
  Symbol name = assign->getObjectName();
  Symbol returnValue;
  if (symbolTable->lookup(name) == NULL){
    cout << "Symbol does not exist "<< name <<endl;
  } else {
    returnValue = name;
  }
  return returnValue;
}

template<class SYM, class DAT>
Symbol program_class::setTypeForAssign(Expression_class* expression, SymbolTable<SYM,DAT> *symbolTable){
  // symbol table needs to know about the value and it needs to be updated into it
  cout << "In the assign class " <<endl;
  assign_class* assign = (assign_class *)expression;
  Symbol lhs = assign->getLHS();
  Symbol returnValue;
  // if (symbolTable->lookup(lhs) == NULL){
    // cout << "Symbol does not exist "<< lhs <<endl;
  // } else {
    returnValue = setTypeForExpression(assign->getExpression(),symbolTable);
  // }
  return returnValue;
}

// Symbol program_class::setTypeInCase(){
//
// }

// This depends upon the
// Symbol program_class::setTypeForPlus(plus_class *plus, SymbolTable *symbolTable){
//   symbolTable->enterscope();
//   Symbol e1 = setTypeForExpression(plus->getE1(),symbolTable);
//   Symbol e2 = setTypeForExpression(plus->getE2(),symbolTable);
//   if ( e1 != Int ){
//     cout << "raise an exception about the E1";
//   }
//   if ( e2 != Int ){
//     cout << "raise an exception about the E2";
//   }
//   symbolTable->exitscope();
//   return Int;
// }

// Symbol program_class::setTypeForOperators(Expression_class* expression, SymbolTable *symbolTable){
//   switch(expression->getType()){
//     case "plus":
//       return setTypeForPlus((plus_class *) expression, symbolTable);
//     case "minus":
//       return setTypeForMinus((minus_class*) expression, symbolTable);
//   }
// }

// This method wil be called with the info about the
template <class SYM, class DAT>
Symbol program_class::setTypeInBlock(block_class* body, SymbolTable<SYM,DAT> *symbolTable){
  symbolTable->enterscope();
  Symbol returnValue;
  Expressions expressions = body->getExpressions();
  cout << "Block Class "<<endl;
  for( int i = expressions->first(); expressions->more(i) ; i = expressions->next(i) ){
    Expression_class *expression = (Expression_class*) expressions->nth(i);
    // cout << "Printing the expresion " << expression->getType() << endl;
    returnValue = setTypeForExpression(expression, symbolTable);
  }
  symbolTable->exitscope();
  return returnValue;
}

template <class SYM, class DAT>
Symbol program_class::setTypeOfMethod(method_class* method,SymbolTable<SYM,DAT> *symbolTable){
// SymbolTable<char *,int> *map = new SymbolTable<char *, int>();
  cout << "In the set Type Of Method" << endl;
  //
  symbolTable->enterscope();

  Formals formals = method->getFormals();
  for ( int i = formals->first(); formals->more(i); i = formals->next(i) ){
    formal_class* formal = (formal_class*)formals->nth(i);
    cout << "Looping over the the Formals   " << formal->GetName() << endl;
    if( symbolTable->probe(formal->GetName()) != NULL ){
      cout << "There is an error, the value of the formal parameter is already defined" <<endl;
    }
    Symbol type = formal->GetTypeDecl();
    if ( checkTypeValid(type) )
      symbolTable->addid(formal->GetName(), &type);
    else{
      cout << "There is an error in the method argument type " <<endl;
    }
  }

// At this point symbol table will have the full info about the ar
// set the method type in the expression of the

  Expression expression = method->getExpression();

  setTypeForExpression(expression, symbolTable);

  symbolTable->exitscope();
  return NULL;
}

bool program_class::isMethodArgValid(method_class* method){
  Formals formals = method->getFormals();
  for(int i = formals->first(); formals->more(i); i = formals->next(i)){
    formal_class* formal = (formal_class*)formals->nth(i);
    if ( tree_classes.find(formal->GetTypeDecl()) == tree_classes.end() )
      return false;
  }
  return true;
}

bool program_class::methodArgUnique(method_class* method){
  std::map<Symbol,bool> argVarMap;
  Formals formals = method->getFormals();
  for(int i = formals->first(); formals->more(i); i = formals->next(i)){
    formal_class* formal = (formal_class*)formals->nth(i);
    if ( argVarMap.find( formal->GetName())  != argVarMap.end() )
      return false;
    argVarMap[formal->GetName()] = 1;
  }
  return true;
}


// to check that the method signature is same as that of the parent
bool program_class::checkMethodSignatureValid(Feature method){
  return false;
}

// To check whatever inside the method is there it is coorect and working
bool program_class::checkMethodExprValid(Feature method){
  return false;
}


bool program_class::checkMethod(method_class* method){
  // cout << methodReturnTypeValid(method) <<endl;
  // cout << methodArgUnique(method) <<endl;
  // checkMethodSignatureValid(method);
  // checkMethodExprValid(method);
  // template<class SYM, class DAT>
  SymbolTable<Symbol,Symbol>* symbolTable = new SymbolTable<Symbol,Symbol>();
  setTypeOfMethod(method, symbolTable);
  cout << isMethodArgValid(method) <<endl;
  return true;
}
//
bool program_class::checkTypeValid(Symbol type){
  return tree_classes.find(type) != tree_classes.end();
}

bool program_class::checkAttr(attr_class* attr){
  return false;
}

// void program_class::storeDeclarations(){
//
// }

bool program_class::checkFeature(){
  for( int i = classes->first(); classes->more(i); i = classes->next(i) ){

    // if (semant_debug)
      cout << "looping over the class" << classes->nth(i)->GetName() << endl;

    Features features = classes->nth(i)->GetFeatures();
    for( int i = features->first(); features->more(i); i = features->next(i) ){
      Feature feature = features->nth(i);
      cout << "looping over the feature" << feature->GetName() << endl;

      if ( feature->isMethod() ){
        checkMethod((method_class*)feature);
      }else{
        checkAttr((attr_class*)feature);
      }
    }
  }
  return true;
}

void program_class::semant()
{
  // program_class should have methods that can be used to traverser things
  //
    initialize_constants();


    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);
    /* some semantic analysis code may go here */

    // tree_classes
    //
    // checkUnique();
    if ( checkUnique() && checkParentExist() )
      isCycle();


    checkFeature();

    // for(int i = features->first(); features->more(i); i = features->next(i))
        // features->nth(i)->dump_with_types(cout, 2);

    // checkUnique(classtable);
    // checkUnique();

    if (classtable->errors()) {
	     cerr << "Compilation halted due to static semantic errors." << endl;
	     exit(1);
    }
}
