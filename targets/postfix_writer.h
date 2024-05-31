#ifndef __TIL_TARGETS_POSTFIX_WRITER_H__
#define __TIL_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"

#include <cdk/emitters/basic_postfix_emitter.h>
#include <set>
#include <sstream>

typedef int lbl;

namespace til {

//!
//! Traverse syntax tree and generate the corresponding assembly code.
//!
class postfix_writer : public basic_ast_visitor {
  cdk::symbol_table<til::symbol> &_symtab;

  std::set<std::string> _functionsToDeclare;
  std::set<std::string> _symbolsToDeclare;

  // code generation
  cdk::basic_postfix_emitter &_pf;
  int _lbl;

  // semantic analysis
  bool _insideFunction = false;
  bool _inFunctionArgs = false;
  bool _mainReturnSeen = false;
  bool _lastBlockInstructionSeen = false;
  // while labels -- for break/continue; work like stacks
  std::vector<lbl> _whileCond, _whileEnd;

  // function labels need to be known to return their segments
  std::vector<std::string> _functionLabels;
  // a given forwarded function's label, as we want to call it, not branch to it
  std::string _currentForwardLabel;
  // for keeping track of functions and their arguments
  // works like a stack
  std::vector<std::shared_ptr<til::symbol>> _functions;

   // current frame pointer offset -- 0 means no vars defined
  int _offset = 0; 

public:
  postfix_writer(std::shared_ptr<cdk::compiler> compiler,
                 cdk::symbol_table<til::symbol> &symtab,
                 cdk::basic_postfix_emitter &pf)
      : basic_ast_visitor(compiler), _symtab(symtab), _pf(pf), _lbl(0) {}

public:
  ~postfix_writer() { os().flush(); }

private:
  /** Method used to generate sequential labels. */
  inline std::string mklbl(int lbl) {
    std::ostringstream oss;
    if (lbl < 0)
      oss << ".L" << -lbl;
    else
      oss << "_L" << lbl;
    return oss.str();
  }

  /** Method use to print error messages. */
  void error(int lineno, std::string e) {
    std::cerr << lineno << ": " << e << std::endl;
  }

protected:

  /**
   * @brief Processes an IDP (Integer Double Pointer) binary expression node.
   *
   * This function processes a binary operation node by first evaluating the left 
   * operand and then the right operand. If the binary operation is typed as a 
   * double, it converts the operand to a double if necessary. Similarly, if the 
   * operation is typed as a pointer, it performs the appropriate pointer arithmetic.
   *
   * @param node Pointer to the binary operation node to process.
   * @param lvl  The current tree level (used for indentation and formatting).
   */
  void handleIDPBinaryExpression(cdk::binary_operation_node *const node,
                                  int lvl);

  /**
   * @brief Processes a binary expression node with potential integer-to-double conversions.
   *
   * This function processes a binary operation node by first evaluating the left operand 
   * and then the right operand. If the binary operation is typed as a double and either 
   * operand is not already a double, it converts the operand to a double. This ensures 
   * type consistency for further operations.
   *
   * @param node Pointer to the binary operation node to process.
   * @param lvl The current tree level (used for indentation and formatting).
   */
  void handleIDBinaryExpression(cdk::binary_operation_node *const node,
                                 int lvl);
  
    /**
   * @brief Processes a general logical binary expression node.
   *
   * This function processes a binary operation node by first evaluating the left 
   * operand and then the right operand. It ensures that the operands are correctly 
   * typed, performing type conversions if necessary. Specifically, it handles 
   * conversions to double type when one operand is of type double and the other is not. 
   * After processing the operands, it compares them appropriately.
   *
   * @param node Pointer to the binary operation node to process.
   * @param lvl  The current tree level (used for indentation and formatting).
   */
  void
  handleGeneralLogicalBinaryExpression(cdk::binary_operation_node *const node,
                                        int lvl);

  /**
   * @brief Processes the initialization of a local variable.
   *
   * This function initializes a local variable based on its type. It handles different
   * types such as integers, strings, pointers, functional types, and unspecified types.
   * Depending on the type, it stores the variable at a specified offset in the local
   * storage. For double types, it may convert an integer initializer to a double before
   * storing it.
   *
   * @param symbol A shared pointer to the symbol representing the local variable.
   * @param initializer Pointer to the expression node that initializes the variable.
   * @param lvl The current tree level (used for indentation and formatting).
   */
  void
  handleLocalVariableInitialization(std::shared_ptr<til::symbol> symbol,
                                     cdk::expression_node *const initializer,
                                     int lvl);
  /**
   * @brief Processes the initialization of a global variable.
   *
   * This function initializes a global variable based on its type. It handles different
   * types such as integers, strings, pointers, doubles, and functional types. Depending 
   * on the type, it places the variable in the data segment, aligns it, and labels it. 
   * For double types, it may convert an integer initializer to a double. For functional 
   * types, it registers the function symbol and sets up its label.
   *
   * @param symbol A shared pointer to the symbol representing the global variable.
   * @param initializer Pointer to the expression node that initializes the variable.
   * @param lvl The current tree level (used for indentation and formatting).
   */
  void
  handleGlobalVariableInitialization(std::shared_ptr<til::symbol> symbol,
                                      cdk::expression_node *const initializer,
                                      int lvl);

public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h" // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end
};

} // namespace til

#endif