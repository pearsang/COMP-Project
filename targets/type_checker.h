#ifndef __TIL_TARGETS_TYPE_CHECKER_H__
#define __TIL_TARGETS_TYPE_CHECKER_H__

#include "targets/basic_ast_visitor.h"
#include ".auto/all_nodes.h" // automatically generated

namespace til {

/**
 * Print nodes as XML elements to the output stream.
 */
class type_checker : public basic_ast_visitor {
  cdk::symbol_table<til::symbol> &_symtab;

  basic_ast_visitor *_parent;

public:
  type_checker(std::shared_ptr<cdk::compiler> compiler,
               cdk::symbol_table<til::symbol> &symtab,
               basic_ast_visitor *parent)
      : basic_ast_visitor(compiler), _symtab(symtab), _parent(parent) {}

public:
  ~type_checker() { os().flush(); }

protected:
 /**
 * @brief Checks if two pointer types are compatible.
 * 
 * This function compares the given types `t1` and `t2` to determine if they are compatible pointer types.
 * Compatibility requires that the referenced types of the pointers are compatible.
 * The function traverses through the pointer types to their referenced types and checks their compatibility.
 * 
 * @param t1 A shared pointer to the first type to check.
 * @param t2 A shared pointer to the second type to check.
 * 
 * @return true if the pointer types are compatible; false otherwise.
 */
  bool ptr_types_compatibility_check(std::shared_ptr<cdk::basic_type> t1,
                                   std::shared_ptr<cdk::basic_type> t2);
  
  /**
 * @brief Checks if two functional types are compatible.
 * 
 * This function compares the given functional types `t1` and `t2` to determine if they are compatible.
 * Compatibility requires that the return types are compatible, the number of input arguments is the same,
 * and the types of the input arguments are compatible.
 * 
 * @param t1 A shared pointer to the first functional type to check.
 * @param t2 A shared pointer to the second functional type to check.
 * 
 * @return true if the functional types are compatible; false otherwise.
 */
  bool func_types_compatibility_check(std::shared_ptr<cdk::functional_type> t1,
                                   std::shared_ptr<cdk::functional_type> t2);
  
  /**
 * @brief Checks if the two provided types are compatible.
 * 
 * This function compares the given types `t1` and `t2` to determine if they are compatible.
 * Compatibility depends on the specific type of `t1` and `t2`, and whether the check is for a return type, 
 * indicated by the `is_return` flag. The function handles various type categories including 
 * integers, doubles, strings, pointers, functionals, and unspecified types.
 * 
 * @param t1 A shared pointer to the first type to check.
 * @param t2 A shared pointer to the second type to check.
 * @param is_return A boolean flag indicating if the type check is for a return statement.
 * 
 * @return true if the types are compatible; false otherwise.
 */
  bool types_compatibility_check(std::shared_ptr<cdk::basic_type> t1,
                               std::shared_ptr<cdk::basic_type> t2,
                               bool is_return = false);
  /**
 * @brief Throws an exception if the given types are incompatible.
 * 
 * This function checks if the two provided types `t1` and `t2` are compatible.
 * If they are not compatible, it throws a descriptive exception based on the type of `t1`.
 * The error message varies depending on whether the incompatibility occurs during a return statement
 * or an initialization, as indicated by the `is_return` flag.
 * 
 * @param t1 A shared pointer to the first type to check.
 * @param t2 A shared pointer to the second type to check.
 * @param is_return A boolean flag indicating if the type check is for a return statement.
 *                  If true, the error message will reference a return type; otherwise, it will reference an initialization.
 * 
 * @throws std::string Describes the type incompatibility error.
 */
  void incompatible_types_throw(std::shared_ptr<cdk::basic_type> t1,
                             std::shared_ptr<cdk::basic_type> t2,
                             bool is_return = false);
  /**
 * @brief Changes the type of the lvalue and rvalue nodes based on their current types.
 * 
 * This function compares the types of the given `lvalue` and `rvalue` nodes.
 * If both are of type `TYPE_UNSPEC`, their types are changed to `TYPE_INT`.
 * Otherwise, it checks for compatibility between pointer types, functional types,
 * or if the `rvalue` is of type `TYPE_UNSPEC` while `lvalue` is of type `TYPE_INT` or `TYPE_DOUBLE`.
 * If compatible, the type of `rvalue` is changed to match the type of `lvalue`.
 * 
 * @param lvalue Pointer to the left value node.
 * @param rvalue Pointer to the right value node.
 */
  void on_match_change_type(cdk::typed_node *const lvalue, cdk::typed_node *const rvalue);

  /**
 * @brief Processes a binary expression node to determine its type based on its operands.
 * 
 * This function evaluates the types of the left and right operands of the given binary operation node.
 * It sets the type of the binary operation node based on the types of its operands, handling cases where 
 * the operands are of type `TYPE_INT`, `TYPE_DOUBLE`, or `TYPE_UNSPEC`.
 * 
 * @param node A pointer to the binary operation node to process.
 * @param lvl The current processing level (used for controlling the depth of type checking).
 * 
 * @return true if the binary expression is processed successfully and the types are compatible; false otherwise.
 */
  bool handleBinaryExpression(cdk::binary_operation_node *const node, int lvl);

  /**
 * @brief Processes an integer binary expression node, ensuring both operands are integers.
 * 
 * This function checks if the left and right operands of the given binary operation node are of type `TYPE_INT`.
 * If they are not, it throws an exception. If both operands are integers, it sets the type of the binary operation node to `TYPE_INT`.
 * 
 * @param node A pointer to the binary operation node to process.
 * @param lvl The current processing level (used for controlling the depth of type checking).
 * 
 * @throws std::string If the type of either operand is not `TYPE_INT`.
 */
  void handleIBinaryExpression(cdk::binary_operation_node *const node,
                                int lvl);


  /**
 * @brief Processes a binary expression node that can involve either integer or double types.
 * 
 * This function delegates the processing of a binary expression to `handleBinaryExpression`.
 * If `handleBinaryExpression` returns false, indicating incompatible types, it throws an exception.
 * 
 * @param node A pointer to the binary operation node to process.
 * @param lvl The current processing level (used for controlling the depth of type checking).
 * 
 * @throws std::string If the types in the binary expression are incompatible.
 */                              
  void handleIDBinaryExpression(cdk::binary_operation_node *const node,
                                 int lvl);

  /**
 * @brief Processes an additive binary expression node, which can handle addition and subtraction operations.
 * 
 * This function first attempts to process the binary expression using `handleBinaryExpression`.
 * If that fails, it handles specific cases involving pointer and integer types.
 * If `isSub` is true, it also checks for pointer subtraction compatibility.
 * 
 * @param node A pointer to the binary operation node to process.
 * @param lvl The current processing level (used for controlling the depth of type checking).
 * @param isSub A boolean flag indicating if the expression is a subtraction.
 * 
 * @throws std::string If the types in the binary expression are incompatible.
 */
  void handleAdditiveBinaryExpression(cdk::binary_operation_node *const node,
                                  int lvl, bool isSub);
  
  /**
 * @brief Processes a comparison binary expression node, ensuring compatible types and setting the result type to `TYPE_INT`.
 * 
 * This function first processes the binary expression using `handleIDBinaryExpression`.
 * Then, it sets the type of the binary operation node to `TYPE_INT`, indicating a comparison result.
 * 
 * @param node A pointer to the binary operation node to process.
 * @param lvl The current processing level (used for controlling the depth of type checking).
 */
  void
  handleComparisonBinaryExpression(cdk::binary_operation_node *const node,
                                       int lvl);
  
  /**
 * @brief Processes a logical binary expression node, ensuring compatible types and setting the result type to `TYPE_INT`.
 * 
 * This function first processes the binary expression using `handleIBinaryExpression`.
 * Then, it sets the type of the binary operation node to `TYPE_INT`, indicating a logical result.
 * 
 * @param node A pointer to the binary operation node to process.
 * @param lvl The current processing level (used for controlling the depth of type checking).
 */
  void
  handleLogicalBinaryExpression(cdk::binary_operation_node *const node,
                                        int lvl);
  
  /**
 * @brief Processes an equality binary expression node, ensuring compatible types and setting the result type to `TYPE_INT`.
 * 
 * This function first processes the types of the left and right operands using `handleBinaryExpression`.
 * It then checks if the types are compatible or if they are compatible pointer types.
 * If the types are not compatible, it throws an exception.
 * Finally, it sets the type of the binary operation node to `TYPE_INT`, indicating a result of the comparison.
 * 
 * @param node A pointer to the binary operation node to process.
 * @param lvl The current processing level (used for controlling the depth of type checking).
 * 
 * @throws std::string If the types on both sides of the equality operator are not compatible.
 */
  void
  handleEqualityBinaryExpression(cdk::binary_operation_node *const node,
                                        int lvl);

  template <typename T>
  void process_literal(cdk::literal_node<T> *const node, int lvl) {}

public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h" // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end
};

} // namespace til

//---------------------------------------------------------------------------
//     HELPER MACRO FOR TYPE CHECKING
//---------------------------------------------------------------------------

/**
 * @brief Macro for checking types using the type checker.
 * 
 * This macro creates a `til::type_checker` object with the given compiler and symbol table,
 * and then accepts the given node for type checking.
 * If an exception of type `std::string` is caught during type checking, it prints the problem message
 * along with the line number of the node and returns.
 * 
 * @param compiler The compiler object.
 * @param symtab The symbol table object.
 * @param node A pointer to the node to check types for.
 */
#define CHECK_TYPES(compiler, symtab, node)                                    \
  {                                                                            \
    try {                                                                      \
      til::type_checker checker(compiler, symtab, this);                       \
      (node)->accept(&checker, 0);                                             \
    } catch (const std::string &problem) {                                     \
      std::cerr << (node)->lineno() << ": " << problem << std::endl;           \
      return;                                                                  \
    }                                                                          \
  }

/**
 * @brief Macro for asserting safe expressions by checking types.
 * 
 * This macro is defined as an alias to the CHECK_TYPES macro, allowing for a more descriptive usage.
 * It invokes CHECK_TYPES with the provided compiler, symbol table, and node.
 * 
 * @param _compiler The compiler object.
 * @param _symtab The symbol table object.
 * @param node A pointer to the node to check types for.
 */
#define ASSERT_SAFE_EXPRESSIONS CHECK_TYPES(_compiler, _symtab, node)

#endif