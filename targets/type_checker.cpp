#include "targets/type_checker.h"
#include ".auto/all_nodes.h" // automatically generated
#include <cdk/types/primitive_type.h>
#include <string>

#include <til_parser.tab.h>

#define ASSERT_UNSPEC                                                          \
  {                                                                            \
    if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC))          \
      return;                                                                  \
  }

bool til::type_checker::ptr_types_compatibility_check(
    std::shared_ptr<cdk::basic_type> t1, std::shared_ptr<cdk::basic_type> t2) {
  auto t1_ptr = t1;
  auto t2_ptr = t2;
  // while both are pointers, keep checking the referenced types
  while (t1_ptr->name() == cdk::TYPE_POINTER &&
         t2_ptr->name() == cdk::TYPE_POINTER) {
    t1_ptr = cdk::reference_type::cast(t1_ptr)->referenced();
    t2_ptr = cdk::reference_type::cast(t2_ptr)->referenced();
  }
  return t1_ptr->name() == t2_ptr->name() || t2_ptr->name() == cdk::TYPE_UNSPEC;
}

bool til::type_checker::func_types_compatibility_check(
    std::shared_ptr<cdk::functional_type> t1,
    std::shared_ptr<cdk::functional_type> t2) {
  // the return type must be compatible
  if ((t1->output_length() > 0 && t2->output_length() > 0) &&
      !types_compatibility_check(t1->output(0), t2->output(0)))
    return false;

  // the number of arguments must be the same
  if (t1->input_length() != t2->input_length())
    return false;

  // the types of the arguments must be compatible
  for (size_t i = 0; i < t1->input_length(); i++)
    if (!types_compatibility_check(t1->input(i), t2->input(i)))
      return false;
  return true;
}

bool til::type_checker::types_compatibility_check(
    std::shared_ptr<cdk::basic_type> t1, std::shared_ptr<cdk::basic_type> t2,
    bool is_return) {
  const auto t1_name = t1->name();
  const auto t2_name = t2->name();
  switch (t1_name) {
  case cdk::TYPE_INT:
  case cdk::TYPE_DOUBLE:
    if (!(t2_name == cdk::TYPE_DOUBLE || t2_name == cdk::TYPE_INT))
      return false;
    break;
  case cdk::TYPE_STRING:
    if (t2_name != cdk::TYPE_STRING)
      return false;
    break;
  case cdk::TYPE_POINTER:
    if (is_return == (t2_name == cdk::TYPE_POINTER) &&
        !ptr_types_compatibility_check(t1, t2))
      return false;
    break;
  case cdk::TYPE_FUNCTIONAL:
    if (!((t2_name == cdk::TYPE_FUNCTIONAL &&
           func_types_compatibility_check(cdk::functional_type::cast(t1),
                                      cdk::functional_type::cast(t2))) ||
          (t2_name == cdk::TYPE_POINTER &&
           cdk::reference_type::cast(t2)->referenced() == nullptr)))
      return false;
    break;
  case cdk::TYPE_UNSPEC: // useful for auto cases
    if (t2_name == cdk::TYPE_VOID)
      // var x = f(), where f calls return void, is not allowed
      return false;
    break;
  default:
    if (t1_name != t2_name)
      return false;
  }
  return true;
}

void til::type_checker::incompatible_types_throw(
    std::shared_ptr<cdk::basic_type> t1, std::shared_ptr<cdk::basic_type> t2,
    bool is_return) {
  if (types_compatibility_check(t1, t2))
    return;

  const std::string field_name =
      is_return ? "return" : "initialization";
  switch (t1->name()) {
  case cdk::TYPE_INT:
  case cdk::TYPE_DOUBLE:
    throw std::string("wrong type in " + field_name +
                      " (expected double or int)");
  case cdk::TYPE_STRING:
    throw std::string("wrong type in " + field_name + " (string was expected)");
  case cdk::TYPE_POINTER:
    throw std::string("wrong type in " + field_name + " (pointer was expected)");
  case cdk::TYPE_FUNCTIONAL:
    throw std::string("wrong type in " + field_name + " (function was expected)");
  default:
    throw std::string("unknown type in " + field_name);
  }
}

void til::type_checker::on_match_change_type(cdk::typed_node *const lvalue,
                                             cdk::typed_node *const rvalue) {
  const auto ltype = lvalue->type();
  const auto rtype = rvalue->type();
  if (ltype->name() == cdk::TYPE_UNSPEC && rtype->name() == cdk::TYPE_UNSPEC) {
    // var x = input;
    lvalue->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    rvalue->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if ((ltype->name() == cdk::TYPE_POINTER &&
              rtype->name() == cdk::TYPE_POINTER &&
              ptr_types_compatibility_check(ltype, rtype)) ||
             (ltype->name() == cdk::TYPE_FUNCTIONAL &&
              rtype->name() == cdk::TYPE_FUNCTIONAL &&
              func_types_compatibility_check(cdk::functional_type::cast(ltype),
                                         cdk::functional_type::cast(rtype))) ||
             ((ltype->name() == cdk::TYPE_INT ||
               ltype->name() == cdk::TYPE_DOUBLE) &&
              rtype->name() == cdk::TYPE_UNSPEC)) {
    rvalue->type(ltype);
  }
}

bool til::type_checker::handleBinaryExpression(
    cdk::binary_operation_node *const node, int lvl) {
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_INT) ||
      node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    if (node->right()->is_typed(cdk::TYPE_INT))
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    else if (node->right()->is_typed(cdk::TYPE_DOUBLE))
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else {
      return false;
    }
  } else if (node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    if (node->right()->is_typed(cdk::TYPE_DOUBLE) ||
        node->right()->is_typed(cdk::TYPE_INT))
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      node->right()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } else {
      return false;
    }
  } else {
    return false;
  }
  return true;
}

void til::type_checker::handleIBinaryExpression(
    cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in left argument of binary expression");

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in right argument of binary expression");
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::handleIDBinaryExpression(
    cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  if (!handleBinaryExpression(node, lvl))
    throw std::string("wrong types in binary expression");
}

void til::type_checker::handleAdditiveBinaryExpression(
    cdk::binary_operation_node *const node, int lvl, bool isSub) {
  ASSERT_UNSPEC;
  if (handleBinaryExpression(node, lvl))
    return;

  if (node->left()->is_typed(cdk::TYPE_POINTER) &&
      node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(node->left()->type());
  } else if (node->left()->is_typed(cdk::TYPE_INT) &&
             node->right()->is_typed(cdk::TYPE_POINTER)) {
    node->type(node->right()->type());
  } else {
    if (isSub) {
      if ((node->left()->is_typed(cdk::TYPE_POINTER) &&
           node->right()->is_typed(cdk::TYPE_POINTER)) &&
          (ptr_types_compatibility_check(node->left()->type(),
                                      node->right()->type()))) {
        node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        return;
      }
    }
    throw std::string("wrong types in binary expression");
  }
}

void til::type_checker::handleComparisonBinaryExpression(
    cdk::binary_operation_node *const node, int lvl) {
  handleIDBinaryExpression(node, lvl);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::handleLogicalBinaryExpression(
    cdk::binary_operation_node *const node, int lvl) {
  handleIBinaryExpression(node, lvl);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::handleEqualityBinaryExpression(
    cdk::binary_operation_node *const node, int lvl) {
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  if (!(handleBinaryExpression(node, lvl) ||
        ptr_types_compatibility_check(node->left()->type(),
                                   node->right()->type()))) {
    throw std::string("same type expected on both sides of equality operator");
  }
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------

void til::type_checker::do_sequence_node(cdk::sequence_node *const node,
                                         int lvl) {
  for (auto n : node->nodes())
    n->accept(this, lvl);
}

//--------------------------PURPOSEFULLY EMPTY-------------------------------

void til::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_stop_node(til::stop_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_next_node(til::next_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_block_node(til::block_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_program_node(til::program_node*, int) {
  // EMPTY
}
void til::type_checker::do_function_def_node(
    til::function_def_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void til::type_checker::do_integer_node(cdk::integer_node *const node,
                                        int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}

void til::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

//---------------------------------------------------------------------------

void til::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) {
    throw std::string("wrong type in argument of not expression");
  }
  node->type(node->argument()->type());
}

//---------------------------------------------------------------------------

void til::type_checker::do_unary_minus_node(cdk::unary_minus_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!(node->argument()->is_typed(cdk::TYPE_INT) ||
        node->argument()->is_typed(cdk::TYPE_DOUBLE))) {
    throw std::string("wrong type in argument of negation expression");
  }
  node->type(node->argument()->type());
}

void til::type_checker::do_unary_plus_node(cdk::unary_plus_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  const auto &type = node->argument()->type();
  if (!(type->name() == cdk::TYPE_INT || type->name() == cdk::TYPE_DOUBLE))
    throw std::string("wrong type in argument of identity expression");
  node->type(type);
}

//---------------------------------------------------------------------------

void til::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  handleAdditiveBinaryExpression(node, lvl, false);
}
void til::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  handleAdditiveBinaryExpression(node, lvl, true);
}
void til::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  handleIDBinaryExpression(node, lvl);
}
void til::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  handleIDBinaryExpression(node, lvl);
}
void til::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  handleIBinaryExpression(node, lvl);
}
void til::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  handleComparisonBinaryExpression(node, lvl);
}
void til::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  handleComparisonBinaryExpression(node, lvl);
}
void til::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  handleComparisonBinaryExpression(node, lvl);
}
void til::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  handleComparisonBinaryExpression(node, lvl);
}
void til::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  handleEqualityBinaryExpression(node, lvl);
}
void til::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  handleEqualityBinaryExpression(node, lvl);
}
void til::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  handleLogicalBinaryExpression(node, lvl);
}
void til::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  handleLogicalBinaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void til::type_checker::do_variable_node(cdk::variable_node *const node,
                                         int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<til::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr) {
    node->type(symbol->type());
  } else {
    throw id;
  }
}

void til::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

void til::type_checker::do_assignment_node(cdk::assignment_node *const node,
                                           int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 2);
  node->rvalue()->accept(this, lvl + 2);

  on_match_change_type(node->lvalue(), node->rvalue());
  const auto lval_type = node->lvalue()->type();
  const auto rval_type = node->rvalue()->type();
  incompatible_types_throw(lval_type, rval_type);
  node->type(lval_type);
}

//---------------------------------------------------------------------------

void til::type_checker::do_evaluation_node(til::evaluation_node *const node,
                                           int lvl) {
  node->argument()->accept(this, lvl + 2);
}

void til::type_checker::do_print_node(til::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);
  for (auto *node : node->arguments()->nodes()) {
    const auto &type = (dynamic_cast<cdk::expression_node *>(node))->type();
    if (!(type->name() == cdk::TYPE_INT || type->name() == cdk::TYPE_DOUBLE ||
          type->name() == cdk::TYPE_STRING)) {
      throw std::string("wrong type in argument of print expression");
    }
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_while_node(til::while_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

void til::type_checker::do_if_node(til::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  node->block()->accept(this, lvl + 4);
}

void til::type_checker::do_if_else_node(til::if_else_node *const node,
                                        int lvl) {
  node->condition()->accept(this, lvl + 4);
  node->thenblock()->accept(this, lvl + 4);
  node->elseblock()->accept(this, lvl + 4);
}

void til::type_checker::do_return_node(til::return_node *const node, int lvl) {
  const auto function = _symtab.find("@");
  const auto ret_val = node->retval();
  if (!function) { // we may be in main
    const auto main = _symtab.find("_main");
    if (main) {
      if (!ret_val)
        throw std::string("wrong type of return value in main (int was expected)");
      ret_val->accept(this, lvl + 2);
      if (!ret_val->is_typed(cdk::TYPE_INT))
        throw std::string("wrong type of return value in main (int was expected)");
      return;
    }
    throw std::string("return statement found outside function");
  } else if (!ret_val) {
    return;
  }

  const auto &fun_sym_type = cdk::functional_type::cast(function->type());
  const auto function_output = fun_sym_type->output(0);
  const bool has_output = fun_sym_type->output() != nullptr;
  if (has_output && function_output->name() == cdk::TYPE_VOID)
    throw std::string("return with a value in void function");
  else if (!has_output)
    throw std::string("unknown return type in function");

  ret_val->accept(this, lvl + 2);
  incompatible_types_throw(function_output, ret_val->type(), true);
}

//---------------------------------------------------------------------------

void til::type_checker::do_nullptr_node(til::nullptr_node *const node,
                                        int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::reference_type::create(
      4, cdk::primitive_type::create(0, cdk::TYPE_VOID)));
}

//---------------------------------------------------------------------------

void til::type_checker::do_declaration_node(til::declaration_node *const node,
                                            int lvl) {
  const auto &init = node->initializer();
  if (init) {
    init->accept(this, lvl + 2);
    if (node->type()) {
      on_match_change_type(node, init);
      incompatible_types_throw(node->type(), init->type());
      if (node->type()->name() == cdk::TYPE_UNSPEC)
        node->type(init->type());
    } else {
      node->type(init->type());
    }
  }

  const auto new_symbol = til::make_symbol(
      node->type(), node->identifier(), (bool)node->initializer(), node->qualifier());
  if (!_symtab.insert(node->identifier(), new_symbol)) {
    // in this case, we are redeclaring a variable
    const auto previous_symbol = _symtab.find_local(node->identifier());
    // the redeclared type must be the exact same
    if (previous_symbol->type()->name() != node->type()->name())
      throw std::string("cannot redeclare variable '" + node->identifier() +
                        "' with incompatible type");
    _symtab.replace(node->identifier(), new_symbol);
  }
  _parent->set_new_symbol(new_symbol);
}

//---------------------------------------------------------------------------

void til::type_checker::do_read_node(til::read_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

//---------------------------------------------------------------------------

void til::type_checker::do_sizeof_node(til::sizeof_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------

void til::type_checker::do_indexptr_node(til::indexptr_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->base()->accept(this, lvl + 2);
  if (!node->base()->is_typed(cdk::TYPE_POINTER))
    throw std::string("wrong type in base of index expression");
  node->index()->accept(this, lvl + 2);
  if (!node->index()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in index of index expression");
  const auto base_ref =
      cdk::reference_type::cast(node->base()->type())->referenced();
  node->type(base_ref);
}

//---------------------------------------------------------------------------

void til::type_checker::do_stack_alloc_node(til::stack_alloc_node *const node,
                                            int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in argument of stack_alloc expression");
  node->type(cdk::reference_type::create(
      4, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC)));
}

//---------------------------------------------------------------------------

void til::type_checker::do_address_node(til::address_node *const node,
                                           int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 2);
  node->type(cdk::reference_type::create(4, node->lvalue()->type()));
}

//---------------------------------------------------------------------------

void til::type_checker::do_function_call_node(
    til::function_call_node *const node, int lvl) {
  ASSERT_UNSPEC;
  std::vector<std::shared_ptr<cdk::basic_type>> args_types;

  if (node->func()) { // regular call
    node->func()->accept(this, lvl + 2);
    if (!(node->func()->is_typed(cdk::TYPE_FUNCTIONAL)))
      throw std::string("wrong type in function call expression");

    const auto &type = node->func()->type();
    args_types = cdk::functional_type::cast(type)->input()->components();
    node->type(cdk::functional_type::cast(type)->output(0));
  } else { // recursive call (@)
    auto symbol = _symtab.find("@");
    if (!symbol) {
      throw std::string("recursive call not allowed in the current scope");
    }
    const auto &type = symbol->type();
    args_types = cdk::functional_type::cast(type)->input()->components();
    node->type(cdk::functional_type::cast(type)->output(0));
  }

  if (node->arguments()) {
    if (args_types.size() != node->arguments()->size())
      throw std::string(
          "wrong number of arguments in function call expression");
    node->arguments()->accept(this, lvl + 2);

    for (size_t i = 0; i < args_types.size(); i++) {
      const auto &param_type =
          dynamic_cast<cdk::expression_node *>(node->arguments()->node(i))
              ->type();
      // note that the second condition is to allow passing an int as a double
      if ((args_types[i] == param_type) ||
          (args_types[i]->name() == cdk::TYPE_DOUBLE &&
           param_type->name() == cdk::TYPE_INT))
        continue;
      throw std::string("wrong type in argument of function call expression");
    }
  }
}
