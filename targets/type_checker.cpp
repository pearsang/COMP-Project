#include <string>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h" // automatically generated
#include <cdk/types/primitive_type.h>

#define ASSERT_UNSPEC                                                 \
  {                                                                   \
    if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) \
      return;                                                         \
  }

bool til::type_checker::check_compatible_ptr_types(
    std::shared_ptr<cdk::basic_type> type1, std::shared_ptr<cdk::basic_type> type2)
{
  auto ptr_type1 = type1; // check initialization
  auto ptr_type2 = type2;

  while (ptr_type1->name() == cdk::TYPE_POINTER && ptr_type2->name() == cdk::TYPE_POINTER)
  {
    ptr_type1 = cdk::reference_type::cast(ptr_type1)->referenced();
    ptr_type2 = cdk::reference_type::cast(ptr_type2)->referenced();
  }

  return ptr_type1->name() == ptr_type2->name() || ptr_type2->name() == cdk::TYPE_UNSPEC;
}

bool til::type_checker::check_compatible_func_types(
    std::shared_ptr<cdk::functional_type> type1,
    std::shared_ptr<cdk::functional_type> type2)
{

  // arguments must be the same
  if (type1->input_length() != type2->input_length())
  {
    return false;
  }

  // return types must be compatible
  if (type1->output_length() > 0 && type2->output_length() > 0)
  {
    if (!check_compatible_ptr_types(type1->output(0), type2->output(0)))
    {
      return false;
    }
  }

  // check argument types - must be compatible
  for (size_t i = 0; i < type1->input_length(); i++)
  {
    if (!check_compatible_types(type1->input(i), type2->input(i)))
    {
      return false;
    }
  }

  return true;
}

// TODO: check if all casees are covered
bool til::type_checker::check_compatible_types(
    std::shared_ptr<cdk::basic_type> type1, std::shared_ptr<cdk::basic_type> type2, bool is_return)
{

  const auto type1_name = type1->name();
  const auto type2_name = type2->name();

  switch (type1_name)
  {
  case cdk::TYPE_DOUBLE:
  case cdk::TYPE_INT:
    if (!(type2_name == cdk::TYPE_DOUBLE || type2_name == cdk::TYPE_INT))
    {
      return false;
    }
    break;
  // TODO: check if this is correct - role of is_return
  case cdk::TYPE_POINTER:
    if ((is_return == (type2_name != cdk::TYPE_POINTER)) && !check_compatible_ptr_types(type1, type2))
    {
      return false;
    }
    break;
  case cdk::TYPE_STRING:
    if (type2_name != cdk::TYPE_STRING)
    {
      return false;
    }
    break;
  case cdk::TYPE_FUNCTIONAL:
    if (!((type2_name == cdk::TYPE_FUNCTIONAL &&
           check_compatible_func_types(cdk::functional_type::cast(type1),
                                       cdk::functional_type::cast(type2))) ||
          (type2_name == cdk::TYPE_POINTER &&
           cdk::reference_type::cast(type2)->referenced() == nullptr)))
    {
      return false;
    }
    break;

  case cdk::TYPE_UNSPEC: // auto situations like auto x = f(), where f() returns void - auto cannot be void;
    if (type2_name == cdk::TYPE_UNSPEC)
    {
      return false;
    }
    break;

  default:
    if (type1_name != type2_name)
    {
      return false;
    }
  }

  return true;
}

void til::type_checker::throw_incompatible_types_error(
    std::shared_ptr<cdk::basic_type> type1, std::shared_ptr<cdk::basic_type> type2,
    bool is_return)
{
  if (check_compatible_types(type1, type2, false))
  {
    return;
  }

  const std::string field_name =
      is_return ? "return" : "initialization";

  switch (type1->name())
  {
  case cdk::TYPE_DOUBLE:
  case cdk::TYPE_INT:
    throw std::string("incompatible types in " + field_name +
                      " (int or double was expected)");

  case cdk::TYPE_POINTER:
    throw std::string("incompatible types in " + field_name + " (pointer was expected)");

  case cdk::TYPE_STRING:
    throw std::string("incompatible types in " + field_name + " (string was expected)");

  case cdk::TYPE_FUNCTIONAL:
    throw std::string("incompatible types in " + field_name + " (function was expected)");

  default:
    throw std::string("incompatible types in " + field_name);
  }
}

void til::type_checker::change_type_on_match(cdk::typed_node *const left_value,
                                             cdk::typed_node *const right_value)
{
  const auto left_type = left_value->type();
  const auto right_type = right_value->type();

  // auto x = input
  if (left_type->name() == cdk::TYPE_UNSPEC && right_type->name() == cdk::TYPE_UNSPEC)
  {
    left_value->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    right_value->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else if ((left_type->name() == cdk::TYPE_POINTER &&
            right_type->name() == cdk::TYPE_POINTER &&
            check_compatible_ptr_types(left_type, right_type)) ||
           (left_type->name() == cdk::TYPE_FUNCTIONAL &&
            right_type->name() == cdk::TYPE_FUNCTIONAL &&
            check_compatible_func_types(cdk::functional_type::cast(left_type),
                                        cdk::functional_type::cast(right_type))) ||
           ((left_type->name() == cdk::TYPE_INT || left_type->name() == cdk::TYPE_DOUBLE) &&
            right_type->name() == cdk::TYPE_UNSPEC))
  {
    right_value->type(left_type);
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl)
{
  // EMPTY
}

//---------------------------------------------------------------------------

void til::type_checker::do_nil_node(cdk::nil_node *const node, int lvl)
{
  // EMPTY
}
void til::type_checker::do_data_node(cdk::data_node *const node, int lvl)
{
  // EMPTY
}
void til::type_checker::do_double_node(cdk::double_node *const node, int lvl)
{
  // EMPTY
}
void til::type_checker::do_not_node(cdk::not_node *const node, int lvl)
{
  // EMPTY
}
void til::type_checker::do_and_node(cdk::and_node *const node, int lvl)
{
  // EMPTY
}
void til::type_checker::do_or_node(cdk::or_node *const node, int lvl)
{
  // EMPTY
}

//---------------------------------------------------------------------------

void til::type_checker::do_integer_node(cdk::integer_node *const node, int lvl)
{
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_string_node(cdk::string_node *const node, int lvl)
{
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

//---------------------------------------------------------------------------

void til::type_checker::processUnaryExpression(cdk::unary_operation_node *const node, int lvl)
{
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in argument of unary expression");

  // in Simple, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_unary_minus_node(cdk::unary_minus_node *const node, int lvl)
{
  processUnaryExpression(node, lvl);
}

void til::type_checker::do_unary_plus_node(cdk::unary_plus_node *const node, int lvl)
{
  processUnaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void til::type_checker::processBinaryExpression(cdk::binary_operation_node *const node, int lvl)
{
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in left argument of binary expression");

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in right argument of binary expression");

  // in Simple, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_add_node(cdk::add_node *const node, int lvl)
{
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_sub_node(cdk::sub_node *const node, int lvl)
{
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_mul_node(cdk::mul_node *const node, int lvl)
{
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_div_node(cdk::div_node *const node, int lvl)
{
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_mod_node(cdk::mod_node *const node, int lvl)
{
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_lt_node(cdk::lt_node *const node, int lvl)
{
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_le_node(cdk::le_node *const node, int lvl)
{
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_ge_node(cdk::ge_node *const node, int lvl)
{
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_gt_node(cdk::gt_node *const node, int lvl)
{
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_ne_node(cdk::ne_node *const node, int lvl)
{
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_eq_node(cdk::eq_node *const node, int lvl)
{
  processBinaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void til::type_checker::do_variable_node(cdk::variable_node *const node, int lvl)
{
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<til::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr)
  {
    node->type(symbol->type());
  }
  else
  {
    throw id;
  }
}

void til::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl)
{
  ASSERT_UNSPEC;
  try
  {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  }
  catch (const std::string &id)
  {
    throw "undeclared variable '" + id + "'";
  }
}

void til::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl)
{
  ASSERT_UNSPEC;

  try
  {
    node->lvalue()->accept(this, lvl);
  }
  catch (const std::string &id)
  {
    auto symbol = std::make_shared<til::symbol>(cdk::primitive_type::create(4, cdk::TYPE_INT), id, 0);
    _symtab.insert(id, symbol);
    _parent->set_new_symbol(symbol);   // advise parent that a symbol has been inserted
    node->lvalue()->accept(this, lvl); // DAVID: bah!
  }

  if (!node->lvalue()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in left argument of assignment expression");

  node->rvalue()->accept(this, lvl + 2);
  if (!node->rvalue()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in right argument of assignment expression");

  // in Simple, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------
void til::type_checker::do_program_node(til::program_node *const node, int lvl)
{
  // EMPTY
}

void til::type_checker::do_evaluation_node(til::evaluation_node *const node, int lvl)
{
  node->argument()->accept(this, lvl + 2);
}

void til::type_checker::do_print_node(til::print_node *const node, int lvl)
{
  for (size_t i = 0; i < node->arguments()->size(); i++)
  {
    auto arg = dynamic_cast<cdk::expression_node *>(node->arguments()->node(i));

    arg->accept(this, lvl);

    if (arg->is_typed(cdk::TYPE_UNSPEC))
    {
      arg->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
    else if (!arg->is_typed(cdk::TYPE_INT) && !arg->is_typed(cdk::TYPE_STRING) && !arg->is_typed(cdk::TYPE_DOUBLE))
    {
      throw std::string("incorrect type for argument " + std::to_string(i + 1) + " of print instruction");
    }
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_read_node(til::read_node *const node, int lvl)
{
  // try {
  //   node->argument()->accept(this, lvl);
  // } catch (const std::string &id) {
  //   throw "undeclared variable '" + id + "'";
  // }
}

//---------------------------------------------------------------------------

void til::type_checker::do_while_node(til::while_node *const node, int lvl)
{
  node->condition()->accept(this, lvl + 4);
}

void til::type_checker::do_stop_node(til::stop_node *const node, int lvl)
{
  // TODO: not needed for now
  throw "not done yet";
}

//---------------------------------------------------------------------------

void til::type_checker::do_next_node(til::next_node *const node, int lvl)
{
  // TODO: not needed for now
  throw "not done yet";
}

//---------------------------------------------------------------------------

void til::type_checker::do_return_node(til::return_node *const node, int lvl)
{
  // TODO: not needed for now
  throw "not done yet";
}

//---------------------------------------------------------------------------

void til::type_checker::do_block_node(til::block_node *const node, int lvl)
{
  // TODO: not needed for now
  throw "not done yet";
}

//---------------------------------------------------------------------------

void til::type_checker::do_sizeof_node(til::sizeof_node *const node, int lvl)
{
  // TODO: not needed for now
  throw "not done yet";
}

//---------------------------------------------------------------------------

void til::type_checker::do_function_call_node(til::function_call_node *const node, int lvl)
{
  // TODO: not needed for now
  throw "not done yet";
}

//---------------------------------------------------------------------------

void til::type_checker::do_nullptr_node(til::nullptr_node *const node, int lvl)
{
  // TODO: not needed for now
  throw "not done yet";
}

//---------------------------------------------------------------------------

void til::type_checker::do_indexptr_node(til::indexptr_node *const node, int lvl)
{
  // TODO: not needed for now
  throw "not done yet";
}

//---------------------------------------------------------------------------

void til::type_checker::do_declaration_node(til::declaration_node *const node, int lvl)
{
  // TODO: not needed for now
  throw "not done yet";
}

//---------------------------------------------------------------------------

void til::type_checker::do_function_def_node(til::function_def_node *const node, int lvl)
{
  // TODO: not needed for now
  throw "not done yet";
}

//---------------------------------------------------------------------------

void til::type_checker::do_address_node(til::address_node *const node, int lvl)
{
  // TODO: not needed for now
  throw "not done yet";
}

//---------------------------------------------------------------------------

void til::type_checker::do_stack_alloc_node(til::stack_alloc_node *const node, int lvl)
{
  // TODO: not needed for now
  throw "not done yet";
}

//---------------------------------------------------------------------------

void til::type_checker::do_if_node(til::if_node *const node, int lvl)
{
  node->condition()->accept(this, lvl + 4);
}

void til::type_checker::do_if_else_node(til::if_else_node *const node, int lvl)
{
  node->condition()->accept(this, lvl + 4);
}
