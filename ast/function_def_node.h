#ifndef __TIL_AST_FUNCTION_DEF_NODE_H__
#define __TIL_AST_FUNCTION_DEF_NODE_H__

#include <memory>
#include <vector>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/typed_node.h>
#include <cdk/types/basic_type.h>
#include <cdk/types/functional_type.h>
#include <cdk/types/primitive_type.h>
#include <cdk/types/typename_type.h>
#include "block_node.h"

namespace til {

  /**
   * Class for describing function definition nodes.
   */
  class function_def_node: public cdk::expression_node {
    cdk::sequence_node *_arguments;
    til::block_node *_block;
    bool _is_main;

  public:
    inline function_def_node(int lineno,
          cdk::sequence_node *arguments,
          std::shared_ptr<cdk::basic_type> return_type,
          til::block_node *block,
          bool is_main = false) :
        cdk::expression_node(lineno), _arguments(arguments), _block(block), _is_main(is_main) {
          std::vector<std::shared_ptr<cdk::basic_type>> arg_types;
          for (size_t i = 0; i < arguments->size(); i++) {
            arg_types.push_back(dynamic_cast<cdk::typed_node*>(arguments->node(i))->type());
          }

          this->type(cdk::functional_type::create(arg_types, return_type));
    }
    /** Constructor for the main function */
    inline function_def_node(int lineno, til::block_node *block) :
        cdk::expression_node(lineno), _arguments(new cdk::sequence_node(lineno)), _is_main(true) {
          this->type(cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_INT)));
    }

  public:
    inline cdk::sequence_node *arguments() {
      return _arguments;
    }
    inline cdk::basic_node *block() {
      return _block;
    }
    inline bool is_main() {
      return _is_main;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_def_node(this, level);
    }

  };

} // til

#endif