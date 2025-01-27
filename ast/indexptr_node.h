#ifndef __TIL_AST_INDEXPTR_NODE_H__
#define __TIL_AST_INDEXPTR_NODE_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>

namespace til {

  /**
   * Class for describing pointer index nodes.
   */
  class indexptr_node: public cdk::lvalue_node {
    cdk::expression_node *_base;
    cdk::expression_node *_index;

  public:
    inline indexptr_node(int lineno, cdk::expression_node *base, cdk::expression_node *index) :
        cdk::lvalue_node(lineno), _base(base), _index(index) {
    }

  public:
    inline cdk::expression_node *base() {
      return _base;
    }
    inline cdk::expression_node *index() {
      return _index;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_indexptr_node(this, level);
    }

  };

} // til

#endif