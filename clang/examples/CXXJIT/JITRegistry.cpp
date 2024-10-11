#include <JITRegistry.hpp>

namespace _jit {

auto DeclMarkedForJIT() -> std::unordered_set<unsigned long> & {
  static std::unordered_set<unsigned long> Decls = {};
  return Decls;
}

auto StmtMarkedForJIT() -> std::unordered_set<unsigned long> & {
  static std::unordered_set<unsigned long> Stmts = {};
  return Stmts;
}

auto FunctionsMarkedToJIT() -> std::unordered_map<unsigned long, FuncToJIT> & {
  static std::unordered_map<unsigned long, FuncToJIT> Functions = {};
  return Functions;
}

auto CallerExprsMarkedToJIT()
    -> std::unordered_map<unsigned long, CallExprToJIT> & {
  static std::unordered_map<unsigned long, CallExprToJIT> CallerExprs = {};
  return CallerExprs;
}
/*
auto FunctionsToJIT() -> std::map<unsigned, clang::FunctionDecl *> & {
  static std::map<unsigned, clang::FunctionDecl *> Functions = {};
  return Functions;
}
auto CallerExprsToJIT() -> std::map<unsigned, clang::CallExpr *> & {
  static std::map<unsigned, clang::CallExpr *> CallerExprs = {};
  return CallerExprs;
}
*/

} // namespace _jit