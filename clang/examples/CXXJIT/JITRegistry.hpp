#pragma once

#include <clang/AST/Decl.h>
#include <clang/AST/Expr.h>
#include <clang/AST/Stmt.h>

#include <string>
#include <unordered_map>
#include <unordered_set>

namespace _jit {

struct FuncToJIT {
  clang::FunctionDecl *fptr;
  long fdeclID;
  std::string name;
};

struct CallExprToJIT {
  std::string fname;
  long fdeclID;
  long cxprID;
  const clang::CallExpr *cxprptr;
  const clang::FunctionDecl *fptr;
  const clang::DeclRefExpr *declrefexpr;
};

auto DeclMarkedForJIT() -> std::unordered_set<unsigned long> &;
auto StmtMarkedForJIT() -> std::unordered_set<unsigned long> &;

auto FunctionsMarkedToJIT() -> std::unordered_map<unsigned long, FuncToJIT> &;
auto CallerExprsMarkedToJIT()
    -> std::unordered_map<unsigned long, CallExprToJIT> &;

/*
auto FunctionsToJIT() -> std::map<unsigned, clang::FunctionDecl *> &;
auto CallerExprsToJIT() -> std::map<unsigned, clang::CallExpr *> &;
*/

} // namespace _jit
