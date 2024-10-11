#pragma once

#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Frontend/CompilerInstance.h>

class JITFunctionCallVisitor
    : public clang::RecursiveASTVisitor<JITFunctionCallVisitor> {
public:
  explicit JITFunctionCallVisitor(clang::ASTContext &Context);

  bool VisitCallExpr(clang::CallExpr *Call);
  bool VisitFunctionDecl(clang::FunctionDecl *FD);

private:
  clang::ASTContext &Context;
};
