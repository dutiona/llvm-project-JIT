#pragma once

#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Frontend/CompilerInstance.h>

class JITAttrStmtDeclVisitor
    : public clang::RecursiveASTVisitor<JITAttrStmtDeclVisitor> {
public:
  explicit JITAttrStmtDeclVisitor(clang::ASTContext &Context);

  bool VisitVarDecl(const clang::VarDecl *VD);
  bool VisitDeclStmt(const clang::DeclStmt *DS);
  bool VisitAttributedStmt(const clang::AttributedStmt *AS);

private:
  clang::ASTContext &Context;
};
