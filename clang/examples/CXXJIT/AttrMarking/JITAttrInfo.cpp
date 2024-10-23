#include <AttrMarking/JITAttrInfo.hpp>

#include <JITRegistry.hpp>
#include <helpers.hpp>

#include <clang/AST/Attr.h>
#include <clang/AST/Expr.h>
#include <clang/AST/Stmt.h>
#include <clang/Basic/DiagnosticFrontend.h>
#include <clang/Basic/ParsedAttrInfo.h>
#include <clang/Sema/ParsedAttr.h>
#include <clang/Sema/Sema.h>
#include <llvm/Support/Casting.h>

namespace _jit {

JITAttrInfo::JITAttrInfo() : ParsedAttrInfo() {
  OptArgs = 0;
  // IsStmt = true;
  // AttrKind = clang::AttributeCommonInfo::NoSemaHandlerAttribute;

  static constexpr Spelling S[] = {{clang::ParsedAttr::AS_GNU, "jit"},
                                   {clang::ParsedAttr::AS_C23, "jit"},
                                   {clang::ParsedAttr::AS_CXX11, "jit"},
                                   {clang::ParsedAttr::AS_CXX11, "clang::jit"}};
  Spellings = S;
}

bool JITAttrInfo::diagAppertainsToDecl(clang::Sema &S,
                                       const clang::ParsedAttr &Attr,
                                       const clang::Decl *D) const {
  _jit::DeclMarkedForJIT().emplace(D->getID());
  auto &os = _jit::log_debug() << "Decl marked for jit:";
  _jit::pretty_print_decl(D, os);
  return true;
}

bool JITAttrInfo::diagAppertainsToStmt(clang::Sema &S,
                                       const clang::ParsedAttr &Attr,
                                       const clang::Stmt *St) const {

  //_jit::StmtMarkedForJIT().emplace(
  //    St->getID(S.getASTContext()),
  //    nullptr /* to be filled during AST traversal */);
  // auto &os = _jit::log_debug() << "Stmt marked for jit:";
  //_jit::pretty_print_stmt(St, os);
  // return true;

  return false;
}

clang::ParsedAttrInfo::AttrHandling
JITAttrInfo::handleDeclAttribute(clang::Sema &S, clang::Decl *D,
                                 const clang::ParsedAttr &Attr) const {

  // handle funcDecl to JIT
  if (D->isFunctionOrFunctionTemplate()) {
    auto *FDecl = D->getAsFunction();

    _jit::log_debug() << "Function <" << FDecl->getNameAsString() << ":"
                      << FDecl->getID() << "/" << FDecl << "/" << FDecl << "> ("
                      << FDecl->getLocation().printToString(
                             S.getSourceManager())
                      << "): " << "Marked with [[jit]] attribute.\n";
    _jit::FunctionsMarkedToJIT().emplace(
        FDecl->getID(), _jit::FuncToJIT{.fptr = FDecl,
                                        .fdeclID = FDecl->getID(),
                                        .name = FDecl->getNameAsString()});
    // no duplicate
    DeclMarkedForJIT().erase(FDecl->getID());
  }

  return AttributeApplied;
}

} // namespace _jit
