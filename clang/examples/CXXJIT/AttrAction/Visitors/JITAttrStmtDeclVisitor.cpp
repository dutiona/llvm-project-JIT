#include <AttrAction/Visitors/JITAttrStmtDeclVisitor.hpp>

#include <JITRegistry.hpp>
#include <helpers.hpp>

JITAttrStmtDeclVisitor::JITAttrStmtDeclVisitor(clang::ASTContext &Context)
    : Context{Context} {}

bool JITAttrStmtDeclVisitor::VisitVarDecl(const clang::VarDecl *VD) {
  if (_jit::DeclMarkedForJIT().contains(VD->getID())) {
    auto &os = _jit::log_debug()
               << "JITAttrStmtDeclVisitor::VisitVarDecl TRUE <"
               << VD->getNameAsString() << ">\n";

    if (!VD->hasInit()) {
      os << "Ignoring as there is no init statement to jit...\n";
      _jit::DeclMarkedForJIT().erase(VD->getID());
    }

    auto *initStmt = VD->getInit();
    auto callexprs = _jit::extract_templated_callexpr_to_jit_in_stmt(initStmt);
    if (!callexprs.empty()) {
      for (const auto [fdecl, callexpr, declrefexpr] : callexprs) {
        os << "CallerExpr marked for JIT:\n";
        auto *ftdecl = fdecl->getDescribedFunctionTemplate();
        auto fid = ftdecl ? ftdecl->getID() : fdecl->getID();
        _jit::pretty_print_stmt(callexpr, os);
        os << "With funcDecl info:" << " FID<" << fid
           << ">: " << fdecl->getNameAsString() << "\n";

        _jit::CallerExprsMarkedToJIT().emplace(
            callexpr->getID(Context),
            _jit::CallExprToJIT{.fname = fdecl->getNameAsString(),
                                .fdeclID = fdecl->getID(),
                                .cxprID = callexpr->getID(Context),
                                .cxprptr = callexpr,
                                .fptr = fdecl,
                                .declrefexpr = declrefexpr});

        _jit::DeclMarkedForJIT().erase(VD->getID());
      }
    }
  }

  return true;
}

bool JITAttrStmtDeclVisitor::VisitDeclStmt(const clang::DeclStmt *DS) {
  // FIXME is this visitor useful for us? Maybe only for when we can handle
  // the attribute on a statement.

  /*
  if (_jit::DeclMarkedForJIT().contains(DS->getID(Context))) {
    auto &os = _jit::log_debug()
               << "[JIT PLUGIN] JITAttrStmtDeclVisitor::VisitDeclStmt TRUE <";
    _jit::pretty_print_stmt(DS, os);
    os << ">\n";
  }
  */

  return true;
}

bool JITAttrStmtDeclVisitor::VisitAttributedStmt(
    const clang::AttributedStmt *AS) {
  // FIXME is this visitor useful for us? Maybe only for when we can handle
  // the attribute on a statement.

  /*
  if (_jit::DeclMarkedForJIT().contains(AS->getID(Context))) {
    auto &os =
        _jit::log_debug()
        << "[JIT PLUGIN] JITAttrStmtDeclVisitor::VisitAttributedStmt TRUE <";
    _jit::pretty_print_stmt(AS, os);
    os << ">\n";
  }
  */

  return true;
}
