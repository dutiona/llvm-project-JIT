#include <AttrAction/Visitors/JITFunctionDeclVisitor.hpp>

#include <JITRegistry.hpp>
#include <helpers.hpp>

JITFunctionDeclVisitor::JITFunctionDeclVisitor(clang::CompilerInstance &CI,
                                               clang::ASTContext &Context)
    : Context(Context), CI(CI) {}

bool JITFunctionDeclVisitor::VisitFunctionDecl(clang::FunctionDecl *FD) {
  /*
  llvm::outs() << "[NOTICE][JIT PLUGIN] Visiting function"
               << FD->getNameAsString() << ":" << FD->getID() << "/" << FD
               << "\n";
  */
  if (_jit::FunctionsMarkedToJIT().find(FD->getID()) !=
      _jit::FunctionsMarkedToJIT().end()) {

    _jit::log_debug() << "Visiting function" << FD->getNameAsString() << ":"
                      << FD->getID() << "/" << FD
                      << " which has been tagged with JIT attribute.\n";

    _jit::log_debug() << "Rewriting template function: "
                      << FD->getNameAsString() << "\n";

    // Get the declaration context to insert the new function.
    clang::DeclContext *DC = FD->getDeclContext();

    // Create the new non-template function.
    [[maybe_unused]]
    clang::FunctionDecl *JitFunction =
        _jit::createJitFunctionDecl(Context, FD, CI.getSourceManager(), DC);

    if (JitFunction) {
      DC->addDecl(JitFunction);

      auto *Stmt = _jit::createDummyFunctionBody(Context, JitFunction);
      if (Stmt)
        JitFunction->setBody(Stmt);

      // Store for callsite parsing
      // _jit::FunctionsMarkedToJIT()[FD->getID()] = JitFunction;
    }
  }

  return true;
}
