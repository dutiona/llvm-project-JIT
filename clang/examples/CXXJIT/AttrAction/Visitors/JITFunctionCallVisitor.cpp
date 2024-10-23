#include <AttrAction/Visitors/JITFunctionCallVisitor.hpp>

#include <JITRegistry.hpp>
#include <algorithm>
#include <helpers.hpp>

JITFunctionCallVisitor::JITFunctionCallVisitor(clang::ASTContext &Context)
    : Context{Context} {}

bool JITFunctionCallVisitor::VisitCallExpr(clang::CallExpr *Call) {

  if (Call) {
    auto callsites = _jit::extract_templated_callexpr_to_jit_in_stmt(Call);
    for (const auto &callsite : callsites) {
      if (_jit::FunctionsMarkedToJIT().contains(callsite.fdecl->getID())) {
        _jit::log_debug() << "Caller to FunctionDecl <"
                          << callsite.fdecl->getNameAsString()
                          << "> is marked for JIT\n";
        _jit::CallerExprsMarkedToJIT().emplace(
            callsite.callexpr->getID(Context),
            _jit::CallExprToJIT{.fname = callsite.fdecl->getNameAsString(),
                                .fdeclID = callsite.fdecl->getID(),
                                .cxprID = callsite.callexpr->getID(Context),
                                .cxprptr = callsite.callexpr,
                                .fptr = callsite.fdecl,
                                .declrefexpr = callsite.declrefexpr});
      }
    }
  }

  return true;
}

bool JITFunctionCallVisitor::VisitFunctionDecl(clang::FunctionDecl *FD) {
  if (FD && not _jit::FunctionsMarkedToJIT().contains(FD->getID())) {
    auto ret = std::find_if(
        _jit::CallerExprsMarkedToJIT().begin(),
        _jit::CallerExprsMarkedToJIT().end(),
        [FD](const auto &el) { return el.second.fdeclID == FD->getID(); });

    if (ret != _jit::CallerExprsMarkedToJIT().end()) {
      _jit::log_debug() << "FunctionDecl found in callexprs marked for JIT <"
                        << FD->getID() << ">: " << FD->getNameAsString()
                        << "\n";
      _jit::FunctionsMarkedToJIT().emplace(
          FD->getID(), _jit::FuncToJIT{.fptr = FD,
                                       .fdeclID = FD->getID(),
                                       .name = FD->getNameAsString()});
    }
  }

  return true;
}

/*
bool JITFunctionCallVisitor::VisitCallExpr(clang::CallExpr *Call) {
  if (auto *Callee = llvm::dyn_cast<clang::DeclRefExpr>(Call->getCallee())) {
    _jit::log_debug() << "Callee <" << Callee->getNameInfo() << ">\n";
    if (auto *FD = llvm::dyn_cast<clang::FunctionDecl>(Callee->getDecl())) {
      _jit::log_debug() << "FunctionDecl <" << FD->getNameAsString() << ">\n";
      if (_jit::FunctionsMarkedToJIT().find(FD->getID()) !=
          _jit::FunctionsMarkedToJIT().end()) {
        _jit::log_debug() << "Found call to function " <<
FD->getNameAsString()
                          << "\n";

        // Step 2: We need to replace the function being called with
        // JitFunction
        clang::FunctionDecl *JitFunction =
            _jit::FunctionsMarkedToJIT()[FD->getID()];

        if (JitFunction) {
          _jit::log_debug() << "Yes3\n";
          // Replace the callee of the CallExpr with the JitFunction

          clang::DeclRefExpr *NewCallee = clang::DeclRefExpr::Create(
              Context,
              clang::NestedNameSpecifierLoc(), // Qualifier
              clang::SourceLocation(),         // TemplateKWLoc
              JitFunction,
              false, // RefersToEnclosingVariableOrCapture
              clang::SourceLocation(), JitFunction->getType(),
              clang::VK_LValue);

          Call->setCallee(NewCallee);

          // Step 3: Replace arguments (including template parameters if
          // necessary)
          replaceCallArguments(Call, FD, JitFunction);
        }
      }
    }
  } else {
    _jit::log_debug() << "Ignoring... \n";
    _jit::pretty_print_stmt(Call, llvm::outs());
    llvm::outs() << "getCallee "
                 << (llvm::dyn_cast<clang::DeclRefExpr>(Call->getCallee())
                         ? "true"
                         : "false")
                 << "\n";
    llvm::outs() << "getCalleeDecl ";
    if (auto *decl = llvm::dyn_cast<clang::Decl>(Call->getCalleeDecl())) {
      llvm::outs() << "true\n";
      if (decl->isFunctionOrFunctionTemplate()) {
        auto *fdecl = llvm::dyn_cast<clang::FunctionDecl>(decl);
        llvm::outs() << "Is templated ? ";
        if (!fdecl->isTemplated()) {
          llvm::outs() << "true\n";
          llvm::outs() << "Attributes: ";
          for (auto *attr : fdecl->getAttrs()) {
            llvm::outs() << attr->getAttrName() << ", ";
          }
          llvm::outs() << "\n";
        } else {
          llvm::outs() << "false\n";
        }
      }
    } else {
      llvm::outs() << "false\n";
    }
  }
  return true;
}

// Helper function to adjust the arguments of the call for the new JitFunction
void JITFunctionCallVisitor::replaceCallArguments(
    clang::CallExpr *Call, clang::FunctionDecl *OldFunc,
    clang::FunctionDecl *NewFunc) {
  // If the original function had template arguments, pass them as regular
  // arguments to JitFunction.
  auto *FTDecl = OldFunc->getDescribedFunctionTemplate();
  auto *TemplateArgs = FTDecl->getTemplateParameters();

  // Create a list of new arguments combining the template args and the
  // original arguments
  std::vector<clang::Expr *> NewArgs;

  if (TemplateArgs && TemplateArgs->size() > 0) {
    // Handle the template arguments, which are now regular arguments
    for (auto *TemplateParam : TemplateArgs->asArray()) {
      if (auto *NTTP =
              llvm::dyn_cast<clang::NonTypeTemplateParmDecl>(TemplateParam)) {
        // Create a dummy argument for the template parameter
        // (in reality, you would pass a real value based on the template
        // instantiation)
        clang::Expr *DummyArg = clang::IntegerLiteral::Create(
            Context, llvm::APInt(32, 42), Context.IntTy,
            clang::SourceLocation());
        NewArgs.push_back(DummyArg);
      }
    }
  }

  // Forward the original arguments from the old function to the new function
  for (unsigned i = 0; i < Call->getNumArgs(); ++i) {
    clang::Expr *Arg = Call->getArg(i);
    NewArgs.push_back(Arg);
  }

  // Set the new arguments for the call
  int i = 0;
  for (auto *NewArg : NewArgs) {
    Call->setArg(i, NewArg);
    ++i;
  }
  Call->computeDependence();
}
*/