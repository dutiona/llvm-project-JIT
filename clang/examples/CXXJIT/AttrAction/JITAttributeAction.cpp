#include <AttrAction/JITAttributeAction.hpp>

#include <AttrAction/DiagHandler/JITDiagConsumer.hpp>
#include <JITRegistry.hpp>
#include <helpers.hpp>

#include <clang/AST/ASTConsumer.h>
#include <clang/AST/ASTContext.h>
#include <clang/AST/Expr.h>
#include <clang/AST/Type.h>
#include <clang/Frontend/CompilerInstance.h>

#include <algorithm>
#include <vector>

struct FuncToJITEntry {
  _jit::FuncToJIT func;
  std::vector<_jit::CallExprToJIT> callsites;
};

static std::vector<FuncToJITEntry> construct_collection_for_JITengine() {
  auto mapping = std::vector<FuncToJITEntry>{};

  for (auto &[fid, fjit] : _jit::FunctionsMarkedToJIT()) {
    mapping.emplace_back(fjit, std::vector<_jit::CallExprToJIT>{});
  }

  for (auto &[cid, cexpr] : _jit::CallerExprsMarkedToJIT()) {
    auto found =
        std::find_if(mapping.begin(), mapping.end(), [&cexpr](const auto &el) {
          return cexpr.fdeclID == el.func.fptr->getID();
        });
    if (found != mapping.end()) {
      (*found).callsites.push_back(cexpr);
    } else {
      _jit::log_error() << "Something went wrong :'(\n";
      return {};
    }
  }

  return mapping;
}

static void dump_mapping(const std::vector<FuncToJITEntry> &mapping) {
  auto &os = _jit::log_debug() << "Dumping constructed mapping\n";
  for (const auto &[k, v] : mapping) {
    os << "\tFunction <" << k.name << "#" << k.fptr->getID()
       << "> has the following callsites:\n";
    for (const auto &cs : v) {
      os << "\t\t----";
      _jit::pretty_print_stmt(cs.cxprptr, os);
      os << "\t\t----";
    }
    os << "----------";
  }
}

static void diag_on_template_arg(const clang::TemplateArgumentLoc &targloc,
                                 clang::ASTContext &Context) {

  auto &os = _jit::log_debug() << "Infos on Template parameter:";
  auto targ = targloc.getArgument();
  targ.dump(os, Context);
  switch (targ.getKind()) {
  case clang::TemplateArgument::Null:
    os << "\tTKind: Null\n";
    break;
  case clang::TemplateArgument::Type:
    os << "\tTKind: Type\n";
    break;
  case clang::TemplateArgument::Declaration:
    os << "\tTKind: Declaration\n";
    break;
  case clang::TemplateArgument::NullPtr:
    os << "\tTKind: NullPtr\n";
    break;
  case clang::TemplateArgument::Integral:
    os << "\tTKind: Integral\n";
    break;
  case clang::TemplateArgument::StructuralValue:
    os << "\tTKind: StructuralValue\n";
    break;
  case clang::TemplateArgument::Template:
    os << "\tTKind: Template\n";
    break;
  case clang::TemplateArgument::TemplateExpansion:
    os << "\tTKind: TemplateExpansion\n";
    break;
  case clang::TemplateArgument::Expression:
    os << "\tTKind: Expression\n";
    break;
  case clang::TemplateArgument::Pack:
    os << "\tTKind: Pack\n";
    break;
    break;
  }
  /*
  auto *d = targ.getAsDecl();
  auto *e = targ.getAsExpr();
  // auto i = targ.getAsIntegral();
  // auto sv = targ.getAsStructuralValue();
  auto tpl = targ.getAsTemplate();
  auto tpl_pat = targ.getAsTemplateOrTemplatePattern();
  auto ty = targ.getAsType();

  os << "\tAsDecl: " << (d ? "true" : "false") << "\n";
  os << "\tAsExpr: " << (e ? "true" : "false") << ":\n";
  if (e) {
    if (e->isEvaluatable(Context)) {
      os << "\t\tisCXX11ConstantExpr: "
         << (e->isCXX11ConstantExpr(Context) ? "true" : "false") << "\n";
      os << "\t\tisCXX98IntegralConstantExpr: "
         << (e->isCXX98IntegralConstantExpr(Context) ? "true" : "false")
         << "\n";
      os << "\t\tisIntegerConstantExpr: "
         << (e->isIntegerConstantExpr(Context) ? "true" : "false") << "\n";
      os << "\t\tisConstantInitializer: "
         << (e->isConstantInitializer(Context, false) ? "true" : "false")
         << "\n";
    } else {
      os << "\t\tisElaboratedTypeSpecifier: "
         << (ty->isElaboratedTypeSpecifier() ? "true" : "false") << "\n";
      os << "\t\tisBuiltinType: " << (ty->isBuiltinType() ? "true" : "false")
         << "\n";
    }

    os << "\t\tisEvaluatable: "
       << (e->isEvaluatable(Context) ? "true" : "false") << "\n";
    os << "\t\tisNullPointerConstant: "
       << (e->isNullPointerConstant(Context,
                                    clang::Expr::NPC_ValueDependentIsNotNull)
               ? "true"
               : "false")
       << "\n";
    os << "\t\tisInstantiationDependent: "
       << (e->isInstantiationDependent() ? "true" : "false") << "\n";
    os << "\t\tisTypeDependent: " << (e->isTypeDependent() ? "true" : "false")
       << "\n";
    os << "\t\tisValueDependent: " << (e->isValueDependent() ? "true" : "false")
       << "\n";
  }
  */
  // os << "\tgetAsIntegral: " << i << ":\n";
  // os << "\tgetAsStructuralValue.isInt: " << (sv.isInt() ? "true" : "false")
  //   << ":\n";
}

static void filter_mapping(std::vector<FuncToJITEntry> &mapping,
                           clang::ASTContext &Context) {

  for (auto &[func, callsites] : mapping) {
    _jit::log_debug() << "Parsing callsites for: " << func.name << "#"
                      << func.fdeclID << "\n";
    for (auto &callsite : callsites) {
      auto &os = _jit::log_debug()
                 << "\tCallsite #" << callsite.cxprID << ":\n";
      _jit::pretty_print_stmt(callsite.cxprptr, os);
      if (callsite.declrefexpr->getNumTemplateArgs() > 0) {
        os << "Detecting template parameters... Diagnostic:\n";
        for (const auto targloc : callsite.declrefexpr->template_arguments()) {
          diag_on_template_arg(targloc, Context);
        }
      }
    }
  }
}

JITASTConsumer::JITASTConsumer(clang::CompilerInstance &CI,
                               clang::ASTContext &Context)
    : VisitorAttr{Context}, VisitorDecl{CI, Context}, VisitorCall{Context} {}

bool JITASTConsumer::HandleTopLevelDecl(clang::DeclGroupRef D) {
  /*
  auto &os = _jit::log_debug()
             << "Entering JITASTConsumer::HandleTopLevelDecl.\n";
  auto is_decl = D.isSingleDecl();
  if (is_decl) {
    auto *decl = D.getSingleDecl();
    os << "Dumping info on DECL<" << decl->getID() << ">\n";
    decl->dump(os);
    os << "\nEnd infodump\n------\n";
  }
  _jit::log_debug() << "Exiting JITASTConsumer::HandleTopLevelDecl.\n";
  */
  return true;
}

void JITASTConsumer::HandleInterestingDecl(clang::DeclGroupRef D) {
  _jit::log_debug() << "Entering JITASTConsumer::HandleInterestingDecl.\n";
  _jit::log_debug() << "Exiting JITASTConsumer::HandleInterestingDecl.\n";
}

void JITASTConsumer::HandleTranslationUnit(clang::ASTContext &Context) {
  _jit::log_debug() << "Entering JITASTConsumer::HandleTranslationUnit.\n";

  _jit::dump_registry();

  _jit::log_debug() << "ENTERING VISITOR PASSES\n";

  // Pass 1: handle correctly tagging called functions in init body of varDecl
  // for JIT
  {
    _jit::log_error() << "\tStarting pass 1...\n";
    VisitorAttr.TraverseDecl(Context.getTranslationUnitDecl());
    _jit::log_debug() << "Ending pass 1...\n";
  }

  // Here we should have either cleaned up all JIT Decl and push all relevant
  // CallExpr or FunDecl
  {
    assert(_jit::DeclMarkedForJIT().size() == 0);
    if (_jit::DeclMarkedForJIT().size() != 0)
      _jit::log_error() << "Still stuff in DeclMarkedForJIT()\n";
    else
      _jit::log_debug() << "DeclMarkedForJIT EMPTY, AS IT SHOULD BE!\n";

    assert(_jit::StmtMarkedForJIT().size() == 0);
    if (_jit::StmtMarkedForJIT().size() != 0)
      _jit::log_error() << "Still stuff in StmtMarkedForJIT\n";
    else
      _jit::log_debug() << "StmtMarkedForJIT EMPTY, AS IT SHOULD BE!\n";
  }

  // control
  _jit::dump_registry();

  // Pass 2: add all call sites (callers) for functions (callees) that are
  // tagged as JIT.
  // Also does a pass on functionDecl to ensure we're are good, but everything
  // should have been parsed with the Attr pass for FunctionDecl
  {
    _jit::log_debug() << "\tStarting pass 2...\n";
    VisitorCall.TraverseDecl(Context.getTranslationUnitDecl());
    _jit::log_debug() << "Ending pass 2...\n";
  }

  // control
  _jit::dump_registry();

  {
    // Pre-pass 3 : we construct the collection where we have for each FunctDecl
    // marked for JIT, all the callexpr corresponding.
    auto mapping = construct_collection_for_JITengine();
    dump_mapping(mapping);

    // Pass 3 : we filter-out function to JIT to those whose template parameters
    // are runtime values
    // We use our mapping so we do not need another visitor right now
    filter_mapping(mapping, Context);
  }

  // VisitorDecl.TraverseDecl(Context.getTranslationUnitDecl());
  _jit::log_debug() << "Exiting JITASTConsumer::HandleTranslationUnit.\n";
}

// Register the custom attribute in the plugin action.
bool JITAttributeAction::ParseArgs(const clang::CompilerInstance &CI,
                                   const std::vector<std::string> &args) {
  _jit::log_debug() << "PluginASTAction correctly registered\n";
  return true;
}

std::unique_ptr<clang::ASTConsumer>
JITAttributeAction::CreateASTConsumer(clang::CompilerInstance &CI,
                                      llvm::StringRef) {
  return std::make_unique<JITASTConsumer>(CI, CI.getASTContext());
}

bool JITAttributeAction::PrepareToExecuteAction(clang::CompilerInstance &CI) {
  _jit::log_debug() << "Entering PrepareToExecuteAction\n";

  // Set up our custom diagnostic consumer
  CI.getDiagnostics().setClient(new JITDiagnosticConsumer(),
                                /*OwnsClient=*/true);

  _jit::log_debug() << "Exiting PrepareToExecuteAction\n";

  return true;
}

bool JITAttributeAction::BeginInvocation(clang::CompilerInstance &CI) {
  _jit::log_debug() << "Entering BeginInvocation\n";

  // Set up our custom diagnostic consumer
  CI.getDiagnostics().setClient(new JITDiagnosticConsumer(),
                                /*OwnsClient=*/true);

  _jit::log_debug() << "Exiting BeginInvocation\n";

  return true;
}

bool JITAttributeAction::BeginSourceFileAction(clang::CompilerInstance &CI) {
  _jit::log_debug() << "Entering BeginSourceFileAction\n";

  // Set up our custom diagnostic consumer
  CI.getDiagnostics().setClient(new JITDiagnosticConsumer(),
                                /*OwnsClient=*/true);

  _jit::log_debug() << "Exiting BeginSourceFileAction\n";

  return true;
}

clang::PluginASTAction::ActionType JITAttributeAction::getActionType() {
  return AddBeforeMainAction;
}
