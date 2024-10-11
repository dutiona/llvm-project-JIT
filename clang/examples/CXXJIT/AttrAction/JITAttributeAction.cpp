#include <AttrAction/JITAttributeAction.hpp>

#include <JITRegistry.hpp>
#include <algorithm>
#include <helpers.hpp>

#include <clang/AST/ASTConsumer.h>
#include <clang/Frontend/CompilerInstance.h>

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
          return cexpr.fdeclId == el.func.fptr->getID();
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

static void filter_mapping(std::vector<FuncToJITEntry> &mapping) {

  for (auto &[func, callsites] : mapping) {
    for (auto &callsite : callsites) {
      if (callsite.declrefexpr->getNumTemplateArgs() > 0) {
      }
    }
  }
}

JITASTConsumer::JITASTConsumer(clang::CompilerInstance &CI,
                               clang::ASTContext &Context)
    : VisitorAttr{Context}, VisitorDecl{CI, Context}, VisitorCall{Context} {}

void JITASTConsumer::HandleTranslationUnit(clang::ASTContext &Context) {
  _jit::log_debug() << "Entering JITASTConsumer::HandleTranslationUnit.\n";

  // Pass 1: handle correctly tagging called functions in init body of varDecl
  // for JIT
  {
    _jit::log_debug() << "ENTERING VISITOR PASSES\n"
                      << "\tStarting pass 1...\n";
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

  // Pre-pass 3 : we construct the collection where we have for each FunctDecl
  // marked for JIT, all the callexpr corresponding.
  {
    auto mapping = construct_collection_for_JITengine();
    dump_mapping(mapping);
  }

  // Pass 3 : we filter-out function to JIT to those whose template parameters
  // are runtime values
  // We use our mapping so we do not need another visitor right now
  {}

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

clang::PluginASTAction::ActionType JITAttributeAction::getActionType() {
  return AddBeforeMainAction;
}
