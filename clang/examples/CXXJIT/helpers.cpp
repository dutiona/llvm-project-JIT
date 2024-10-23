#include <helpers.hpp>

#include <JITRegistry.hpp>

#include <clang/AST/ASTContext.h>
#include <clang/AST/DeclTemplate.h>
#include <clang/AST/ExprCXX.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

namespace _jit {

llvm::Type *translateClangTypeToLLVM(clang::QualType QT,
                                     llvm::LLVMContext &Context) {
  // This function needs a full implementation that covers all the cases you
  // expect to handle. This is a placeholder for simple types.
  if (QT->isIntegerType()) {
    return llvm::Type::getInt32Ty(
        Context); // Simplified: assume all integers are 32-bit
  }
  // Add other type translations as necessary
  return nullptr;
}

llvm::FunctionType *translateFunctionType(clang::FunctionDecl *FD,
                                          llvm::LLVMContext &Context) {
  std::vector<llvm::Type *> ParamTypes;

  // Translate parameter types
  for (auto *param : FD->parameters()) {
    llvm::Type *ParamType = translateClangTypeToLLVM(param->getType(), Context);
    if (!ParamType)
      return nullptr; // Translation failed
    ParamTypes.push_back(ParamType);
  }

  // Translate return type
  llvm::Type *ReturnType =
      translateClangTypeToLLVM(FD->getReturnType(), Context);
  if (!ReturnType)
    return nullptr; // Translation failed

  return llvm::FunctionType::get(ReturnType, ParamTypes, FD->isVariadic());
}

llvm::Function *createLogFunction(llvm::Module *Module,
                                  llvm::IRBuilder<> &Builder) {
  // Get or create the printf function declaration
  llvm::FunctionType *printfType = llvm::FunctionType::get(
      Builder.getInt32Ty(), {Builder.getInt8Ty()}, true);
  llvm::FunctionCallee printfFunc =
      Module->getOrInsertFunction("printf", printfType);

  // Define the logMessage function
  llvm::FunctionType *logFuncType =
      llvm::FunctionType::get(Builder.getVoidTy(), false);
  llvm::Function *logFunc = llvm::Function::Create(
      logFuncType, llvm::Function::InternalLinkage, "logMessage", Module);

  // Create a basic block for the logMessage function
  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(Builder.getContext(), "Entry", logFunc);
  Builder.SetInsertPoint(BB);

  // Create the message to be printed
  llvm::Value *message = Builder.CreateGlobalStringPtr("I am JITTed\n");

  // Call printf to print the message
  Builder.CreateCall(printfFunc, {message});

  // Return from the function
  Builder.CreateRetVoid();

  return logFunc;
}

llvm::raw_ostream &log_error(const std::source_location &scl) {
  llvm::errs() << "[ERROR][JIT PLUGIN] " << scl.file_name() << '(' << scl.line()
               << ':' << scl.column() << ") `" << scl.function_name()
               << "`: \n\t\t";
  return llvm::errs();
}

llvm::raw_ostream &log_debug(const std::source_location &scl) {
  llvm::outs() << "[DEBUG][JIT PLUGIN] " << scl.file_name() << '(' << scl.line()
               << ':' << scl.column() << ") `" << scl.function_name()
               << "`: \n\t\t";
  return llvm::outs();
}

llvm::raw_ostream &pretty_print_decl(const clang::Decl *decl,
                                     llvm::raw_ostream &os) {
  if (decl) {
    os << "<<<STR DeclKindName<" << decl->getDeclKindName() << ">\n";
    decl->print(os);
  } else {
    os << "nullptr";
  }
  os << "\n>>>\n";

  return os;
}

llvm::raw_ostream &pretty_print_stmt(const clang::Stmt *stmt,
                                     llvm::raw_ostream &os) {
  if (stmt) {
    os << "<<<STR StmtType<" << stmt->getStmtClassName() << ">\n";
    auto LO = clang::LangOptions();
    LO.CPlusPlus = true;
    LO.Bool = true;
    stmt->printPretty(os, nullptr, clang::PrintingPolicy(LO));
  } else {
    os << "nullptr";
  }
  os << "\n>>>\n";

  return os;
}

llvm::raw_ostream &pretty_print_stmt_json(const clang::Stmt *stmt,
                                          llvm::raw_ostream &os) {
  if (stmt) {
    os << "<<<JSON StmtType<" << stmt->getStmtClassName() << ">\n";
    auto LO = clang::LangOptions();
    LO.CPlusPlus = true;
    LO.Bool = true;
    stmt->printJson(os, nullptr, clang::PrintingPolicy(LO), true);
  } else {
    os << "{NULL}";
  }
  os << "\n>>>\n";

  return os;
}

void dump_registry() {
  // Inspect what we have in FunctionsMarkedToJIT and CallerExprsMarkedToJIT
  auto &os = _jit::log_debug() << "Listing FunctionsMarkedToJIT:\n";
  for (const auto &[k, v] : _jit::FunctionsMarkedToJIT()) {
    os << "\tFID<" << k << ">: " << v.name << "\n";
  }

  os << "Listing CallerExprsMarkedToJIT:\n";
  for (const auto &[k, v] : _jit::CallerExprsMarkedToJIT()) {
    os << "\tCallExprID<" << k << ">: pointing to func FID<" << v.fdeclID
       << "> (" << v.fname << ")\n";
  }
}

void instrumentFunction(llvm::Function *Function, llvm::IRBuilder<> &Builder) {
  // Assuming we have a logging function declared somewhere in the module

  Builder.SetInsertPoint(&Function->getEntryBlock().front());

  // Create the log function and add a call to it at the start of `yourFunction`
  llvm::Function *logFunc = createLogFunction(Function->getParent(), Builder);

  if (!logFunc) {
    // Create a prototype for the log function (e.g., void logMessage(const
    // char*))
    llvm::FunctionType *logFuncType = llvm::FunctionType::get(
        Builder.getVoidTy(), {Builder.getInt8Ty()}, false);
    logFunc =
        llvm::Function::Create(logFuncType, llvm::Function::ExternalLinkage,
                               "logMessage", Function->getParent());
  }

  Builder.CreateCall(logFunc);

  // Instrument the entry of the function
  llvm::BasicBlock &EntryBlock = Function->getEntryBlock();
  Builder.SetInsertPoint(&EntryBlock, EntryBlock.getFirstInsertionPt());
  Builder.CreateCall(logFunc,
                     {Builder.CreateGlobalStringPtr("Entering function")});

  // Instrument before each return instruction
  for (auto &BB : *Function) {
    for (auto &Inst : BB) {
      if (llvm::ReturnInst *retInst = llvm::dyn_cast<llvm::ReturnInst>(&Inst)) {
        Builder.SetInsertPoint(retInst);
        Builder.CreateCall(logFunc,
                           {Builder.CreateGlobalStringPtr("Exiting function")});
      }
    }
  }
}

const clang::FunctionDecl *
extract_originated_fdecl_if_any(const clang::Decl *decl) {
  if (decl) {
    // get the function Decl from the call
    if (auto *fdecl = decl->getAsFunction(); fdecl) {
      // if it is a call to a function template, we dig and retreive the
      // FuncDecl from which it is from
      if (auto *ftspecinfo = fdecl->getTemplateSpecializationInfo();
          ftspecinfo) {
        // we retrieve the FTDecl it is originated from,
        if (auto *ftdecl = ftspecinfo->getTemplate(); ftdecl) {
          // we cast it as a FuncDecl as it is what we store in the registry
          return ftdecl->getAsFunction();
        }
      }
    }
  }

  return nullptr;
}

static void extract_templated_declrefexpr_from_funcdecl(
    const clang::CallExpr *parent_callexpr, const clang::FunctionDecl *fdecl,
    std::vector<CallSiteInfo> &templated_callexprs_to_jit);

static void extract_templated_callexpr_to_jit_in_stmt_impl(
    const clang::Stmt *stmt,
    std::vector<CallSiteInfo> &templated_callexprs_to_jit);

static void handle_nested_calls_if_needed(
    const clang::CallExpr *parent_callexpr, const clang::FunctionDecl *fdecl,
    const clang::Stmt *child,
    std::vector<CallSiteInfo> &templated_callexprs_to_jit);

static void handle_implicit_cast_operator(
    const clang::CallExpr *parent_callexpr, const clang::FunctionDecl *fdecl,
    const clang::ImplicitCastExpr *implcastexpr,
    std::vector<CallSiteInfo> &templated_callexprs_to_jit);

void handle_nested_calls_if_needed(
    const clang::CallExpr *parent_callexpr, const clang::FunctionDecl *fdecl,
    const clang::Stmt *child,
    std::vector<CallSiteInfo> &templated_callexprs_to_jit) {
  if (auto *declrefexr = llvm::dyn_cast<clang::DeclRefExpr>(child)) {
    if (declrefexr->hasTemplateKWAndArgsInfo()) {
      auto *FD = _jit::extract_originated_fdecl_if_any(fdecl);
      templated_callexprs_to_jit.push_back({FD, parent_callexpr, declrefexr});
      auto &os = _jit::log_debug()
                 << "Adding <" << declrefexr->getNameInfo() << "/"
                 << FD->getID() << "> in function list to JIT."
                 << " Parent callexpr:\n";
      _jit::pretty_print_stmt(parent_callexpr, os);
    }

    // handle nested function call
  } else if (auto *callexpr = llvm::dyn_cast<clang::CallExpr>(child)) {
    // crossed recursion
    extract_templated_callexpr_to_jit_in_stmt_impl(callexpr,
                                                   templated_callexprs_to_jit);
  }
}

void handle_implicit_cast_operator(
    const clang::CallExpr *parent_callexpr, const clang::FunctionDecl *fdecl,
    const clang::ImplicitCastExpr *implcastexpr,
    std::vector<CallSiteInfo> &templated_callexprs_to_jit) {
  for (auto *child : implcastexpr->children()) {
    handle_nested_calls_if_needed(parent_callexpr, fdecl, child,
                                  templated_callexprs_to_jit);
  }
}

void extract_templated_declrefexpr_from_funcdecl(
    const clang::CallExpr *parent_callexpr, const clang::FunctionDecl *fdecl,
    std::vector<CallSiteInfo> &templated_callexprs_to_jit) {
  if (fdecl && parent_callexpr) {
    for (auto *child : parent_callexpr->children()) {

      auto *implcastexpr = llvm::dyn_cast<clang::ImplicitCastExpr>(child);
      if (implcastexpr) {
        handle_implicit_cast_operator(parent_callexpr, fdecl, implcastexpr,
                                      templated_callexprs_to_jit);
      } else {
        handle_nested_calls_if_needed(parent_callexpr, fdecl, child,
                                      templated_callexprs_to_jit);
      }
    }
  }
}

void extract_templated_callexpr_to_jit_in_stmt_impl(
    const clang::Stmt *stmt,
    std::vector<CallSiteInfo> &templated_callexprs_to_jit) {

  if (!stmt)
    return;

  auto *callexpr = llvm::dyn_cast<clang::CallExpr>(stmt);

  if (callexpr) {
    auto *decl = callexpr->getCalleeDecl();
    if (decl) {
      if (decl->isFunctionOrFunctionTemplate()) {
        auto *fdecl = llvm::dyn_cast<clang::FunctionDecl>(decl);
        extract_templated_declrefexpr_from_funcdecl(callexpr, fdecl,
                                                    templated_callexprs_to_jit);
      }
    }
  } else {
    for (auto *child : stmt->children()) {
      extract_templated_callexpr_to_jit_in_stmt_impl(
          child, templated_callexprs_to_jit);
    }
  }
}

std::vector<CallSiteInfo>
extract_templated_callexpr_to_jit_in_stmt(const clang::Stmt *stmt) {
  if (!stmt)
    return {};

  auto templated_callexprs_to_jit = std::vector<CallSiteInfo>{};
  extract_templated_callexpr_to_jit_in_stmt_impl(stmt,
                                                 templated_callexprs_to_jit);

  return templated_callexprs_to_jit;
}

/*
std::ostringstream oss;
oss << "Diagnostic: " << "isFunctionOrFunctionTemplate<"
    << (D->isFunctionOrFunctionTemplate() ? "true" : "false") << "> "
    << "isTemplateDecl<" << (D->isTemplateDecl() ? "true" : "false") << "> "
    << "isTemplated<" << (D->isTemplated() ? "true" : "false") << "> "
    << "isTemplateParameter<"
    << (D->isTemplateParameter() ? "true" : "false") << "> "
    << "getTemplateDepth<" << (D->getTemplateDepth()) << "> ";
*/
// S.Diag(Attr.getLoc(), diag::warn_attribute_type_not_supported)
//     << Attr << oss.str();

/*
auto *FTDecl = cast<clang::FunctionTemplateDecl>(D);
clang::FunctionDecl *FD = FTDecl->getTemplatedDecl();

// Generate LLVM IR from the function's AST
auto Context = std::make_unique<llvm::LLVMContext>();
auto CI = std::make_unique<clang::CompilerInstance>();

auto CG = std::unique_ptr<clang::CodeGenerator>(CreateLLVMCodeGen(
CI->getDiagnostics(), "main-module", &CI->getVirtualFileSystem(),
CI->getHeaderSearchOpts(), CI->getPreprocessorOpts(),
CI->getCodeGenOpts(), *Context));

auto *Module = CG->StartModule("main-module", *Context);

clang::CodeGen::CodeGenModule &CGM = CG->CGM();

llvm::FunctionType *FT = translateFunctionType(FD, *Context);
llvm::Function *LLVMFunc = llvm::Function::Create(
FT, llvm::Function::ExternalLinkage, FD->getNameAsString(), Module);

// Add a basic block to the function. As before, it automatically inserts
// because of the last argument.
llvm::BasicBlock *BB =
llvm::BasicBlock::Create(*Context, "EntryBlock", LLVMFunc);

// BB->

// Setup LLVM's OrcJIT
llvm::IRBuilder<> Builder(BB);

instrumentFunction(LLVMFunc, Builder);

// Create an LLJIT instance.
llvm::ExitOnError ExitOnErr;
auto JIT = ExitOnErr(llvm::orc::LLJITBuilder().create());

if (!JIT) {
S.Diag(Attr.getLoc(), clang::diag::err_fe_unable_to_load_plugin);
return AttributeNotApplied;
}

// Add the module containing the function to the JIT
auto err = JIT->addIRModule(llvm::orc::ThreadSafeModule{
std::unique_ptr<llvm::Module>{CG->ReleaseModule()},
std::move(Context)});

auto Add1Addr = ExitOnErr(JIT->lookup(FD->getNameAsString()));
void (*test)(void) = Add1Addr.toPtr<void(void)>();

test();
llvm::outs() << "Called test() function\n";

if (!err.success()) {
S.Diag(Attr.getLoc(), clang::diag::err_fe_backend_error_attr)
<< Attr << Attr.isRegularKeywordAttribute()
<< (std::string{"Could not load JIT IR facilities"} +
llvm::toString(std::move(err)));
return AttributeNotApplied;
}

// Replace the function in the module with a stub that invokes the
// JIT-compiled version
// replaceFunctionWithJITStub(S, FD, LLVMFunc, JIT.get());
return AttributeApplied;
*/

// Helper function to generate the rewritten non-template function.
clang::FunctionDecl *createJitFunctionDecl(clang::ASTContext &Context,
                                           clang::FunctionDecl *FDecl,
                                           clang::SourceManager &SM,
                                           clang::DeclContext *DC) {

  using namespace std::string_literals;

  if (!FDecl->isTemplated()) {
    llvm::outs() << "[DEBUG][JIT PLUGIN] <" << FDecl->getNameAsString()
                 << "> is not a function template... Ignoring...\n";
    return nullptr;
  }

  // Create a new function name: __jit_test.
  std::string JitFuncName = "__jit_"s + FDecl->getNameAsString();
  auto *FTDecl = FDecl->getDescribedFunctionTemplate();
  //
  clang::IdentifierInfo &Id = Context.Idents.get(JitFuncName);
  clang::DeclarationName DeclName = Context.DeclarationNames.getIdentifier(&Id);

  // Create a parameter for each template argument (e.g., int I becomes int i).
  std::vector<clang::ParmVarDecl *> Params;
  std::vector<clang::QualType> ParamsQualTypes;

  auto *TemplateParams = FTDecl->getTemplateParameters();
  if (TemplateParams && TemplateParams->size() > 0) {
    llvm::outs() << "[DEBUG][JIT PLUGIN] template parameters nb = "
                 << TemplateParams->size() << "\n";

    unsigned i = 0;
    for (auto *TemplateParam : TemplateParams->asArray()) {
      if (TemplateParam) {

        const auto *NTTP =
            llvm::dyn_cast<clang::NonTypeTemplateParmDecl>(TemplateParam);
        if (NTTP) {
          llvm::outs()
              << "[DEBUG][JIT PLUGIN] Non-typed template parameter supported = "
              << TemplateParam->getNameAsString() << " <"
              << TemplateParam->getID() << "> processed" << "\n";
          clang::QualType ParamType = NTTP->getType();
          clang::IdentifierInfo &ParamId = Context.Idents.get(
              JitFuncName + "_param_" + TemplateParam->getNameAsString() + "_" +
              std::to_string(i));
          clang::ParmVarDecl *NewParam = clang::ParmVarDecl::Create(
              Context, DC, clang::SourceLocation(), clang::SourceLocation(),
              &ParamId, ParamType, nullptr, clang::SC_None, nullptr);

          llvm::outs() << "[DEBUG][JIT PLUGIN] Param "
                       << NewParam->getNameAsString() << " pushed.\n";

          Params.push_back(NewParam);
          ParamsQualTypes.push_back(NewParam->getOriginalType());
        } else {
          llvm::outs() << "[DEBUG][JIT PLUGIN] Typed template parameter not "
                          "supported yet "
                       << TemplateParam->getNameAsString() << " <"
                       << TemplateParam->getID() << ">" << " ignored\n";
        }
      }
      ++i;
    }

    // forward original parameters
    for (int i = 0, nb_params = FDecl->getNumParams(); i < nb_params; ++i) {
      auto *param = FDecl->getParamDecl(i);
      Params.push_back(param);
      ParamsQualTypes.push_back(param->getOriginalType());

      llvm::outs() << "[DEBUG][JIT PLUGIN] Forwarding original param "
                   << param->getNameAsString() << ".\n";
    }

    clang::QualType ReturnType = FDecl->getReturnType();
    clang::FunctionProtoType::ExtProtoInfo EPI;
    auto ParamsQualTypes_ArrRef = clang::SmallVector<clang::QualType>{
        ParamsQualTypes.begin(), ParamsQualTypes.end()};
    clang::QualType FuncType =
        Context.getFunctionType(ReturnType, ParamsQualTypes_ArrRef, EPI);

    clang::FunctionDecl *NewFunc = clang::FunctionDecl::Create(
        Context, DC, clang::SourceLocation(), clang::SourceLocation(), DeclName,
        FuncType, nullptr, clang::SC_None, clang::SC_None);

    NewFunc->setParams(Params);

    return NewFunc;
  }

  return nullptr;
}

clang::Stmt *createDummyFunctionBody(clang::ASTContext &Context,
                                     clang::FunctionDecl *FDecl) {
  using namespace std::string_literals;

  // get the reference to the simple_print top level function which is a wrapper
  // around std::cout
  auto &print = Context.Idents.get("simple_print");
  auto *print_decl = Context.getTranslationUnitDecl()
                         ->lookup(clang::DeclarationName(&print))
                         .front();

  if (!print_decl) {
    llvm::outs() << "[DEBUG][JIT PLUGIN] Nope1!! \n";
    return nullptr;
  }

  auto *FDecl_print = llvm::dyn_cast<clang::FunctionDecl>(print_decl);

  if (!FDecl_print) {
    llvm::outs() << "[DEBUG][JIT PLUGIN] Nope2!! \n";
    return nullptr;
  }

  llvm::outs() << "[DEBUG][JIT PLUGIN] [createDummyFunctionBody] Using "
               << FDecl_print->getNameAsString() << " to print info\n";

  // step 1 create the call operator attached to the JIT function decl
  clang::DeclRefExpr *callOperatorDeclRef = clang::DeclRefExpr::Create(
      /* Ctx =*/Context,
      /* QualifierLoc =*/clang::NestedNameSpecifierLoc(),
      /* TemplateKWLoc =*/clang::SourceLocation(),
      /* TODO/FIXME const_cast<FunctionDecl *>(callOperatorDecl)*/ FDecl_print,
      /* RefersToEnclosingVariableOrCapture=*/false,
      /* NameLoc =*/clang::SourceLocation(),
      /* T = TODO:FIXME callOperatorDecl->getType()*/ FDecl_print->getType(),
      /* VK =*/clang::VK_LValue);

  // Step 2: Create a string literal expression that is going to be output
  clang::StringLiteral *StrLitFuncName = clang::StringLiteral::Create(
      Context, "JITting..."s + FDecl->getNameAsString(),
      clang::StringLiteralKind::Ordinary, false, Context.CharTy,
      clang::SourceLocation());

  auto CallArgs = clang::SmallVector<clang::Expr *>{StrLitFuncName};

  auto *OpCallExpr = clang::CXXOperatorCallExpr::Create(
      /*AstContext=*/Context, clang::OO_Call, callOperatorDeclRef,
      /*Args=*/CallArgs,
      /*QualType=*/Context.VoidTy,
      /*ExprValueType=*/clang::VK_PRValue,
      /*SourceLocation=*/clang::SourceLocation(),
      /*FPFeatures=*/clang::FPOptionsOverride());

  // Step 5: Create the compound statement (the function body)
  std::vector<clang::Stmt *> Stmts;
  Stmts.push_back(OpCallExpr); // Add 'simple_print(functionName);'

  return clang::CompoundStmt::Create(Context, Stmts, clang::FPOptionsOverride(),
                                     clang::SourceLocation(),
                                     clang::SourceLocation());
}

} // namespace _jit