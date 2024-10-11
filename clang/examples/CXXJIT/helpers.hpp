#pragma once

#include <clang/AST/Decl.h>
#include <clang/AST/Expr.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/raw_ostream.h>

#ifdef __has_include
#if __has_include(<source_location>)
#include <source_location>
#else
#error "Missing <source_location>"
#endif
#endif

#ifndef __cpp_lib_source_location
#error "No std::source_location! DAYUM!"
#endif

namespace _jit {

llvm::raw_ostream &
log_error(const std::source_location &scl = std::source_location::current());
llvm::raw_ostream &
log_debug(const std::source_location &scl = std::source_location::current());

void instrumentFunction(llvm::Function *Function, llvm::IRBuilder<> &Builder);

[[maybe_unused]] llvm::raw_ostream &pretty_print_decl(const clang::Decl *decl,
                                                      llvm::raw_ostream &os);

[[maybe_unused]] llvm::raw_ostream &pretty_print_stmt(const clang::Stmt *stmt,
                                                      llvm::raw_ostream &os);

[[maybe_unused]] llvm::raw_ostream &
pretty_print_stmt_json(const clang::Stmt *stmt, llvm::raw_ostream &os);

void dump_registry();

const clang::FunctionDecl *
extract_originated_fdecl_if_any(const clang::Decl *decl);

struct CallSiteInfo {
  const clang::FunctionDecl *fdecl;
  const clang::CallExpr *callexpr;
  const clang::DeclRefExpr *declrefexpr;
};

std::vector<CallSiteInfo>
extract_templated_callexpr_to_jit_in_stmt(const clang::Stmt *stmt);

llvm::Type *translateClangTypeToLLVM(clang::QualType QT,
                                     llvm::LLVMContext &Context);

llvm::Function *createLogFunction(llvm::Module *Module,
                                  llvm::IRBuilder<> &Builder);
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
                                           clang::DeclContext *DC);

clang::Stmt *createDummyFunctionBody(clang::ASTContext &Context,
                                     clang::FunctionDecl *FDecl);
} // namespace _jit
