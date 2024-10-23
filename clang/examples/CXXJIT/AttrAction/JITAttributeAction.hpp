#pragma once

#include <AttrAction/Visitors/JITAttrStmtDeclVisitor.hpp>
#include <AttrAction/Visitors/JITFunctionCallVisitor.hpp>
#include <AttrAction/Visitors/JITFunctionDeclVisitor.hpp>

#include <clang/Frontend/FrontendAction.h>

#include <memory>

/// Custom attribute handler for [[jit]]
class JITASTConsumer : public clang::ASTConsumer {
public:
  explicit JITASTConsumer(clang::CompilerInstance &CI,
                          clang::ASTContext &Context);

  /// HandleTopLevelDecl - Handle the specified top-level declaration.  This is
  /// called by the parser to process every top-level Decl*.
  ///
  /// \returns true to continue parsing, or false to abort parsing.
  bool HandleTopLevelDecl(clang::DeclGroupRef D) override;

  /// HandleInterestingDecl - Handle the specified interesting declaration. This
  /// is called by the AST reader when deserializing things that might interest
  /// the consumer. The default implementation forwards to HandleTopLevelDecl.
  void HandleInterestingDecl(clang::DeclGroupRef D) override;

  /// HandleTranslationUnit - This method is called when the ASTs for entire
  /// translation unit have been parsed.
  void HandleTranslationUnit(clang::ASTContext &Context) override;

  /// HandleTagDeclDefinition - This callback is invoked each time a TagDecl
  /// (e.g. struct, union, enum, class) is completed.  This allows the client to
  /// hack on the type, which can occur at any point in the file (because these
  /// can be defined in declspecs).
  // void HandleTagDeclDefinition(TagDecl *D) override {}

  /// This callback is invoked the first time each TagDecl is required to
  /// be complete.
  // void HandleTagDeclRequiredDefinition(const clang::TagDecl *D) override {}

  /// If the consumer is interested in entities getting modified after
  /// their initial creation, it should return a pointer to
  /// an ASTMutationListener here.
  clang::ASTMutationListener *GetASTMutationListener() override {
    return nullptr;
  }

private:
  JITAttrStmtDeclVisitor VisitorAttr;
  JITFunctionDeclVisitor VisitorDecl;
  JITFunctionCallVisitor VisitorCall;
};

class JITAttributeAction : public clang::PluginASTAction {
protected:
  // Register the custom attribute in the plugin action.
  bool ParseArgs(const clang::CompilerInstance &CI,
                 const std::vector<std::string> &args) override;

  /// Create the AST consumer object for this action, if supported.
  ///
  /// This routine is called as part of BeginSourceFile(), which will
  /// fail if the AST consumer cannot be created. This will not be called if the
  /// action has indicated that it only uses the preprocessor.
  ///
  /// \param CI - The current compiler instance, provided as a convenience, see
  /// getCompilerInstance().
  ///
  /// \param InFile - The current input file, provided as a convenience, see
  /// getCurrentFile().
  ///
  /// \return The new AST consumer, or null on failure.
  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI, llvm::StringRef) override;

  /// Prepare to execute the action on the given CompilerInstance.
  ///
  /// This is called before executing the action on any inputs, and can modify
  /// the configuration as needed (including adjusting the input list).
  bool PrepareToExecuteAction(clang::CompilerInstance &CI) override;

  /// Callback before starting processing a single input, giving the
  /// opportunity to modify the CompilerInvocation or do some other action
  /// before BeginSourceFileAction is called.
  ///
  /// \return True on success; on failure BeginSourceFileAction(),
  /// ExecuteAction() and EndSourceFileAction() will not be called.
  bool BeginInvocation(clang::CompilerInstance &CI) override;

  /// Callback at the start of processing a single input.
  ///
  /// \return True on success; on failure ExecutionAction() and
  /// EndSourceFileAction() will not be called.
  bool BeginSourceFileAction(clang::CompilerInstance &CI) override;

  PluginASTAction::ActionType getActionType() override;
};
