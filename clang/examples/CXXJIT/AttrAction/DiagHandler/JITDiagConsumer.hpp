#include <clang/Basic/Diagnostic.h>
#include <clang/Basic/DiagnosticOptions.h>
#include <llvm/Support/raw_ostream.h>

class JITDiagnosticConsumer : public clang::DiagnosticConsumer {
public:
  JITDiagnosticConsumer() {}

  // Handle each diagnostic message (errors, warnings, etc.)
  void HandleDiagnostic(clang::DiagnosticsEngine::Level DiagLevel,
                        const clang::Diagnostic &Info) override;
};
