#include <AttrAction/DiagHandler/JITDiagConsumer.hpp>

#include <helpers.hpp>

#include <clang/Basic/Diagnostic.h>
#include <clang/Basic/DiagnosticOptions.h>
#include <clang/Basic/DiagnosticSema.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/Support/raw_ostream.h>

// Handle each diagnostic message (errors, warnings, etc.)
void JITDiagnosticConsumer::HandleDiagnostic(
    clang::DiagnosticsEngine::Level DiagLevel, const clang::Diagnostic &Info) {
  llvm::SmallString<100> DiagMessage;
  Info.FormatDiagnostic(DiagMessage);

  // Log the diagnostic message (for debugging purposes)
  auto &os =
      _jit::log_debug()
      << "[JITDiagnosticConsumer::HandleDiagnostic] Original Diagnostic: "
      << DiagMessage << "\n";

  // Example: Intercept specific error
  if (DiagLevel == clang::DiagnosticsEngine::Error) {
    unsigned DiagID = Info.getID();

    // Check if this is the error related to invalid template instantiation
    // (you may need to replace `diag::err_template_arg_not_constant` with the
    // correct ID)
    if (DiagID == clang::diag::note_ovl_candidate_explicit_arg_mismatch_named) {
      os << "Intercepted error: "
            "note_ovl_candidate_explicit_arg_mismatch_named\n";
      return;
    }

    if (DiagID ==
        clang::diag::note_ovl_candidate_explicit_arg_mismatch_unnamed) {
      os << "Intercepted error: "
            "note_ovl_candidate_explicit_arg_mismatch_unnamed\n";
      return;
    }

    if (DiagID == clang::diag::err_ovl_no_viable_function_in_call) {
      os << "Intercepted error: "
            "err_ovl_no_viable_function_in_call\n";
      return;
    }

    if (DiagID == clang::diag::err_ovl_no_viable_object_call) {
      os << "Intercepted error: "
            "err_ovl_no_viable_object_call\n";
      return;
    }

    if (DiagID == clang::diag::err_template_arg_not_valid_template) {
      os << "Intercepted error: "
            "err_template_arg_not_valid_template\n";
      return;
    }

    if (DiagID == clang::diag::err_template_arg_must_be_expr) {
      os << "Intercepted error: "
            "err_template_arg_must_be_expr\n";
      return;
    }

    if (DiagID == clang::diag::err_template_arg_not_decl_ref) {
      os << "Intercepted error: "
            "err_template_arg_not_decl_ref\n";
      return;
    }

    if (DiagID == clang::diag::err_template_arg_not_integral_or_enumeral) {
      os << "Intercepted error: "
            "err_template_arg_not_integral_or_enumeral\n";
      return;
    }

    if (DiagID == clang::diag::err_template_arg_must_be_expr) {
      os << "Intercepted error: "
            "err_template_arg_must_be_expr\n";
      return;
    }
  }

  // Call the base class implementation for other diagnostics
  clang::DiagnosticConsumer::HandleDiagnostic(DiagLevel, Info);
}