#include <AttrAction/JITAttributeAction.hpp>
#include <AttrMarking/JITAttrInfo.hpp>

#include <clang/Basic/ParsedAttrInfo.h>
#include <clang/Frontend/FrontendPluginRegistry.h>

// Will parse [[jit]] attribute on funcDecl and varDecl
// Will handle correctly tagging funcDecl for JIT
// FIXME does not support tagging a statement with an attribute injected by a
// plugin (yet?)
static clang::ParsedAttrInfoRegistry::Add<_jit::JITAttrInfo>
    X1("cxxjit-attr-plugin", "Enable [[jit]] parsing for function template.");

// Will handle correctly tagging called functions in init body of varDecl for
// JIT
// Will handle filtering Callers/Callees (we only JIT those which have an input
// template parameter known at runtime)
// Will finally handle the JIT engine and replacing callers and callee
// accordingly
static clang::FrontendPluginRegistry::Add<::JITAttributeAction>
    X2("cxxjit-ast-plugin", "Enable JITing function template.");
