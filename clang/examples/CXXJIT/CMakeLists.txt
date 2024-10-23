add_llvm_library(CXXJITPlugin MODULE
  CXXJITPlugin.cpp
  JITRegistry.cpp
  helpers.cpp
  AttrMarking/JITAttrInfo.cpp
  AttrAction/JITAttributeAction.cpp
  AttrAction/DiagHandler/JITDiagConsumer.cpp
  AttrAction/Visitors/JITAttrStmtDeclVisitor.cpp
  AttrAction/Visitors/JITFunctionCallVisitor.cpp
  AttrAction/Visitors/JITFunctionDeclVisitor.cpp
  PLUGIN_TOOL clang)

target_include_directories(CXXJITPlugin PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

if(WIN32 OR CYGWIN)
  set(LLVM_LINK_COMPONENTS
    Support
    Core
    ExecutionEngine
    IRReader
    JITLink
    MC
    OrcJIT
    Passes
    Target
    nativecodegen
  )
  clang_target_link_libraries(CXXJITPlugin PRIVATE
    clangAST
    clangBasic
    clangFrontend
    clangLex
    clangSerialization
    clangTooling
    clangCodeGen
  )
endif()
