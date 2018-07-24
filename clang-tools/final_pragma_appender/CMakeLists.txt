set( LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  Option
  Support
  )

add_clang_executable(final_pragma_appender
  PragmaAppend.cpp
  )

target_link_libraries(final_pragma_appender
  PRIVATE
  clangAST
  clangBasic
  clangDriver
  clangFrontend
  clangRewriteFrontend
  clangStaticAnalyzerFrontend
  clangASTMatchers
  clangTooling
  )
