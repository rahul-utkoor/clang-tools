set( LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  Option
  Support
  )

add_clang_executable(print-loop
  loopPrinter.cpp
  )

target_link_libraries(print-loop
  PRIVATE
  clangBasic
  clangASTMatchers
  clangTooling
  )
