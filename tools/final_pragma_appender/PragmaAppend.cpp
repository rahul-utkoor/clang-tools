#include <cstdio>
#include <memory>
#include <sstream>
#include <string>
#include <cstdlib>
#include <system_error>

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"

#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/DiagnosticOptions.h"

#include "clang/Rewrite/Core/Rewriter.h"
#include "llvm/Option/OptTable.h"

#include "clang/Driver/Options.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"

#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/Core/Replacement.h"
#include "clang/Tooling/CommonOptionsParser.h"

using namespace std;
using namespace llvm;
using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

Rewriter rewriter;
std::map<std::string, bool> visited;

static cl::OptionCategory PragmaAppendCategory("clang-check options");
static std::unique_ptr<opt::OptTable> Options(createDriverOptTable());
static cl::opt<unsigned> UnrollCount(
    "unroll-count", cl::init(1), cl::Hidden,
    cl::desc("Add praga loop unroll with this unroll-count "),
    cl::cat(PragmaAppendCategory));

class PragmaAppendVisitor : public RecursiveASTVisitor<PragmaAppendVisitor> {
private:
    ASTContext *astContext;
    std::map< std::string, Replacements > *Replace;
public:
    explicit PragmaAppendVisitor(ASTContext *Context, std::map< std::string, Replacements > *Replace)
      : astContext(Context), Replace(Replace) // initialize private members
    {
        rewriter.setSourceMgr(astContext->getSourceManager(), astContext->getLangOpts());
    }

    virtual bool VisitStmt(Stmt *s) {
      if(isa<ForStmt>(s) && (visited.find(s->getLocStart().printToString(astContext->getSourceManager())) == visited.end())) {

		std::string SSBefore = "#pragma clang loop unroll_count(" + std::to_string(UnrollCount) + ")\n";
	    Replacement R(astContext->getSourceManager(), s->getLocStart(), 0, SSBefore);

        auto Err = (*Replace)[R.getFilePath()].add(R);
        if (Err)
          llvm_unreachable(llvm::toString(std::move(Err)).c_str());

      	visited[s->getLocStart().printToString(astContext->getSourceManager())] = true;

      }

      return true;
    }
};


class PragmaAppendASTConsumer : public clang::ASTConsumer {
private:
    PragmaAppendVisitor *visitor; // doesn't have to be private
    std::map< std::string, Replacements > *Replace;
public:
    PragmaAppendASTConsumer(std::map< std::string, Replacements > *Replace)
        : Replace(Replace) {}

    virtual void Initialize(ASTContext &Context) {
      visitor = new PragmaAppendVisitor(&Context , Replace);
    }

    virtual void HandleTranslationUnit(ASTContext &Context) {
        visitor->TraverseDecl(Context.getTranslationUnitDecl());
    }
};

class PragmaAppendClassAction {
  std::map<std::string, tooling::Replacements> *Replace;
public:
  PragmaAppendClassAction(std::map<std::string, tooling::Replacements> *Replace) : Replace(Replace) {};
  std::unique_ptr<ASTConsumer> newASTConsumer();
};

std::unique_ptr<ASTConsumer> PragmaAppendClassAction::newASTConsumer() {
  return llvm::make_unique<PragmaAppendASTConsumer>(Replace);
}

int main(int argc, const char **argv) {

  CommonOptionsParser OptionsParser(argc, argv, PragmaAppendCategory);
  RefactoringTool Tool(OptionsParser.getCompilations(),
               OptionsParser.getSourcePathList());

  PragmaAppendClassAction AppendClassAction(&Tool.getReplacements());

  auto Factory = newFrontendActionFactory(&AppendClassAction);
  int ExitCode = Tool.runAndSave(Factory.get());

  LangOptions DefaultLangOptions;
  IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts(new DiagnosticOptions());
  TextDiagnosticPrinter DiagnosticPrinter(errs(), &*DiagOpts);
  DiagnosticsEngine Diagnostics(
      IntrusiveRefCntPtr<DiagnosticIDs>(new DiagnosticIDs()), &*DiagOpts,
      &DiagnosticPrinter, false);

  auto &FileMgr = Tool.getFiles();
  SourceManager Sources(Diagnostics, FileMgr);
  Rewriter Rewrite(Sources, DefaultLangOptions);
  Tool.applyAllReplacements(Rewrite);

  for (const auto &File : OptionsParser.getSourcePathList()) {
    const auto *Entry = FileMgr.getFile(File);
    const auto ID = Sources.getOrCreateFileID(Entry, SrcMgr::C_User);
    // Rewrite.getEditBuffer(ID).write(outs());
  }

  return ExitCode;
}

