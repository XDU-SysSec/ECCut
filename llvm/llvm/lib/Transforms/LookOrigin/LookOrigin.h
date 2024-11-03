
#ifndef LookOrigin_H_
#define LookOrigin_H_

#include "string.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/Twine.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/IRBuilder.h"
#include <sstream>

using namespace llvm;


typedef std::map<unsigned long, llvm::Instruction *> mapIcallInstNum;
typedef std::map<unsigned long, llvm::Instruction *>::iterator mapIcallInstNumIt;
typedef std::set<llvm::CallInst *> UpdateCallSet;
typedef std::set<llvm::CallInst *>::iterator UpdateCallSetIt;
typedef std::set<unsigned long > IcallIDset;

typedef std::map<unsigned long, UpdateCallSet> IcallToUpdate;
typedef std::map<unsigned long, UpdateCallSet>::iterator IcallToUpdateIt;

extern unsigned long indirect_num;


class LookOrigin : public llvm::ModulePass {
  private:
    
  UpdateCallSet updatecallset;

  public:
    static char ID;

    LookOrigin() : ModulePass(ID){};
    ~LookOrigin() {};
    
    bool runOnModule( llvm::Module &M) ;
    void mapIcall(llvm::Module &M);
    void LookForUpdate(llvm::Module &M);
    void mapIcallToUpdate(llvm::CallInst *update_call);
    void printmap();
    
};

#endif
