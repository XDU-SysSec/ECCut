#ifndef Monitor_H_
#define Monitor_H_
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/Twine.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/PassSupport.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include "string.h"
#include <sstream>

using namespace llvm;


class Monitor : public ModulePass {
    public:
    static char ID;

    Monitor() : ModulePass(ID){};
    ~Monitor() {};
    
    
    bool runOnModule( llvm::Module &M);
    Value* EmitCastToVoidPtr(Value *value, Instruction *InsertPoint);
    unsigned long Ref_id(LoadInst *loadCalleePtr);
    void monitor(CallSite CS);
    
};

#endif
