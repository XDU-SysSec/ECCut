#include "llvm/IR/CallSite.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include <llvm/Bitcode/BitcodeReader.h> /// for isBitcode
#include <llvm/IRReader/IRReader.h>     /// for isIRFile

#include <fstream>
#include <map>
#include <set>
#include <vector>

using namespace llvm;

static cl::opt<std::string> dirPath("DIR_PATH",
                                    cl::desc("give the program path directory"),
                                    cl::value_desc("directory path"));

typedef std::vector<unsigned long> contextList;
typedef std::vector<unsigned long>::iterator contextListIt;
typedef std::pair<unsigned long, contextList> ctxToTargetPair;
typedef std::set<ctxToTargetPair> ctxToTargetSet;
typedef std::set<ctxToTargetPair>::iterator ctxToTargetSetIt;
typedef std::map<unsigned long, ctxToTargetSet> pointToECMap;
typedef std::map<unsigned long, ctxToTargetSet>::iterator pointToECMapIt;
typedef std::map<unsigned long, int> pointToType;
typedef std::map<unsigned long, int>::iterator pointToTypeIt;

typedef enum TARGET_TYPE {
  OS_CTX = 1,
  OS = 2,
  CI = 3,
} targetType;

class INSTCFG : public ModulePass {
private:
  void replaceGLBUsage(GlobalVariable *New, GlobalVariable *Old) {
    std::set<User *> users;
    for (Value::user_iterator u = Old->user_begin(); u != Old->user_end();
         ++u) {
      User *user = *u;
      users.insert(user);
    }
    for (std::set<User *>::iterator u = users.begin(); u != users.end(); ++u) {
      User *user = *u;
      if (isa<GetElementPtrInst>(user)) {
        GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(user);
        gep->setSourceElementType(New->getValueType());
        for (unsigned I = 0, E = user->getNumOperands(); I < E; ++I) {
          if (user->getOperand(I) == Old) {
            user->setOperand(I, New);
          }
        }
      }
    }
    New->setSection(Old->getSection());
  }
public:
  static char ID;
  INSTCFG() : ModulePass(ID) {
    unsigned long p, t, s1, s2;
    std::ifstream initcallfd;
    std::string path;
    if (dirPath[0] == '~') {
      dirPath.replace(0, 1, std::getenv("HOME"));
    }

    // path = dirPath + "/ciCFG.bin";
    path = "ciCFG.bin";
    initcallfd.open(path.c_str());
    if (initcallfd.is_open()) {
      while (initcallfd >> p >> t) {
        contextList ctx;
        ctxToTargetPair target;
        target = std::make_pair(t, ctx);
        mapPEC[p].insert(target);
        mapPD[p] = CI;
      }
    }
    initcallfd.close();

    // path = dirPath + "/osCFG.bin";
    path = "osCFG.bin";
    initcallfd.open(path.c_str());
    if (initcallfd.is_open()) {
      while (initcallfd >> p >> t >> s1 >> s2) {
        contextList ctx;
        ctx.push_back(s1);
        ctx.push_back(s2);
        originList.push_back(s1);
        ctxToTargetPair target;
        target = std::make_pair(t, ctx);

        mapPEC[p].insert(target);
        if(s2 != 0) 
          mapPD[p] = OS_CTX;
        else if(s2 == 0)
          mapPD[p] = OS;
      }
    }
    initcallfd.close();
  }

  bool runOnModule(Module &M) override {
    PointerType *int32PtTy = Type::getInt32PtrTy(M.getContext());
    IntegerType *int32Ty = Type::getInt32Ty(M.getContext());
    IntegerType *int64Ty = Type::getInt64Ty(M.getContext());
    std::vector<Constant *> list_OS,list_CI;

    for (pointToECMapIt pIt = mapPEC.begin(); pIt != mapPEC.end(); ++pIt) {
      Constant *cPoint_t = ConstantInt::get(int32Ty, pIt->first, false);
      Constant *cPoint = ConstantFolder().CreateIntToPtr(cPoint_t, int32PtTy);

      for (ctxToTargetSetIt tIt = pIt->second.begin(); tIt != pIt->second.end();
            ++tIt) {
        Constant *cTarget_t = ConstantInt::get(int32Ty, tIt->first, false);
        Constant *cTarget =
            ConstantFolder().CreateIntToPtr(cTarget_t, int32PtTy);
        
        if (mapPD[pIt->first] == CI) {
          list_CI.push_back(cPoint);
          list_CI.push_back(cTarget);
        } else if (mapPD[pIt->first] == OS) {
          list_OS.push_back(cPoint);
          list_OS.push_back(cTarget);
          for (auto cIt = (tIt->second).begin(); cIt != (tIt->second).end();
                  ++cIt) {
              Constant *cContext_t = ConstantInt::get(int32Ty, *cIt, false);
              Constant *cContext =
                  ConstantFolder().CreateIntToPtr(cContext_t, int32PtTy);
              list_OS.push_back(cContext);
          }
        }
      }
    }
      

    // CI
    ArrayRef<Constant *> blockArrayPCI(list_CI);
    // create the constant type and array
    ArrayType *pArrTyPCI = ArrayType::get(int32PtTy, list_CI.size());
    Constant *blockItemsPCI = ConstantArray::get(pArrTyPCI, blockArrayPCI);

    GlobalVariable *gCFG_oldPCI = M.getGlobalVariable("STATIC_TABLE");
    gCFG_oldPCI->setDSOLocal(false);

    GlobalVariable *gvar_cfg_dataPCI = new GlobalVariable(
        M, blockItemsPCI->getType(), true, GlobalValue::ExternalLinkage,
        blockItemsPCI, "STATIC_TABLE");

    replaceGLBUsage(gvar_cfg_dataPCI, gCFG_oldPCI);

    Constant *cfgLenPCI = ConstantInt::get(int32Ty, list_CI.size(), false);
    GlobalVariable *gCFG_lenPCI = M.getGlobalVariable("STATIC_TABLE_LENGTH");
    gCFG_lenPCI->setInitializer(cfgLenPCI);

    // OS
    ArrayRef<Constant *> blockArrayPOS(list_OS);
    ArrayType *pArrTyPOS = ArrayType::get(int32PtTy, list_OS.size());
    Constant *blockItemsPOS = ConstantArray::get(pArrTyPOS, blockArrayPOS);

    GlobalVariable *gCFG_oldPOS = M.getGlobalVariable("PCALL_OACFI");
    gCFG_oldPOS->setDSOLocal(false);

    GlobalVariable *gvar_cfg_dataPOS = new GlobalVariable(
        M, blockItemsPOS->getType(), true, GlobalValue::ExternalLinkage,
        blockItemsPOS, "PCALL_OACFI");

    replaceGLBUsage(gvar_cfg_dataPOS, gCFG_oldPOS);

    Constant *cfgLenPOS = ConstantInt::get(int32Ty, list_OS.size(), false);
    GlobalVariable *gCFG_lenPOS = M.getGlobalVariable("PCALL_OACFI_C");
    gCFG_lenPOS->setInitializer(cfgLenPOS);

    Function *U_MPX = M.getFunction("update_mpx_table");

    Function *P_REF = M.getFunction("pcall_reference_monitor");
    Function *V_REF = M.getFunction("vcall_reference_monitor");
    Function *CFI_P_CTX_REF =
        M.getFunction("cfi_pcall_ctx_reference_monitor");
    Function *OACFI_REF = M.getFunction("cfi_call_reference_monitor");
    Function *CI_REF = M.getFunction("static_reference_monitor");

    unsigned long callID, originID;
    for (Function &Fn : M) {
      for (BasicBlock &BB : Fn) {
        for (Instruction &Inst : BB) {
          Instruction *inst = &Inst;
          if (isa<CallInst>(inst)) {
            CallInst *call = dyn_cast<CallInst>(inst);
            // if (call->getCalledFunction() &&
            //     (call->getCalledFunction() == P_REF ||
            //      call->getCalledFunction() == V_REF) ) {
            //       Value *idValue = call->getArgOperand(0);
            //       if (isa<ConstantInt>(idValue)) {
            //         ConstantInt *cint = dyn_cast<ConstantInt>(idValue);
            //         callID = cint->getZExtValue();

            //         if (mapPD.find(callID) != mapPD.end()) {
            //           int d = mapPD[callID];
            //           // if (d == OS_CTX) {
            //           //   call->setCalledFunction(CFI_P_CTX_REF);
            //           // } else if (d == OS) {
            //           //   call->setCalledFunction(OACFI_REF);
            //           // } else 
            //           if (d == CI) {
            //             call->setCalledFunction(CI_REF);
            //           }
            //         }
            //       }
            // } else 
            if (call->getCalledFunction() &&
                       call->getCalledFunction() == U_MPX) {
              Value *idValue = call->getArgOperand(2);
              if (isa<ConstantInt>(idValue)) {
                ConstantInt *cint = dyn_cast<ConstantInt>(idValue);
                originID = cint->getZExtValue();
                if (find(originList.begin(), originList.end(), originID) ==
                    originList.end()) {
                  Constant *rm_id = ConstantInt::get(int64Ty, 0, false);
                  call->setArgOperand(2, rm_id);
                }
              }
            }
          }
        }
      }
    }
  
    return true;
    }


private:
  pointToECMap mapPEC;
  pointToType mapPD;
  contextList originList;

};
char INSTCFG::ID = 0;
static RegisterPass<INSTCFG> Trans("llvm-inst-cfg",
                                   "LLVM Instrumentation of dCFG and cCFG");
