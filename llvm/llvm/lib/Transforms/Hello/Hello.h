#ifndef LLVM_HELLO_H
#define LLVM_HELLO_H

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
#include "llvm/IR/Constants.h"
#include <sstream>

using namespace llvm;

typedef std::set<Value *> Originset;
typedef std::set<Value *>::iterator OriginsetIt;
typedef std::map<CallInst *,Originset *> IcallToOriginsetMap;
typedef std::map<CallInst *,Originset *>::iterator IcallToOriginsetMapIt;
typedef std::map<Instruction *, unsigned long> OriginInstToIDMap;
typedef std::map<Instruction *, unsigned long>::iterator OriginInstToIDMapIt;
typedef struct {
  signed int ArgState; //trackback state. -2:call is an indirect call;-1:trackback the return value; >= 0:trackback the ArgState-th argument 
  StringRef FunctionName;//called function name
  Instruction *CallInstruction; //call instruction
} Func;
typedef struct {
  int offset;
  Function *F;
} STF;

extern IcallToOriginsetMap mapOrigin;
extern OriginInstToIDMap mapOriginIDInst;

class FuncTraverse;

int iCallNum = 0, // the num of indirect call
    FuncAllNum, // the num of traversed function
    STFNum,
    STFGNum,
    biaoji = 0,
    V_f;
bool beforestore = false;
Func FuncAll[32768]; // store all of the traversed functions
std::vector<STF> STFV;
std::vector<STF> STFVG;
std::vector<GlobalVariable*> CAGV;
std::vector<std::string> SR;
std::vector<std::string> SRG;
std::vector<Function*> Funcset;
std::vector<Instruction*> Callset;
std::vector<Instruction*> ICallset;
std::set<Function*> ConsFuncset;
std::set<StoreInst*> StoreVtableset;
std::map<Type*, std::set<StoreInst*>> TSmap;
class Hello : public ModulePass {
public:
  static char ID;
  Hello() : ModulePass(ID){};
  ~Hello() {};

  Originset *oset;
  IcallToOriginsetMap mapOrigin;
  OriginInstToIDMap mapOriginIDInst;

  virtual bool runOnModule(Module &M);

private:
  std::hash<std::string> hash_fn;
  //Find StoreInst from call/icall/ret
  void CallToAlloca(int ArgState, Instruction *I, FuncTraverse *Prev);
  //Trave StoreInst
  void StoreTraverse(Instruction *A, FuncTraverse *FT, int g, int flag/*stcall*/);
  //Trave StoreInst for Global variables
  void StoreTraverseG(Instruction *A, FuncTraverse *FT, int g, int flag/*stcall*/);
  
  void STCall(Instruction *ci, Value *A, FuncTraverse *FT, int g);
  
  void STCallG(Instruction *ci, Value *A, FuncTraverse *FT, int g);
  
  void STFunction(Function *F, FuncTraverse *FT, int g, int i, Instruction *ci);
  
  void STFunctionG(Function *F, FuncTraverse *FT, int g, int i, Instruction *ci);
  //Find the call of store's function
  void StoreToCall(Instruction *I, FuncTraverse *FT, Value *V = NULL);
  //Trave global use
  void GlobalUse(Value *G, FuncTraverse *FT, int i);

  int64_t get_struct_mem_offset(Value *getelementptr_inst);
  //Compare Gep's offset
  bool isGepOffsetEqual(Value *I, Value *G);
  
  bool isGepOffsetEqual1(Value *I, Instruction *G, FuncTraverse *FT);
  //Determine whether the Value operand of Store is a function parameter, yes return the specific offset position, no return -3
  signed int ArginFunction(Value *V);
  //Determining if a function is a duplicate
  bool isNotRepeatFunction(Instruction *I, int offset);
  //Determining if a function is a duplicate for return
  bool isNotRepeatFunction(Function *F);
  
  bool isInFuncALL(const Instruction *ci, int i);
  
  bool isInSTFV(Function *F, int i);
  
  bool isInSTFVG(Function *F, int i);
  
  bool isInCAGV(GlobalVariable *G);
  //Type Matching Functions
  bool isTypeMatch(const Instruction *sink, const Value *source);
  
  bool isTypeSame(Instruction *c, Instruction *I);

  void beforeStore(StoreInst *S, FuncTraverse *FT);

  void FindVtable(Instruction *I);
  
  void VtableMap();
  //ret the returninst of the Function
  Instruction * getReturn(Function *F);
  //Instrument Store instructions
  void EmitStoreOrigin(StoreInst *Origin_Store, FuncTraverse *FT, int type_gep_offset = 0);
  //Instrument Globlal
  void EmitGlobalOrigin(Value *OriginInst, FuncTraverse *FT, GlobalVariable *globalvl);
  //Instrument GEP instructions
  void EmitGepOrigin(GetElementPtrInst *OriginInst, FuncTraverse *FT, GlobalVariable *globalvl); 
  //对Instrument Load instructions
  void EmitLoadOrigin(LoadInst *OriginInst, FuncTraverse *FT, GlobalVariable *globalvl);
  //Instrument PHI instructions
  void EmitPhiOrign(PHINode *Origin_Phi, FuncTraverse *FT);
  //Instrument SELECT instructions
  void EmitSelectOrign(SelectInst *Origin_Select, FuncTraverse *FT);
  //Instrument CALL instructions
  void EmitCallOrign(CallInst *Origin_Call, FuncTraverse *FT, Value *ptr_val);

  void FindValInVtable(StoreInst* Origin_Inst, FuncTraverse* FT);

  void EmitVcallOrigin(StoreInst *Origin_Store, FuncTraverse *FT);
  //Instrument update_mpk_table
  void EmitUpdateCall(Instruction *Origin_Inst, Value *ptr_OriginVal, Value *ptr_OriginAddr, Instruction *InsertPoint,  FuncTraverse *FT, int type_gep_offset = 0, Value* ctx = NULL);
  //return ptraddress with int64 type
  Value* EmitCastToVoidPtr(Value *value, Instruction *InsertPoint);
 
  bool IssameInst(Instruction *Inst, GetElementPtrInst *GepInst);
};

class FuncTraverse {
public:
  int *GInT,// the offset of gep in all Instruction
      TInstNum,// the num of traversed Instruction
      GepNum,// the num of Gep Instruction
      FNameNum,// the num of function on this on this load
      dt = 0,//donminate
      globaluse = 0,
      rci;// the offset of ret/call/icall Instruction
  Instruction **TInst, **GInst;
  Func *FName;
  Value *Val;//the argument of call/the fptr of icall/the value of retInst
 
  //constructor
  FuncTraverse(){}
  
  FuncTraverse(signed int i, Instruction *I, FuncTraverse *FT) {
    if (i == -2) {
      Malloc(-1 , -1, -1);
      Assignment(i, I);
    } else {
      Malloc(FT->FNameNum, FT->TInstNum, FT->GepNum);
      Copy(FT);
      Assignment(i, I);
    }
  }
  //assignment for Instruction
  void InstAssignment(Instruction *I) {
    TInstNum++;
    if ((FName[FNameNum].ArgState != -2) && (TInstNum == sizeof(TInst)/sizeof(Instruction*))) {
       errs()() << "Insterr\n";
    }
    TInst[TInstNum] = I;
  }

  void GepAssignment(Instruction *I) {
    GepNum++;
    if ((FName[FNameNum].ArgState != -2) && (TInstNum == sizeof(TInst)/sizeof(Instruction*))) {
       errs()() << "Instgeperr\n";
    }
    GInst[GepNum] = I;
    GInT[GepNum] = TInstNum;
  }
 
  ~FuncTraverse(){}

private:
  //process and assignment
  void Assignment(signed int i, Instruction *I) {
    int j = I->getNumOperands() - 1, k = i;
    if (i < j && i >= 0) {
      i = 0;
    }
    switch (i) {
    case -3: {
      FuncAssignment(i, I);
      InstAssignment(I);
      rci = TInstNum;
      Val = dyn_cast<StoreInst>(I)->getValueOperand();
    } break;
    case -2: {
      FuncAssignment(i, I);
      InstAssignment(I);
      rci = TInstNum;
      Val = isa<CallInst>(I) ? dyn_cast<CallInst>(I)->getCalledValue() : dyn_cast<InvokeInst>(I)->getCalledValue();
    } break;
    case -1: {
      FuncAssignment(i, I);
      InstAssignment(I);
      rci = TInstNum;
      Val = dyn_cast<ReturnInst>(I)->getReturnValue();
    } break;
    case 0: {
      FuncAssignment(k, I);
      InstAssignment(I);
      rci = TInstNum;
      Val = I->getOperand(k);
    } break;
    default:
      errs()() << "There is an error with ArgState\n";
      break;
    }
  }
  //the real assignment for Func
  void FuncAssignment(signed int i, Instruction *I) {
    FNameNum++;
    FuncAllNum++;
    FName[FNameNum].ArgState = i;
    FName[FNameNum].CallInstruction = I;
    FName[FNameNum].FunctionName = I->getFunction()->getName();
    FuncAll[FuncAllNum] = FName[FNameNum];
  }
  //copy
  void Copy(FuncTraverse *FT) {
    int i;
    for (i = 0; i <= TInstNum; i++) {
      TInst[i] = FT->TInst[i];
    }
    for (i = 0; i <= FNameNum; i++) {
      FName[i] = FT->FName[i];
    }
    for (i = 0; i <= GepNum; i++) {
      GInst[i] = FT->GInst[i];
      GInT[i] = FT->GInT[i];
    }
  }
  //malloc
  void Malloc(int f, int t, int g) {
    FNameNum = f;
    TInstNum = t;
    GepNum = g;
    FName = new Func[FNameNum + 2];
    TInst = new Instruction*[TInstNum + 24];
    GInst = new Instruction*[GepNum + 6];
    GInT = new int[GepNum + 6];
  }

};

#endif // LLVM_HELLO_H
