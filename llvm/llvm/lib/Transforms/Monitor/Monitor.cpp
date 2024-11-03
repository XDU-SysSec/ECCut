#include "Monitor.h"

using namespace llvm;

//Traverse bc
bool Monitor::runOnModule(Module &M) {
    for (auto &f: M) {
        for (auto &b: f) {
            for (auto &i: b) {
                Instruction *I = &i;
                if (isa<CallInst>(I) || isa<InvokeInst>(I) ) {
                    CallSite cs = CallSite(I);
                    if (cs.isIndirectCall()) {
                        if(isa<CallInst>(I->getPrevNode()) 
                          && cast<CallInst>(I->getPrevNode())->getCalledFunction()
                          && cast<CallInst>(I->getPrevNode())->getCalledFunction()->getName() == "vcall_reference_monitor")
                          continue;
                        monitor(cs);
                    }
                }
            }
        }
    }
}

//Instrument monitor for ICT
void Monitor::monitor(CallSite CS) {
  llvm::Value *CalleePtr = CS.getCalledValue();
  llvm::Value *tempCalleePtr = CalleePtr;
  Module *M = dyn_cast<Instruction>(tempCalleePtr)->getModule();
  LLVMContext &context = M->getContext();
  IRBuilder<> Builder(context);
  Builder.SetInsertPoint(CS.getInstruction());
  std::hash<std::string> hash_fn;
  while (1) {
    tempCalleePtr = tempCalleePtr->stripPointerCasts();
    if (isa<llvm::LoadInst>(tempCalleePtr)) {
      auto *loadCalleePtr = dyn_cast<llvm::LoadInst>(tempCalleePtr);

      // create an identification for the ICT
      std::string idCreator;
      llvm::raw_string_ostream rso(idCreator);
      loadCalleePtr->print(rso);
      idCreator +=
          ("[" + loadCalleePtr->getParent()->getParent()->getName().str() +
            "]");
      unsigned long id = hash_fn(idCreator) % 10000000;

      llvm::Value *id_value = llvm::ConstantInt::get(Type::getInt32Ty(context), id, false);
      llvm::Value *id_value_64 =
          Builder.CreateIntCast(id_value, Type::getInt64Ty(context), false);

      // collect calleePtr address and value
      llvm::Value *calleePtrAddr =
          EmitCastToVoidPtr(loadCalleePtr->getPointerOperand(), CS.getInstruction());
      llvm::Value *calleePtrAddrVal =
          Builder.CreatePtrToInt(calleePtrAddr, Type::getInt64Ty(context));
      llvm::Value *calleePtrVal =
          Builder.CreatePtrToInt(CalleePtr, Type::getInt64Ty(context));

      // prepare to call reference monitor
      llvm::FunctionType *preftype = llvm::FunctionType::get(
          Type::getVoidTy(context), {Type::getInt64Ty(context), Type::getInt64Ty(context), Type::getInt64Ty(context)}, false);

      llvm::Constant *rf = M->getOrInsertFunction("pcall_reference_monitor",preftype);

      llvm::CallInst *rfcall = Builder.CreateCall(
          rf, {id_value_64, calleePtrAddrVal, calleePtrVal});

      rfcall->setTailCallKind(llvm::CallInst::TailCallKind::TCK_NoTail);
      break;
    } else if (isa<llvm::BitCastInst>(tempCalleePtr)) {
      // if current IR is for casting the calleePtr to another type
      auto *castCalleePtr = dyn_cast<llvm::BitCastInst>(tempCalleePtr);
      tempCalleePtr = castCalleePtr->getOperand(0);
    } else if (isa<llvm::PHINode>(tempCalleePtr)) {
      // if current IR is a phi node because of pointer-to-member-function
      auto phiCalleePtr = dyn_cast<llvm::PHINode>(tempCalleePtr);

      // to collect phi calleePtr address, we need to create another phi node
      // in parallel
      llvm::IRBuilder<> PhiBuilder(phiCalleePtr);
      llvm::PHINode *phiCalleePtrAddr = PhiBuilder.CreatePHI(
          PointerType::getInt8PtrTy(context), phiCalleePtr->getNumIncomingValues());
      
      // replicate the same as calleePtr phi node
      for (unsigned int i = 0; i < phiCalleePtr->getNumIncomingValues();
            i++) {
        llvm::BasicBlock *bb = phiCalleePtr->getIncomingBlock(i);
        llvm::Value *value = phiCalleePtr->getIncomingValue(i);
        llvm::Instruction *terminst = bb->getTerminator();

        while (1) {
          if (isa<llvm::LoadInst>(value)) {
            auto phiload = dyn_cast<llvm::LoadInst>(value);
            llvm::IRBuilder<> BBBuilder(terminst);
            // llvm::Value *phival = PhiBuilder.CreateBitCast(
            //     phiload->getPointerOperand(), Type::getVoidTy(context));
            llvm::Value *phival =
              EmitCastToVoidPtr(phiload->getPointerOperand(), terminst);

            // llvm::Value *phival = phiload->getPointerOperand();
            phiCalleePtrAddr->addIncoming(phival, bb);
            break;
          } else if (isa<llvm::BitCastInst>(value)) {
            auto phicast = dyn_cast<llvm::BitCastInst>(value);
            value = phicast->getOperand(0);
          } else if (isa<llvm::IntToPtrInst>(value)) {
            auto phiipcast = dyn_cast<llvm::IntToPtrInst>(value);
            value = phiipcast->getOperand(0);
          } else if (isa<llvm::ExtractValueInst>(value)) {
            auto phiextract = dyn_cast<llvm::ExtractValueInst>(value);
            value = phiextract->getOperand(0);
          } else {
            llvm::errs()
                << "[ECCut-INSTRUMENT] Following case requires to "
                    "handle (From phi node handler) ...\n";
            llvm::errs()
                << "-------------------------------------------------\n";
            break;
          }
        }
      }

      // create an identification for the ICT
      std::string idCreator;
      llvm::raw_string_ostream rso(idCreator);
      phiCalleePtr->print(rso);
      idCreator +=
          ("[" + phiCalleePtr->getParent()->getParent()->getName().str() +
            "]");
      unsigned long id = hash_fn(idCreator) % 10000000;

      llvm::Value *id_value = llvm::ConstantInt::get(Type::getInt32Ty(context), id, false);
      llvm::Value *id_value_64 =
          Builder.CreateIntCast(id_value, Type::getInt64Ty(context), false);

      // llvm::Value *phiCalleePtrAddr

      llvm::Value *phiCalleePtrAddrVal =
          Builder.CreatePtrToInt(phiCalleePtrAddr, Type::getInt64Ty(context));

      llvm::Value *calleePtrVal =
          Builder.CreatePtrToInt(CalleePtr, Type::getInt64Ty(context));

      // prepare to call reference monitor
      llvm::FunctionType *preftype = llvm::FunctionType::get(
          Type::getVoidTy(context), {Type::getInt64Ty(context), Type::getInt64Ty(context), Type::getInt64Ty(context)}, false);
      
      llvm::Constant *rf = M->getOrInsertFunction("pcall_reference_monitor",preftype);

      llvm::CallInst *rfcall = Builder.CreateCall(
          rf, {id_value_64, phiCalleePtrAddrVal, calleePtrVal});

      rfcall->setTailCallKind(llvm::CallInst::TailCallKind::TCK_NoTail);

      break;
    } else if (isa<llvm::IntToPtrInst>(tempCalleePtr)) {
      auto *cast = dyn_cast<llvm::IntToPtrInst>(tempCalleePtr);
      tempCalleePtr = cast->getOperand(0);
    } else if (isa<llvm::ExtractValueInst>(tempCalleePtr)) {
      auto *extract = dyn_cast<llvm::ExtractValueInst>(tempCalleePtr);
      tempCalleePtr = extract->getOperand(0);
    } else {
      // log this event to track missing cases
      llvm::errs() << "[ECCut-INSTRUMENT] Following case requires to "
                      "handle (top-level handler) ...\n";
      llvm::errs() << "-------------------------------------------------\n";
      break;
    }
  }
}

//Generate ref_id with int64 type
unsigned long Monitor::Ref_id(LoadInst *loadCalleePtr){
  // create an identification for the ICT
  std::string idCreator;
  std::hash<std::string> hash_fn;
  raw_string_ostream rso(idCreator);
  loadCalleePtr->print(rso);
  idCreator += ("[" + loadCalleePtr->getFunction()->getName().str() + "]");
  unsigned long id = hash_fn(idCreator) % 10000000;
  return id;
}

//return ptraddress with int64 type
Value* Monitor::EmitCastToVoidPtr(Value *value, Instruction *InsertPoint) {
  LLVMContext &context = dyn_cast<Instruction>(InsertPoint)->getModule()->getContext();
  IRBuilder<> Builder(context);
  Builder.SetInsertPoint(InsertPoint);
  unsigned addressSpace =
      cast<PointerType>(value->getType())->getAddressSpace();
  PointerType *destType = PointerType::getInt8PtrTy(context);
  if (addressSpace)
    destType = Type::getInt8PtrTy(context, addressSpace);
  if (value->getType() == destType) return value;
  return Builder.CreateBitCast(value, destType);
}


char Monitor::ID = 0;
static RegisterPass<Monitor>
Y("monitor", "Monitor Pass (with getAnalysisUsage implemented)");
