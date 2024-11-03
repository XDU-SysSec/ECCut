#include "LookOrigin.h"


using namespace llvm;

char LookOrigin::ID = 0;

mapIcallInstNum mapIcallInst;
IcallToUpdate icalltoupdate;
IcallIDset icallIDset;
IcallIDset vcallIDset;

unsigned long indirect_num = 0;

bool LookOrigin::runOnModule(Module &M) {

  mapIcall(M);
  LookForUpdate(M);
  return false;
}

void LookOrigin::mapIcall(llvm::Module &M) {
  Instruction *I;
  for(auto &f: M) {
    for(auto &b: f) {
      for(auto &i: b) {
        I = &i;
        if(isa<CallInst>(I)) {
          CallSite cs = CallSite(I);
          CallInst *ci = dyn_cast<CallInst>(I);
          if(cs.isIndirectCall()) {
            mapIcallInst[indirect_num] = I;
            indirect_num++;
          } 
          if(ci->getCalledFunction() && ci->getCalledFunction()->hasName() && ci->getCalledFunction()->getName() == "pcall_reference_monitor") {
            icallIDset.insert(cast<ConstantInt>(ci->getOperand(0))->getZExtValue());
          } else if(ci->getCalledFunction() && ci->getCalledFunction()->hasName() && ci->getCalledFunction()->getName() == "vcall_reference_monitor") {
            vcallIDset.insert(cast<ConstantInt>(ci->getOperand(0))->getZExtValue());
          }
        } else if(isa<InvokeInst>(I)) {
          CallSite cs = CallSite(I);
          InvokeInst *ci = dyn_cast<InvokeInst>(I);
          if(cs.isIndirectCall()) {
            mapIcallInst[indirect_num] = I;
            indirect_num++;
          } 
          if(ci->getCalledFunction() && ci->getCalledFunction()->hasName() && ci->getCalledFunction()->getName() == "pcall_reference_monitor") {
            icallIDset.insert(cast<ConstantInt>(ci->getOperand(0))->getZExtValue());
          } else if(ci->getCalledFunction() && ci->getCalledFunction()->hasName() && ci->getCalledFunction()->getName() == "vcall_reference_monitor") {
            vcallIDset.insert(cast<ConstantInt>(ci->getOperand(0))->getZExtValue());
          }
        }
      }
    }
  }
}

void LookOrigin::LookForUpdate(llvm::Module &M) {

  LLVMContext &context = M.getContext();

  Instruction *I;
  for(auto &f: M) {
    for(auto &b: f) {
      for(auto &i: b) {
        I = &i;
        if(isa<CallInst>(I)) {
          CallInst *ci = cast<CallInst>(I);
          if(ci->getNumOperands() == 6 
                && ci->getCalledFunction() 
                && ci->getCalledFunction()->getName() == "update_mpx_table"
                && ci->getOperand(1) != ConstantInt::get(Type::getInt64Ty(context), 0, false)) {
            mapIcallToUpdate(ci);
          }
        }
      }
    }
  }
}

void LookOrigin::mapIcallToUpdate(llvm::CallInst *update_call) {
  for(mapIcallInstNumIt it = mapIcallInst.begin(); it != mapIcallInst.end(); it++) {
    unsigned long update_icallnum = dyn_cast<ConstantInt>(update_call->getOperand(4))->getZExtValue();
    if(it->first == update_icallnum) {
      if(it->second->getPrevNode()
        && isa<CallInst>(it->second->getPrevNode()) 
        && cast<CallInst>(it->second->getPrevNode())->getCalledFunction() 
        && cast<CallInst>(it->second->getPrevNode())->getCalledFunction()->hasName()
        && ( cast<CallInst>(it->second->getPrevNode())->getCalledFunction()->getName() == "pcall_reference_monitor" 
          ||cast<CallInst>(it->second->getPrevNode())->getCalledFunction()->getName() == "vcall_reference_monitor") ) {     
        Value *icallId_val =  it->second->getPrevNode()->getOperand(0);
        unsigned long icallId = dyn_cast<ConstantInt>(icallId_val)->getZExtValue();
        updatecallset = icalltoupdate[icallId];
        updatecallset.insert(update_call);
        icalltoupdate[icallId] = updatecallset;
        break;
      }
      else if (it->second->getPrevNode()->getPrevNode()
        && isa<CallInst>(it->second->getPrevNode()->getPrevNode()) 
        && cast<CallInst>(it->second->getPrevNode()->getPrevNode())->getCalledFunction() 
        && cast<CallInst>(it->second->getPrevNode()->getPrevNode())->getCalledFunction()->hasName()
        && ( cast<CallInst>(it->second->getPrevNode()->getPrevNode())->getCalledFunction()->getName() == "pcall_reference_monitor" 
          ||cast<CallInst>(it->second->getPrevNode()->getPrevNode())->getCalledFunction()->getName() == "vcall_reference_monitor") ) {
            Value *icallId_val =  it->second->getPrevNode()->getPrevNode()->getOperand(0);
        unsigned long icallId = dyn_cast<ConstantInt>(icallId_val)->getZExtValue();
        updatecallset = icalltoupdate[icallId];
        updatecallset.insert(update_call);
        icalltoupdate[icallId] = updatecallset;
        break;
      } else if (it->second->getPrevNode()->getPrevNode()->getPrevNode()
        && isa<CallInst>(it->second->getPrevNode()->getPrevNode()->getPrevNode()) 
        && cast<CallInst>(it->second->getPrevNode()->getPrevNode()->getPrevNode())->getCalledFunction() 
        && cast<CallInst>(it->second->getPrevNode()->getPrevNode()->getPrevNode())->getCalledFunction()->hasName()
        && ( cast<CallInst>(it->second->getPrevNode()->getPrevNode()->getPrevNode())->getCalledFunction()->getName() == "pcall_reference_monitor" 
          ||cast<CallInst>(it->second->getPrevNode()->getPrevNode()->getPrevNode())->getCalledFunction()->getName() == "vcall_reference_monitor") ) {
            Value *icallId_val =  it->second->getPrevNode()->getPrevNode()->getPrevNode()->getOperand(0);
        unsigned long icallId = dyn_cast<ConstantInt>(icallId_val)->getZExtValue();
        updatecallset = icalltoupdate[icallId];
        updatecallset.insert(update_call);
        icalltoupdate[icallId] = updatecallset;
        break;
      } else if (it->second->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()
        && isa<CallInst>(it->second->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()) 
        && cast<CallInst>(it->second->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode())->getCalledFunction() 
        && cast<CallInst>(it->second->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode())->getCalledFunction()->hasName()
        && ( cast<CallInst>(it->second->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode())->getCalledFunction()->getName() == "pcall_reference_monitor" 
          ||cast<CallInst>(it->second->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode())->getCalledFunction()->getName() == "vcall_reference_monitor") ) {
            Value *icallId_val =  it->second->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()->getOperand(0);
        unsigned long icallId = dyn_cast<ConstantInt>(icallId_val)->getZExtValue();
        updatecallset = icalltoupdate[icallId];
        updatecallset.insert(update_call);
        icalltoupdate[icallId] = updatecallset;
        break;
      } else if (it->second->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()
        && isa<CallInst>(it->second->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()) 
        && cast<CallInst>(it->second->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode())->getCalledFunction() 
        && cast<CallInst>(it->second->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode())->getCalledFunction()->hasName()
        && ( cast<CallInst>(it->second->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode())->getCalledFunction()->getName() == "pcall_reference_monitor" 
          ||cast<CallInst>(it->second->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode())->getCalledFunction()->getName() == "vcall_reference_monitor") ) {
            Value *icallId_val =  it->second->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode()->getOperand(0);
        unsigned long icallId = dyn_cast<ConstantInt>(icallId_val)->getZExtValue();
        updatecallset = icalltoupdate[icallId];
        updatecallset.insert(update_call);
        icalltoupdate[icallId] = updatecallset;
        break;
      }
    }
  }
  updatecallset.clear();
}

void LookOrigin::printmap() {
  std::string P_REF = "pcall_reference_monitor";
  std::string V_REF = "vcall_reference_monitor";

  for(IcallToUpdateIt it = icalltoupdate.begin(); it != icalltoupdate.end(); it++ ) {
    if(icallIDset.count(it->first)) {
      icallIDset.erase(it->first);
    } else if(vcallIDset.count(it->first)) {
      vcallIDset.erase(it->first);
    }
  }
}

static RegisterPass<LookOrigin> X("lookorigin", "LookOrigin Pass");
