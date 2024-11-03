#include "Hello.h"

bool Hello::runOnModule(Module &M) {
  for (auto &f : M) {
    Function *F = &f;
    //record function can be address taken
    if (F->hasAddressTaken()) {
      Funcset.push_back(F);
    }
    for (auto &b : f) {
      for (auto &i : b) {
        Instruction *I = &i;
        if (isa<CallInst>(I) || isa<InvokeInst>(I)) {
          CallSite cs = CallSite(I);
          //record indirect call and direct call
          if (cs.isIndirectCall()) {
            ICallset.push_back(I);
          } else {
            Callset.push_back(I);
          }
        }
        //record Class generator functions with virtual tables
        if (isa<StoreInst>(I) && (!isa<Instruction>(I->getOperand(0))) && isa<BitCastOperator>(I->getOperand(0)) &&
            isa<GEPOperator>(dyn_cast<BitCastOperator>(I->getOperand(0))->getOperand(0))) {
          ConsFuncset.insert(F);
          StoreVtableset.insert(dyn_cast<StoreInst>(I));
        }
      }
    }
  }

  for (auto I : ICallset) {
    iCallNum++;
    FuncAllNum = -1;
    STFNum = -1;
    STFGNum = -1;
    if (isa<CallInst>(I->getPrevNode()) && dyn_cast<CallInst>(I->getPrevNode())->getCalledFunction()
        && dyn_cast<CallInst>(I->getPrevNode())->getCalledFunction()->getName() == "vcall_reference_monitor") {
      V_f = 0;
      //analysis vcall
      FindVtable(I);
      if (V_f <= 0) {
      //analysis vcall can not find vtable
        STFV.clear();
        STFVG.clear();
        CAGV.clear();
        if (iCallNum)
          CallToAlloca(-2, I, NULL);
      }
    } else {
      //analysis indirect call
      STFV.clear();
      STFVG.clear();
      CAGV.clear();
      if (iCallNum)
        CallToAlloca(-2, I, NULL);
    }

  }
  return false;
}

void Hello::VtableMap() {
  for (auto S: StoreVtableset) {
    Instruction *I = dyn_cast<Instruction>(S->getPointerOperand()->stripPointerCastsAndInvariantGroups());
    while(1) {
      if (isa<LoadInst>(I)) {
        Type *T = I->getType();
        if (TSmap.count(T)) {
            TSmap[T].insert(S);
        } else {
            std::set<StoreInst*> sset;
            sset.insert(S);
            TSmap.insert(std::make_pair(T,sset));
        }
        break ;
      } else if(isa<GetElementPtrInst>(I)) {
        I = dyn_cast<Instruction>(I->getOperand(0)->stripPointerCastsAndInvariantGroups());
      } else if (isa<AllocaInst>(I)) {
        Type *T = dyn_cast<AllocaInst>(I)->getAllocatedType();
        if (TSmap.count(T)) {
            TSmap[T].insert(S);
        } else {
            std::set<StoreInst*> sset;
            sset.insert(S);
            TSmap.insert(std::make_pair(T,sset));
        }
        break ;
      } else {
        errs() << *I << "VMerr\n";
        break ;
      }
    }
  }
}
void Hello::FindVtable(Instruction *I) {
  FuncTraverse *FT = new FuncTraverse(-2, I, NULL);
  int k = isa<CallInst>(I) ? 1 : 3;
  Value *V = I->getOperand(I->getNumOperands() - k);
  if (isa<LoadInst>(V)) {
    FT->InstAssignment(dyn_cast<Instruction>(V));
    V = dyn_cast<LoadInst>(V)->getOperand(0);
    if (isa<GetElementPtrInst>(V)) {
      FT->InstAssignment(dyn_cast<Instruction>(V));
      FT->GepAssignment(dyn_cast<Instruction>(V));
      V = dyn_cast<GetElementPtrInst>(V)->getOperand(0);
      if (isa<LoadInst>(V)) {
        FT->InstAssignment(dyn_cast<Instruction>(V));
        V = dyn_cast<LoadInst>(V)->getOperand(0);
        if (isa<BitCastInst>(V)) {
          Value *v0 = dyn_cast<BitCastInst>(V)->getOperand(0);
          Type *T0;
          if (isa<BitCastInst>(v0)) {
            FT->InstAssignment(dyn_cast<Instruction>(V));
            T0 = dyn_cast<BitCastInst>(V)->getSrcTy();
            if (TSmap.count(T0)) {
              V_f++;
              for (auto S: TSmap[T0]) {
                EmitVcallOrigin(S,FT);
              }
            }
          }
          FT->InstAssignment(dyn_cast<Instruction>(V));
          Type *T = dyn_cast<BitCastInst>(V)->getSrcTy();
          if(T0 != T) {
            if (TSmap.count(T)) {
              V_f++;
              for (auto S: TSmap[T]) {
                EmitVcallOrigin(S,FT);
              }
            }
          }
        }
      }
    }
  }
}

void  Hello::CallToAlloca(int ArgState, Instruction *I, FuncTraverse *Prev) {
  //break loop
  if (Prev != NULL && Prev->FName[Prev->FNameNum].ArgState == -1 &&
      Prev->FName[Prev->FNameNum - 1].ArgState == -3 ) {
    return ;
  }
  FuncTraverse *FT = new FuncTraverse(ArgState, I, Prev);
  Value *Val = FT->Val;
  int flag = 0;
  //find alloca of function pointer
  while (1) {
    if (isa<Instruction>(Val)){
      FT->InstAssignment(dyn_cast<Instruction>(Val));
      if (isa<AllocaInst>(Val)) {
        StoreTraverse(dyn_cast<Instruction>(Val), FT, FT->GepNum, 0);
        break;
      } else if (isa<BitCastInst>(Val)) {
        Val = dyn_cast<Instruction>(Val)->getOperand(0);
      } else if (isa<CallInst>(Val)) {
        CallSite cs = CallSite(Val);
        if (cs.isIndirectCall()) {
          Instruction *icall = dyn_cast<Instruction>(Val);
          Function *F;
          for (int i = 0; i < Funcset.size(); i++) {
            F = Funcset[i];
            if (isTypeMatch(icall, F)) {
                CallToAlloca(-1, getReturn(F), FT);
            }
          }
        } else {
          Function *F;
          if (cs.getCalledFunction()) {
            F = cs.getCalledFunction();
          } else {//for 483 call bitcast
            F = dyn_cast<Function>(dyn_cast<Instruction>(Val)->getOperand(dyn_cast<Instruction>(Val)->getNumOperands() - 1)->stripPointerCasts());
          }
          if ((!F->isDeclaration())) {
            CallToAlloca(-1, getReturn(F), FT);
          }
        }
        break;
      } else if (isa<InvokeInst>(Val)) {
        CallSite cs = CallSite(Val);
        if (cs.isIndirectCall()) {
          Instruction *icall = dyn_cast<Instruction>(Val);
          Module *M = I->getModule();
          Function *F;
          for (int i = 0; i < Funcset.size(); i++) {
            F = Funcset[i];
            if (isTypeMatch(icall, F)) {
              CallToAlloca(-1, getReturn(F), FT);
            }
          }
        } else {
          Function *F;
          if (cs.getCalledFunction()) {
            F = cs.getCalledFunction();
          } else {//for 483 invoke bitcast
            F = dyn_cast<Function>(dyn_cast<Instruction>(Val)->getOperand(dyn_cast<Instruction>(Val)->getNumOperands() - 3)->stripPointerCasts());
          }
          if (!F->isDeclaration()) {
            CallToAlloca(-1, getReturn(F), FT);
          }
        }
        break;
      } else if (isa<GetElementPtrInst>(Val)) {
        GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(Val);
        FT->GepAssignment(dyn_cast<Instruction>(Val));//book GepInst
        Val = dyn_cast<GetElementPtrInst>(Val)->getOperand(0);
      } else if (isa<LoadInst>(Val)) {
        Val = dyn_cast<LoadInst>(Val)->getPointerOperand();
      } else if (isa<SelectInst>(Val)) {
        EmitSelectOrign(cast<SelectInst>(Val), FT);
        break;
      } else if (isa<PHINode>(Val)) {
        EmitPhiOrign(cast<PHINode>(Val), FT);
        break;
      } else {
        errs() << "CS Inst err: " << *Val << "\n";
        break;
      }
    } else {
      if (isa<Function>(Val)) {
        V_f++;
        //emitCall
        EmitCallOrign(cast<CallInst>(FT->TInst[FT->TInstNum]), FT, Val);
        break;
      } else if (isa<GlobalVariable>(Val)) {
        FT->globaluse = 1;
        GlobalUse(Val, FT, FT->GepNum);
        GlobalVariable *G = dyn_cast<GlobalVariable>(Val);
        if (G->hasInitializer() && !G->getInitializer()->isNullValue() && !isa<ConstantAggregateZero>(G)) {
          if (FT->globaluse > 1) {
            CAGV.push_back(G);
          } else {
            if (!isInCAGV(G)) {
              V_f++;
              EmitGlobalOrigin(FT->TInst[FT->TInstNum],FT,cast<GlobalVariable>(Val));
            }
          }
        } else {
          if (FT->globaluse > 1) {
            CAGV.push_back(G);
          }
        }
        FT->globaluse = 0;
        break;
      } else if (isa<GEPOperator>(Val)) {
        GEPOperator *gep = dyn_cast<GEPOperator>(Val);
        Val = dyn_cast<GEPOperator>(Val)->getOperand(0);
        FT->GepAssignment(FT->TInst[FT->TInstNum]);
      } else if (isa<BitCastOperator>(Val)) {
        Val = dyn_cast<BitCastOperator>(Val)->getOperand(0);
      } else if (isa<Argument>(Val)) {
        StoreToCall(FT->TInst[FT->TInstNum], FT, Val);
        break;
      } else {
        if (isa<Constant>(Val)) {
          if (!dyn_cast<Constant>(Val)->isNullValue()) {
            errs() << "CS Constant: " << *Val << "\n";
          }
        } else {
          errs() << "CS NotInst err: " << *Val << "\n";
        }
        break;
      }
    }
  }
}
void Hello::StoreTraverse(Instruction *A, FuncTraverse *FT, int g, int flag) {
  Instruction *I, *Pre = FT->TInst[FT->TInstNum - 1];
  for (auto u : dyn_cast<Value>(A)->users()) {
    if (FT->dt == 1)
      break;//to get the closest store
    if (isa<Instruction>(u)) {
      I = dyn_cast<Instruction>(u);
      if (I == Pre)
        continue;
      if (isa<LoadInst>(I)) {
        StoreTraverse(I, FT, g, flag);
      } else if (isa<GetElementPtrInst>(I)) {
        if (g < 0) 
          continue;
        if (isGepOffsetEqual1(I, FT->GInst[g], FT))
          StoreTraverse(I, FT, g - 1, flag);
      } else if (isa<BitCastInst>(u)) {
        StoreTraverse(I, FT, g, flag);
      } else if (isa<PtrToIntInst>(u)) {
        StoreTraverse(I, FT, g, flag);
      } else if (isa<CallInst>(u)) {
        STCall(I, A, FT, g);
      } else if (isa<InvokeInst>(u)) {
        STCall(I, A, FT, g);
      } else if (isa<ReturnInst>(u)) {
      } else if (isa<ZExtInst>(u)) {
      } else if (isa<TruncInst>(u)) {
      } else if (isa<FCmpInst>(u)) {
      } else if (isa<AddOperator>(u)) {
      } else if (isa<ICmpInst>(u)) {
      } else if (isa<SubOperator>(u)) {
      } else if (isa<PHINode>(u)) {
      } else if (isa<SExtInst>(u)) {
      } else if (isa<FPExtInst>(u)) {
      } else if (dyn_cast<Instruction>(u)->getOpcode() == 21) { // srem
      } else if (dyn_cast<Instruction>(u)->getOpcode() == 26) /*and*/ {
      } else if (dyn_cast<Instruction>(u)->getOpcode() == 27) /*or*/ {
      } else if (dyn_cast<Instruction>(u)->getOpcode() == Instruction::FDiv) {
      } else if (isa<StoreInst>(u)) {
        StoreInst *S = dyn_cast<StoreInst>(I);
        Value *V = S->getValueOperand();
        if (A == V) { continue; }
        if (isa<Constant>(V) && dyn_cast<Constant>(V)->isNullValue()) { continue; }
        signed int arg_num = ArginFunction(V);
        if (flag == 1){
          if (arg_num < 0) {
            V_f++;
	    FT->globaluse += 1;
            if (FT->FNameNum > 1 && FT->FName[FT->FNameNum - 1].ArgState == -3) {
              beforestore = true;
              //emitstore
              if(isa<PointerType>(dyn_cast<StoreInst>(u)->getValueOperand()->getType()))
                EmitStoreOrigin(S,FT,g);
            } else {
              beforeStore(S, FT);
              if (beforestore) {
                beforestore = false;
              } else {
                //emit
                if(isa<PointerType>(dyn_cast<StoreInst>(u)->getValueOperand()->getType()))
                  EmitStoreOrigin(S,FT,g);
              }
            }
          }
        } else {
          if (arg_num >= 0) {
            FT->InstAssignment(I);     
            if (FT->FName[FT->FNameNum].ArgState == -1) {
              if (isNotRepeatFunction(FT->TInst[FT->rci - 1], -1)) {
                CallToAlloca(ArginFunction(V), FT->TInst[FT->rci - 1], FT);
              }
            } else {
              StoreToCall(S, FT);
            }
          } else {
            DominatorTree *DT = new DominatorTree(*S->getFunction());
            FT->dt = DT->dominates(S, Pre);
            FT->globaluse += 1;
            if (FT->FNameNum > 1 && FT->FName[FT->FNameNum - 1].ArgState == -3) {
              beforestore = true;
               V_f++;
               if(isa<PointerType>(S->getValueOperand()->getType()))
                 EmitStoreOrigin(S,FT,g);
            } else {
              beforeStore(S, FT);
              if (beforestore) {
                beforestore = false;
              } else {
                 V_f++;
                if(isa<PointerType>(S->getValueOperand()->getType()))
                  EmitStoreOrigin(S,FT,g);
              }
            }
          }
        }
      } 
    } 
  }
}

void Hello::STFunction(llvm::Function *F, FuncTraverse *FT, int g, int i, Instruction *CI) {
  if (isInSTFV(F, i)) {return;}
  STFV.push_back({i, F});
  STFNum++;
  auto argv = F->arg_begin() + i;
  for (auto v: argv->users()) {
    if (isa<StoreInst>(v)) {
      StoreTraverse(dyn_cast<Instruction>(dyn_cast<StoreInst>(v)->getPointerOperand()), FT, g, 1);
    } else if (isa<CallInst>(v)) {
      CallInst *ci = dyn_cast<CallInst>(v);
      STCall(ci, argv, FT, g);
    } else if (isa<InvokeInst>(v)) {
      InvokeInst *ci = dyn_cast<InvokeInst>(v);
      STCall(ci, argv, FT, g);
    } else if (isa<GetElementPtrInst>(v)) {
      if (g >= 0 && isGepOffsetEqual1(v, FT->GInst[g], FT)) {
        GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(v);
        StoreTraverse(dyn_cast<Instruction>(v), FT, g - 1, 1);
      }
    }
  }
}

void Hello::STCall(Instruction *ci, Value *A, FuncTraverse *FT, int g) {
  int i, j;
  CallSite cs = CallSite(ci);
  if (!cs.isIndirectCall()) {
    if (cs.getCalledFunction()) {
      StringRef n = cs.getCalledFunction()->getName();
      if (n == "vcall_reference_monitor" || n == "pcall_reference_monitor" || n == "update_mpx_table") {return;}
      if (isa<CallInst>(FT->TInst[FT->rci]) || isa<InvokeInst>(FT->TInst[FT->rci])) {
        CallSite csf = CallSite(FT->TInst[FT->rci]);
        if (csf.getCalledValue() == cs.getCalledValue()) {return;}
      }
      for (i = 0; i < cs.getNumArgOperands(); i++) {
        if (cs.getArgOperand(i) == A) {break;}
      }
      j = cs.getCalledFunction()->arg_end() - cs.getCalledFunction()->arg_begin();
      if (j < cs.getNumArgOperands()) {return;}
      if (isInFuncALL(ci, i)) {return;}
      STFunction(cs.getCalledFunction(), FT, g, i, ci);
    } else {
      int k = isa<CallInst>(ci) ? 1 : 3;
      Value *CV = ci->getOperand(ci->getNumOperands() - k);
      if (isa<BitCastOperator>(CV)) {
        Function *CF = dyn_cast<Function>(CV->stripPointerCasts());
        if (isa<CallInst>(FT->TInst[FT->rci]) || isa<InvokeInst>(FT->TInst[FT->rci])) {
          CallSite csf = CallSite(FT->TInst[FT->rci]);
          if (csf.getCalledValue() == cs.getCalledValue()) {return;}
        }
        for (i = 0; i < cs.getNumArgOperands(); i++) {
          if (cs.getArgOperand(i) == A) {break;}
        }
        j = CF->arg_end() - CF->arg_begin();
        if (j < cs.getNumArgOperands()) {return;}
        if (isInFuncALL(ci, i)) {return;}
        STFunction(CF, FT, g, i, ci);
      } else if (isa<GlobalValue>(CV)) {
        Function *CF = dyn_cast<Function>(CV->stripPointerCastsAndInvariantGroups());
        if (isa<CallInst>(FT->TInst[FT->rci]) || isa<InvokeInst>(FT->TInst[FT->rci])) {
          CallSite csf = CallSite(FT->TInst[FT->rci]);
          if (csf.getCalledValue() == cs.getCalledValue()) {return;}
        }
        for (i = 0; i < cs.getNumArgOperands(); i++) {
          if (cs.getArgOperand(i) == A) {break;}
        }
        j = CF->arg_end() - CF->arg_begin();
        if (j < cs.getNumArgOperands()) {return;}
        if (isInFuncALL(ci, i)) {return;}
        STFunction(CF, FT, g, i, ci);
      }
    }
  } else {//indirect call/invoke
    if (cs.getCalledValue() == A) {return;} 
    if (isTypeSame(FT->TInst[FT->rci], ci)) {return;}
    for (i = 0; i < cs.getNumArgOperands(); i++) {
      if (cs.getArgOperand(i) == A) {break;}
    }
    if (isInFuncALL(ci, i)) {return;}
    for (auto f : Funcset) {
      if (isTypeMatch(ci, f)) {
        j = f->arg_end() - f->arg_begin();
        if (j < cs.getNumArgOperands()) {return;}
        STFunction(f, FT, g, i, ci);
      }
    }
  }
}

void Hello::StoreToCall(Instruction *S, FuncTraverse *FT, Value *V) {
  if (FT->FName[FT->FNameNum].ArgState == -1 && FT->FName[FT->FNameNum - 1].ArgState == -3) {
    return ;
  }
  Module *M = S->getModule();
  Function *F = S->getFunction();
  if (V == NULL) { //for 447 call(%0)
    V = dyn_cast<StoreInst>(S)->getValueOperand();
  }
  for (auto I : Callset) {
    CallSite cs = CallSite(I);
    if (isNotRepeatFunction(I, ArginFunction(V))) {
      if (cs.getCalledFunction()) {
        if (cs.getCalledFunction() == F) {
          CallToAlloca(ArginFunction(V), I, FT);
        }
      } else {
        int k = isa<CallInst>(I) ? 1 : 3;
        Value *CV = I->getOperand(I->getNumOperands() - k);
        if (isa<BitCastOperator>(CV)) {
          Function *CF = dyn_cast<Function>(CV->stripPointerCasts());
          if (CF == F) {
            CallToAlloca(ArginFunction(V), I, FT);
          }
        } else if (isa<InlineAsm>(CV)) {
        } else if (isa<GlobalValue>(CV) && isa<PointerType>(CV->getType())) {
          Function *CF =
              dyn_cast<Function>(CV->stripPointerCastsAndInvariantGroups());
          if (CF == F) {
            CallToAlloca(ArginFunction(V), I, FT);
          }
        } 
      }
    }
  }
  for (auto I : ICallset) {
    if (isTypeMatch(I, F)
        && isNotRepeatFunction(I, ArginFunction(V))) {
      CallToAlloca(ArginFunction(V), I, FT);
    }
  }
}

void Hello::GlobalUse(Value *G, FuncTraverse *FT, int i) {
  for (auto *g : G->users()) {
    if (isa<Instruction>(g)) {
      if (isa<StoreInst>(g)) {
        Value *V = dyn_cast<StoreInst>(g)->getValueOperand();
        if (G == V) { continue; }
        if (isa<Constant>(V) && dyn_cast<Constant>(V)->isNullValue()) { continue; }
        FT->globaluse += 1;
        if (FT->FNameNum > 1 && FT->FName[FT->FNameNum - 1].ArgState == -3) {
          beforestore = true;
           V_f++;
          //emit
          if(isa<PointerType>(dyn_cast<StoreInst>(g)->getValueOperand()->getType()))
            EmitStoreOrigin(cast<StoreInst>(g), FT, i);
        } else {
          beforeStore(dyn_cast<StoreInst>(g), FT);
          if (beforestore) {
            beforestore = false;
          } else {
             V_f++;
            //emit
            if(isa<PointerType>(dyn_cast<StoreInst>(g)->getValueOperand()->getType()))
             EmitStoreOrigin(cast<StoreInst>(g), FT, i);
          }
        }
      } else if (isa<LoadInst>(g)){
        StoreTraverseG(dyn_cast<Instruction>(g), FT, i, 0);
      } else if (isa<GetElementPtrInst>(g)) {
        if (i >= 0 && isGepOffsetEqual1(g, FT->GInst[i], FT)) {
          StoreTraverseG(dyn_cast<Instruction>(g), FT, i - 1, 0);
        }
      } else if (isa<CallInst>(g)) {
        STCallG(dyn_cast<Instruction>(g), G, FT, i);
      } else if (isa<InvokeInst>(g)) {
        STCallG(dyn_cast<Instruction>(g), G, FT, i);
      } else if (isa<PHINode>(g)) {
      } else if (isa<ReturnInst>(g)){
      }
    } else {
      if (isa<GEPOperator>(g)) {
        if (i >= 0 && isGepOffsetEqual1(g, FT->GInst[i], FT)) {
          GlobalUse(g, FT, i - 1);
        }
      } else {
        GlobalUse(g, FT, i);
      }
    }
  }
}

  
bool Hello::isGepOffsetEqual1(llvm::Value *I, Instruction *G, FuncTraverse *FT) {
  GEPOperator *g1 = dyn_cast<GEPOperator>(I),
      *g2;
  Value *V = G;
  while(1) {
    if (isa<GEPOperator>(V)) {
      g2 = dyn_cast<GEPOperator>(V);
      break;
    } else if (isa<CallInst>(V)) {
      V = dyn_cast<CallInst>(V)->getOperand(FT->FName[FT->FNameNum].ArgState);
    } else {
      V = dyn_cast<Operator>(V)->getOperand(0);
    }
  }
  int i1 = g1->getNumOperands(), i2 = g2->getNumOperands();
  if (i1 == i2) {
    if (isa<Constant>(g1->getOperand(i1 - 1)) && isa<Constant>(g2->getOperand(i2 - 1))) {
        if (g1->getOperand(i1 - 1) == g2->getOperand(i2 - 1)) {
        return true;
      }
      return false;
    }
    return true ;
  }
  return false ;
}
bool Hello::isGepOffsetEqual(Value *I, Value *G) {
  bool i;
  if (get_struct_mem_offset(I) == get_struct_mem_offset(G)) {
    i = true;
  } else {
    i = false;
  }
  return i;
}
int64_t Hello::get_struct_mem_offset(Value *getelementptr_inst) {
  int64_t former_offset = 0 ,after_offset = 0;
  APInt ap_offset(64,0,true);
  auto *dl = new DataLayout(dyn_cast<GetElementPtrInst>(getelementptr_inst)->getModule());

  former_offset = ap_offset.getSExtValue();
  auto cal = dyn_cast<GEPOperator>(getelementptr_inst)->accumulateConstantOffset(*dl, ap_offset);
  if(cal) {
    after_offset = ap_offset.getSExtValue();
    return after_offset - former_offset;
  } else {
    errs() << "Failed to get offset!\n";
    return -1;
  }
}
int Hello::ArginFunction(Value *V){
  int i = -3;
  if (isa<Argument>(V))
    i = dyn_cast<Argument>(V)->getArgNo();
  return i;
}
bool Hello::isTypeSame(llvm::Instruction *c, llvm::Instruction *I) {
  int ncArg = 0, nIArg = 0, i;
  std::vector<const Type *> cArgList, IArgList;
  if (isa<CallInst>(c) && isa<CallInst>(I)) {
    const CallInst *cBase = dyn_cast<CallInst>(c);
    const CallInst *IBase = dyn_cast<CallInst>(I);
    if (cBase->getFunctionType()->getReturnType() != IBase->getFunctionType()->getReturnType()) { return false;}
    ncArg = cBase->getNumArgOperands();
    nIArg = IBase->getNumArgOperands();
    if (ncArg != nIArg) { return false;}
    for (i = 0; i < ncArg; i++) {
      const Value *arg = cBase->getArgOperand(i);
      const Type *argType = arg->getType();
      cArgList.push_back(argType);
    }
    for (i = 0; i < nIArg; i++) {
      const Value *arg = IBase->getArgOperand(i);
      const Type *argType = arg->getType();
      IArgList.push_back(argType);
    }
    for (i = 0; i < ncArg; i++) {
      if (cArgList[i] != IArgList[i]) { return false;}
    }
    return true;
  } else if (isa<InvokeInst>(c) && isa<InvokeInst>(I)){
    const InvokeInst *cBase = dyn_cast<InvokeInst>(c);
    const InvokeInst *IBase = dyn_cast<InvokeInst>(I);
    if (cBase->getFunctionType()->getReturnType() != IBase->getFunctionType()->getReturnType()) { return false;}
    ncArg = cBase->getNumArgOperands();
    nIArg = IBase->getNumArgOperands();
    if (ncArg != nIArg) { return false;}
    for (i = 0; i < ncArg; i++) {
      const Value *arg = cBase->getArgOperand(i);
      const Type *argType = arg->getType();
      cArgList.push_back(argType);
    }
    for (i = 0; i < nIArg; i++) {
      const Value *arg = IBase->getArgOperand(i);
      const Type *argType = arg->getType();
      IArgList.push_back(argType);
    }
    for (i = 0; i < ncArg; i++) {
      if (cArgList[i] != IArgList[i]) { return false;}
    }
    return true;
  }
  return false;
}
bool Hello::isTypeMatch(const Instruction *sink, const Value *source) {
  int nFnArg = 0, nCallArg = 0;
  std::vector<const Type *> fnArgList, callArgList;
  if (isa<Function>(source)) {
    const Function *fn = dyn_cast<Function>(source);
    const Type *rTy = fn->getReturnType();
    nFnArg = fn->arg_size();
    for (Function::const_arg_iterator AI = fn->arg_begin(), AE = fn->arg_end();
         AI != AE; ++AI) {
      const Value *arg = AI;
      const Type *argType = arg->getType();
      fnArgList.push_back(argType);
    }
    if (isa<CallInst>(sink)) {
      const CallInst *cBase = dyn_cast<CallInst>(sink);
      if (cBase->getFunctionType()->getReturnType() != rTy) {
        return false;
      }
      nCallArg = cBase->getNumArgOperands();
      for (int i = 0; i < cBase->getNumArgOperands(); i++) {
        const Value *arg = cBase->getArgOperand(i);
        const Type *argType = arg->getType();
        callArgList.push_back(argType);
      }
    } else if (isa<InvokeInst>(sink)) {
      const InvokeInst *cBase = dyn_cast<InvokeInst>(sink);
      if (cBase->getFunctionType()->getReturnType() != rTy) {
        return false;
      }
      nCallArg = cBase->getNumArgOperands();
      for (int i = 0; i < cBase->getNumArgOperands(); i++) {
        const Value *arg = cBase->getArgOperand(i);
        const Type *argType = arg->getType();
        callArgList.push_back(argType);
      }
    }
    if (nFnArg == nCallArg) {
      for (int i = 0; i < nFnArg; i++) {
        if (fnArgList[i] != callArgList[i]) {
          return false;
        }
      }
      return true;
    }
  } else {
    return true;
  }
  return false;
}
bool Hello::isNotRepeatFunction(Function *F) {
  for (int i = FuncAllNum; i >= 0; i --) {
    if (FuncAll[i].ArgState == -1 && FuncAll[i].FunctionName == F->getName()) {
      return false;
    }
  }
  return true;
}
bool Hello::isNotRepeatFunction(Instruction *I, int offset) {
  for (int i = FuncAllNum; i >= 0; i--) {
    if (FuncAll[i].FunctionName == I->getFunction()->getName() &&
        FuncAll[i].CallInstruction == I && FuncAll[i].ArgState == offset) {
      return false;
    }
  }
  return true;
}
bool Hello::isInFuncALL(const llvm::Instruction *ci, int i) {
  for (int j = 0; j <= FuncAllNum; j++) {
    if (FuncAll[j].CallInstruction == ci && FuncAll[j].ArgState == i) {
      return true;
    }
  }
  return false;
}
bool Hello::isInSTFV(llvm::Function *F, int i) {
  for (int j = 0; j <= STFNum; j++) {
    if (STFV[j].F == F && STFV[j].offset == i) {
      return true;
    }
  }
  return false;
}
bool Hello::isInSTFVG(llvm::Function *F, int i) {
  for (int j = 0; j <= STFGNum; j++) {
    if (STFVG[j].F == F && STFVG[j].offset == i) {
      return true;
    }
  }
  return false;
}
bool Hello::isInCAGV(llvm::GlobalVariable *G) {
  for (int i = 0; i < CAGV.size(); i++) {
    if (G == CAGV[i]) {
      return true;
    }
  }
  return false;
}
Instruction * Hello::getReturn(Function *F) {
  Instruction *I;
  for (BasicBlock &b : *F) {
    for (auto &i : b) {
      I = &i;
      if (isa<ReturnInst>(I)) {
        return I;
      }
    }

  }
}

void Hello::StoreTraverseG(Instruction *A, FuncTraverse *FT, int g, int flag) {
  Instruction *I, *Pre = FT->TInst[FT->TInstNum - 1];
  for (auto u : dyn_cast<Value>(A)->users()) {
    if (FT->dt == 1)
      break;
    if (isa<Instruction>(u)) {
      I = dyn_cast<Instruction>(u);
      if (I == Pre)
        continue;
      if (isa<LoadInst>(I)) {
        StoreTraverseG(I, FT, g, flag);
      } else if (isa<GetElementPtrInst>(I)) {
        if (g < 0) 
          continue;
        if (isGepOffsetEqual1(I, FT->GInst[g], FT))
          StoreTraverseG(I, FT, g - 1, flag);
      } else if (isa<BitCastInst>(u)) {
        StoreTraverseG(I, FT, g, flag);
      } else if (isa<PtrToIntInst>(u)) {
        StoreTraverseG(I, FT, g, flag);
      } else if (isa<CallInst>(u)) {
        STCallG(I, A, FT, g);
      } else if (isa<InvokeInst>(u)) {
        STCallG(I, A, FT, g);
      } else if (isa<ReturnInst>(u)) {
      } else if (isa<ZExtInst>(u)) {
      } else if (isa<TruncInst>(u)) {
      } else if (isa<FCmpInst>(u)) {
      } else if (isa<AddOperator>(u)) {
      } else if (isa<ICmpInst>(u)) {
      } else if (isa<SubOperator>(u)) {
      } else if (isa<PHINode>(u)) {
      } else if (isa<SExtInst>(u)) {
      } else if (isa<FPExtInst>(u)) {
      } else if (dyn_cast<Instruction>(u)->getOpcode() == 21) { // srem
      } else if (dyn_cast<Instruction>(u)->getOpcode() == 26) /*and*/ {
      } else if (dyn_cast<Instruction>(u)->getOpcode() == 27) /*or*/ {
      } else if (dyn_cast<Instruction>(u)->getOpcode() == Instruction::FDiv) {
      } else if (isa<StoreInst>(u)) {
        StoreInst *S = dyn_cast<StoreInst>(I);
        Value *V = S->getValueOperand();
        if (A == V) {continue; }
        if (isa<Constant>(V) && dyn_cast<Constant>(V)->isNullValue()) { continue; }
        signed int arg_num = ArginFunction(V);
        if (flag == 1){
          if (arg_num < 0) {
            FT->globaluse += 1;
            if (FT->FNameNum > 1 && FT->FName[FT->FNameNum - 1].ArgState == -3) {
              beforestore = true;
            V_f++;
              //emitstore
              EmitStoreOrigin(S,FT,g);
            } else {
              beforeStore(S, FT);
              if (beforestore) {
                beforestore = false;
              } else {
                 V_f++;
                //emitstore
                EmitStoreOrigin(S,FT,g);
              }
            }
          }
        } else {
          if (arg_num >= 0) {
            FT->InstAssignment(I);
            if (FT->FName[FT->FNameNum].ArgState == -1) {
              if (isNotRepeatFunction(FT->TInst[FT->rci - 1], -1)) {
                CallToAlloca(ArginFunction(V), FT->TInst[FT->rci - 1], FT);
              }
            } else {
              StoreToCall(S, FT);
            }
          } else {
            DominatorTree *DT = new DominatorTree(*S->getFunction());
            FT->dt = DT->dominates(S, Pre);
            FT->globaluse += 1;
            if (FT->FNameNum > 1 && FT->FName[FT->FNameNum - 1].ArgState == -3) {
              beforestore = true;
               V_f++;
              //emitstore
              EmitStoreOrigin(S,FT,g);
            } else {
              beforeStore(S, FT);
              if (beforestore) {
                beforestore = false;
              } else {
                 V_f++;
                //emitstore
                EmitStoreOrigin(S,FT,g);
              }
            }
          }
        }
      }
    }
  }
}

void Hello::STFunctionG(llvm::Function *F, FuncTraverse *FT, int g, int i, Instruction *CI) {
  if (isInSTFVG(F, i)) {return;}
  STFVG.push_back({i, F});
  STFGNum++;
  auto argv = F->arg_begin() + i;
  for (auto v: argv->users()) {
    if (isa<StoreInst>(v)) {
      StoreTraverseG(dyn_cast<Instruction>(dyn_cast<StoreInst>(v)->getPointerOperand()), FT, g, 1);
    } else if (isa<CallInst>(v)) {
      CallInst *ci = dyn_cast<CallInst>(v);
      STCallG(ci, argv, FT, g);
    } else if (isa<InvokeInst>(v)) {
      InvokeInst *ci = dyn_cast<InvokeInst>(v);
      STCallG(ci, argv, FT, g);
      SRG.pop_back();
    } else if (isa<GetElementPtrInst>(v)) {
      if (g >= 0 && isGepOffsetEqual1(v, FT->GInst[g], FT)) {
        StoreTraverseG(dyn_cast<Instruction>(v), FT, g - 1, 1);
      }
    }
  }
}

void Hello::STCallG(Instruction *ci, Value *A, FuncTraverse *FT, int g) {
  int i, j;
  CallSite cs = CallSite(ci);

  if (!cs.isIndirectCall()) {
    if (cs.getCalledFunction()) {
      StringRef n = cs.getCalledFunction()->getName();
      if (n == "vcall_reference_monitor" || n == "pcall_reference_monitor" || n == "update_mpx_table") {return;}
      if (isa<CallInst>(FT->TInst[FT->rci]) || isa<InvokeInst>(FT->TInst[FT->rci])) {
        CallSite csf = CallSite(FT->TInst[FT->rci]);
        if (csf.getCalledValue() == cs.getCalledValue()) {return;}
      }
      for (i = 0; i < cs.getNumArgOperands(); i++) {
        if (cs.getArgOperand(i) == A) {break;}
      }
      j = cs.getCalledFunction()->arg_end() - cs.getCalledFunction()->arg_begin();
      if (j < cs.getNumArgOperands()) {return;}
      if (isInFuncALL(ci, i)) {return;}
      STFunctionG(cs.getCalledFunction(), FT, g, i, ci);
    } else {
      int k = isa<CallInst>(ci) ? 1 : 3;
      Value *CV = ci->getOperand(ci->getNumOperands() - k);
      if (isa<BitCastOperator>(CV)) {
        Function *CF = dyn_cast<Function>(CV->stripPointerCasts());
        if (isa<CallInst>(FT->TInst[FT->rci]) || isa<InvokeInst>(FT->TInst[FT->rci])) {
          CallSite csf = CallSite(FT->TInst[FT->rci]);
          if (csf.getCalledValue() == cs.getCalledValue()) {return;}
        }
        for (i = 0; i < cs.getNumArgOperands(); i++) {
          if (cs.getArgOperand(i) == A) {break;}
        }
        j = CF->arg_end() - CF->arg_begin();
        if (j < cs.getNumArgOperands()) {return;}
        if (isInFuncALL(ci, i)) {return;}
        STFunctionG(CF, FT, g, i, ci);
      } else if (isa<GlobalValue>(CV)) {
        Function *CF = dyn_cast<Function>(CV->stripPointerCastsAndInvariantGroups());
        if (isa<CallInst>(FT->TInst[FT->rci]) || isa<InvokeInst>(FT->TInst[FT->rci])) {
          CallSite csf = CallSite(FT->TInst[FT->rci]);
          if (csf.getCalledValue() == cs.getCalledValue()) {return;}
        }
        for (i = 0; i < cs.getNumArgOperands(); i++) {
          if (cs.getArgOperand(i) == A) {break;}
        }
        j = CF->arg_end() - CF->arg_begin();
        if (j < cs.getNumArgOperands()) {return;}
        if (isInFuncALL(ci, i)) {return;}
        STFunctionG(CF, FT, g, i, ci);
      }
    }
  } else {//indirect call/invoke
    if (cs.getCalledValue() == A) {return;}
    if (isTypeSame(FT->TInst[FT->rci], ci)) {return;}
    for (i = 0; i < cs.getNumArgOperands(); i++) {
      if (cs.getArgOperand(i) == A) {break;}
    }
    if (isInFuncALL(ci, i)) {return;}
    Module *M = ci->getModule();
    for (auto f: Funcset) {
      if (isTypeMatch(ci, f)) {
        j = f->arg_end() - f->arg_begin();
        if (j < cs.getNumArgOperands()) {return;}
        STFunctionG(f, FT, g, i, ci);
      }
    }
  }
}

void Hello::beforeStore(llvm::StoreInst *S, FuncTraverse *FT) {
  Value *V = S->getValueOperand();
  if (isa<CallInst>(V) || isa<InvokeInst>(V)) {
    CallSite CS = CallSite(V);
    if ((!(CS.isIndirectCall() && isTypeSame(dyn_cast<Instruction>(V), FT->TInst[0]))) && isNotRepeatFunction(S, -3)) {
      CallToAlloca(-3, S, FT);
    }
  } else {
    beforestore = false;
  }
}

void Hello::EmitVcallOrigin(StoreInst *Origin_Inst, FuncTraverse* FT) {
  Value *Val = Origin_Inst->getValueOperand();
  Value* Addr = Origin_Inst->getPointerOperand();
  Instruction* insert = Origin_Inst->getNextNode();
  if(!isa<BitCastOperator>(Val)) {
    return;
  }
  Val = cast<BitCastOperator>(Val)->getOperand(0);
  if(!isa<GEPOperator>(Val)) {
    return;
  }

  GEPOperator* Vptr_Gep_Origin = cast<GEPOperator>(Val);
  GlobalVariable* global_var = dyn_cast<GlobalVariable>(Vptr_Gep_Origin->getOperand(0));
  if(global_var->hasExternalLinkage()) {
    return;
  }
  Constant* global_init = dyn_cast<GlobalVariable>(global_var)->getInitializer();
  if(isa<ConstantAggregateZero>(global_init)) {
    return;
  }
  
  llvm::ConstantDataArray* array_var = cast<ConstantDataArray>(global_init);
  assert(array_var != nullptr);

  Type *global_type = global_var->getType();
  while (global_type->isPointerTy())
      global_type = global_type->getPointerElementType();

  auto array_type = dyn_cast<ArrayType>(global_type);
  assert(array_type != nullptr);

  LLVMContext context;
  IRBuilder<> Builder(context);
  Builder.SetInsertPoint(Origin_Inst);
  
  Value* Vcall_to_Target;
  unsigned long Vptr_Offset = dyn_cast<ConstantInt>(Vptr_Gep_Origin->getOperand(2))->getZExtValue();
  ConstantStruct* CS = dyn_cast<ConstantStruct>(global_init);
  ConstantArray* Vtable_Array = dyn_cast<ConstantArray>(CS->getOperand(Vptr_Offset));

  //get ptr_value
  for(int i = 0;i <=FT->TInstNum; i++) {
    if(isa<GetElementPtrInst>(FT->TInst[i])) {
      unsigned long Target_Offset = 
        dyn_cast<ConstantInt>(FT->TInst[i]->getOperand(1))->getZExtValue() + 2;
      if(Target_Offset >= Vtable_Array->getNumOperands()) {
        return;
      }

      Vcall_to_Target = Vtable_Array->getAggregateElement(Target_Offset);
      if(isa<BitCastOperator>(Vcall_to_Target)) {
        Vcall_to_Target = dyn_cast<BitCastOperator>(Vcall_to_Target)->getOperand(0);
      }
    }
  }
  
  for(int i = FT->TInstNum;i >= 0; i--) {
    if(isa<LoadInst>(FT->TInst[i])) {
      unsigned int load_alignment = dyn_cast<LoadInst>(FT->TInst[i])->getAlignment();
      Type *load_type = dyn_cast<LoadInst>(FT->TInst[i])->getPointerOperandType();
      if(load_type != Origin_Inst->getPointerOperand()->getType())
        Addr = Builder.CreateBitCast(Addr,load_type);
      Addr = Builder.CreateAlignedLoad(Addr,load_alignment);
    }else if(isa<GetElementPtrInst>(FT->TInst[i])) {
      std::vector<Value *> indexList = {};
        
      unsigned int offset_size = dyn_cast<GEPOperator>(FT->TInst[i])->getNumOperands();
      unsigned int gep_argcounter = 0; 

      while(gep_argcounter < (offset_size-1)) {
        Value *arg = dyn_cast<GEPOperator>(FT->TInst[i])->getOperand(gep_argcounter+1);
          indexList.push_back(arg);
        gep_argcounter++;
      }
      
      Addr = Builder.CreateInBoundsGEP(Addr, indexList);
      break;
    }     
  }

  EmitUpdateCall(Origin_Inst,Vcall_to_Target,Addr,insert,FT);
}

void Hello::EmitStoreOrigin(StoreInst *Origin_Store, FuncTraverse *FT, int type_gep_offset) {
  Instruction *OriginInst = Origin_Store;
  Instruction *insert = Origin_Store->getNextNode();

  Value *Val = Origin_Store->getValueOperand();
  Value *Addr = Origin_Store->getPointerOperand();

  while( isa<BitCastOperator>(Val) ) {
    Val = dyn_cast<BitCastOperator>(Val)->getOperand(0);
  }
  
  
  while( isa<BitCastOperator>(Addr) ) {
    Addr = dyn_cast<BitCastOperator>(Addr)->getOperand(0);
  }

  if(!isa<PointerType>(Val->getType()) || !isa<PointerType>(Addr->getType()))
    return;

  int callos_flag = 0, icont_flag = 0, gep_count = FT->TInstNum , TInstNum = 0 ;
  
    
  if(FT->TInstNum > 0 && !Val->getType()->getPointerElementType()->isFunctionTy()) {
    for( ; gep_count > 0; gep_count--) {
      if(!isa<GEPOperator>(FT->TInst[gep_count]))
        continue;

      int offset_size = dyn_cast<GEPOperator>(FT->TInst[gep_count])->getNumOperands();
      int arg_count = 0;
      Value *arg = dyn_cast<GEPOperator>(FT->TInst[gep_count])->getOperand(offset_size-1);
      
      if( !isa<Constant>(arg) ) {
        if(FT->TInst[gep_count]->getFunction() != OriginInst->getFunction()) {
          callos_flag = gep_count;
        } else 
          icont_flag = gep_count;
      }
    }
  }

  if(callos_flag) {
    Addr = FT->TInst[callos_flag]->getOperand(0);
    while( isa<BitCastOperator>(Addr) ) {
      Addr = dyn_cast<BitCastOperator>(Addr)->getOperand(0);
    }
    insert = FT->TInst[callos_flag]->getNextNode();
    FT->TInstNum = callos_flag;
    EmitUpdateCall(OriginInst,Val, Addr, insert, FT);
  } else if (icont_flag) {
    insert =  FT->TInst[icont_flag]->getNextNode();
    EmitUpdateCall(OriginInst, Val, Addr, insert, FT, type_gep_offset);
  } else {
    EmitUpdateCall(OriginInst, Val, Addr, insert, FT, type_gep_offset);
  } 

}

void Hello::EmitGlobalOrigin(Value *OriginInst, FuncTraverse *FT, GlobalVariable *globalvl) {
  if (isa<LoadInst>(OriginInst)) {
    EmitLoadOrigin(cast<LoadInst>(OriginInst), FT, globalvl);
  } else if (isa<CallInst>(OriginInst)) {  
    EmitCallOrign(cast<CallInst>(OriginInst), FT, globalvl);
  } else if (isa<GetElementPtrInst>(OriginInst)) {
    EmitGepOrigin(cast<GetElementPtrInst>(OriginInst), FT, globalvl);
  } else if (isa<StoreInst>(OriginInst) ) {
    errs() << "gep_inst is storeinst!\n";
  } else if (!isa<Instruction>(OriginInst)) {
    errs() << "NOT Instruction!" << *OriginInst << "\n";   
  }
}

void Hello::EmitGepOrigin(GetElementPtrInst *OriginInst, FuncTraverse *FT, GlobalVariable *globalvl) {
  llvm::Value *Addr = globalvl;
  llvm::Value *Val = OriginInst;
  Instruction *insert = OriginInst->getNextNode();
  llvm::Value *ctx = NULL;
  int callos_flag = 0, icont_flag = 0, gep_count = FT->TInstNum-1 ; 
    
  if(FT->TInstNum > 0) { 
    for( ; gep_count > 0; gep_count--) {
      if(!isa<GEPOperator>(FT->TInst[gep_count]))
        continue;

      int offset_size = dyn_cast<GEPOperator>(FT->TInst[gep_count])->getNumOperands();
      int arg_count = 0;
      Value *arg = dyn_cast<GEPOperator>(FT->TInst[gep_count])->getOperand(offset_size-1);
      
      if( !isa<Constant>(arg) ) {
        if(FT->TInst[gep_count]->getFunction() != OriginInst->getFunction()) {
          callos_flag = gep_count;
        } else {
          icont_flag = gep_count; 
        }
      }
    }
  }

  if(callos_flag) {
    Addr = FT->TInst[callos_flag]->getOperand(0);
    while( isa<BitCastOperator>(Addr) ) {
      Addr = dyn_cast<BitCastOperator>(Addr)->getOperand(0);
    }
    insert = FT->TInst[callos_flag]->getNextNode();
    FT->TInstNum = callos_flag; 
    EmitUpdateCall(OriginInst,Val, Addr, insert, FT);
  } else if (icont_flag) {
    insert =  FT->TInst[icont_flag]->getNextNode();
    int offset_size = OriginInst->getNumOperands();
    ctx = isa<ConstantInt>(OriginInst->getOperand(offset_size-1))? NULL:OriginInst->getOperand(offset_size-1);
    EmitUpdateCall(OriginInst, Val, Addr, insert, FT, 0, ctx);
  } else {
    int offset_size = OriginInst->getNumOperands();
    ctx = isa<ConstantInt>(OriginInst->getOperand(offset_size-1))? NULL:OriginInst->getOperand(offset_size-1);
    
    EmitUpdateCall(OriginInst, Val, Addr, insert, FT, 0, ctx);
  }
}


void Hello::EmitLoadOrigin(LoadInst *OriginInst, FuncTraverse *FT, GlobalVariable *globalvl) {
  Instruction *insert = OriginInst->getNextNode();
  Value *Val = OriginInst;
  Value *Addr = globalvl;
  Value *ctx = NULL;

  int callos_flag = 0, icont_flag = 0, gep_count = FT->TInstNum ; 
    
  if(FT->TInstNum > 0) {
    for( ; gep_count > 0; gep_count--) {
      if(!isa<GEPOperator>(FT->TInst[gep_count]))
        continue;

      int offset_size = dyn_cast<GEPOperator>(FT->TInst[gep_count])->getNumOperands();
      int arg_count = 0;
      Value *arg = dyn_cast<GEPOperator>(FT->TInst[gep_count])->getOperand(offset_size-1);
      
      if( !isa<Constant>(arg) ) {
        if(FT->TInst[gep_count]->getFunction() != OriginInst->getFunction()) {
          callos_flag = gep_count;
        } else {
          icont_flag = gep_count;
        }
      }
    }
  }

  if(callos_flag) {
    Addr = FT->TInst[callos_flag]->getOperand(0);
    while( isa<BitCastOperator>(Addr) ) {
      Addr = dyn_cast<BitCastOperator>(Addr)->getOperand(0);
    }

    insert = FT->TInst[callos_flag]->getNextNode();
    FT->TInstNum = callos_flag;
    EmitUpdateCall(OriginInst,Val, Addr, insert, FT);
  } else if (icont_flag) {
    insert =  FT->TInst[icont_flag]->getNextNode();
    if( isa<GEPOperator>(OriginInst->getPointerOperand()) ) {
      GEPOperator *gep_vl = dyn_cast<GEPOperator>(OriginInst->getPointerOperand());
      int offset_size = gep_vl->getNumOperands();
      if(!isa<ConstantInt>(gep_vl->getOperand(offset_size-1)))
        ctx = gep_vl->getOperand(offset_size-1);
    }
    EmitUpdateCall(OriginInst, Val, Addr, insert, FT, 0, ctx);
  } else {
    if( isa<GEPOperator>(OriginInst->getPointerOperand()) ) {
      GEPOperator *gep_vl = dyn_cast<GEPOperator>(OriginInst->getPointerOperand());
      int offset_size = gep_vl->getNumOperands();
      if(!isa<ConstantInt>(gep_vl->getOperand(offset_size-1)))
        ctx = gep_vl->getOperand(offset_size-1);
    }
    EmitUpdateCall(OriginInst,Val,Addr,insert,FT,0,ctx);
  }
}

void Hello::EmitPhiOrign(PHINode *Origin_Phi, FuncTraverse *FT) {
  Instruction *OriginInst = dyn_cast<Instruction>(Origin_Phi);
  Value *Addr = NULL;
  Value *Val = OriginInst;
  while( isa<BitCastOperator>(Val) ) {
    Val = dyn_cast<BitCastOperator>(Val)->getOperand(0);
  }
  Instruction *insert = OriginInst->getNextNode();
  EmitUpdateCall(OriginInst, Val, Addr, insert, FT);
}

void Hello::EmitSelectOrign(SelectInst *Origin_Select, FuncTraverse *FT) {
  Instruction *OriginInst = dyn_cast<Instruction>(Origin_Select);
  Value *Addr = NULL;

  Value *Val = OriginInst;
  while( isa<BitCastOperator>(Val) ) {
    Val = dyn_cast<BitCastOperator>(Val)->getOperand(0);
  }
  Instruction *insert = OriginInst->getNextNode();
  EmitUpdateCall(OriginInst, Val, Addr, insert, FT);
}

void Hello::EmitCallOrign(CallInst *Origin_Call, FuncTraverse *FT,Value *ptr_val) {
  Instruction *OriginInst = Origin_Call;
  Value *Val = ptr_val;
  while( isa<BitCastOperator>(Val) ) {
    Val = dyn_cast<BitCastOperator>(Val)->getOperand(0);
  }
  Value *Addr = NULL;
  Instruction *insert = OriginInst;
  
  int callos_flag = 0, icont_flag = 0, gep_count = FT->TInstNum , TInstNum = 0;
  
  if( isa<GlobalVariable>(Val) ) {
    Addr = Val;
  }
    
  if(FT->TInstNum > 0 && !Val->getType()->getPointerElementType()->isFunctionTy()) {
    for( ; gep_count > 0; gep_count--) {
      if(!isa<GEPOperator>(FT->TInst[gep_count]))
        continue;

      int offset_size = dyn_cast<GEPOperator>(FT->TInst[gep_count])->getNumOperands();
      int arg_count = 0;
      Value *arg = dyn_cast<GEPOperator>(FT->TInst[gep_count])->getOperand(offset_size-1);
      
      if( !isa<Constant>(arg) ) {
        if(dyn_cast<Instruction>( FT->TInst[gep_count] )->getFunction() != OriginInst->getFunction()) {
          callos_flag = gep_count;
        } else 
          icont_flag = gep_count;
      }
    }
  }

  if(callos_flag) {
    Addr = FT->TInst[callos_flag]->getOperand(0);
    while( isa<BitCastOperator>(Addr) ) {
      Addr = dyn_cast<BitCastOperator>(Addr)->getOperand(0);
    }
    insert = dyn_cast<Instruction>(FT->TInst[callos_flag])->getNextNode();
    FT->TInstNum = callos_flag;
    EmitUpdateCall(OriginInst,Val, Addr, insert,FT);
  } else if (icont_flag ) {
    insert =  FT->TInst[icont_flag]->getNextNode();
    EmitUpdateCall(OriginInst, Val, Addr, insert, FT);
  } else {
    EmitUpdateCall(OriginInst, Val, Addr, insert, FT);
  } 

}


void Hello::EmitUpdateCall(Instruction *Origin_Inst, Value *ptr_OriginVal, Value *ptr_OriginAddr, Instruction *InsertPoint,  FuncTraverse *FT, int type_gep_offset, Value* ctx){
  Module *M = Origin_Inst->getModule();
  LLVMContext &context = M->getContext();
  IRBuilder<> Builder(context);
  Builder.SetInsertPoint(InsertPoint);
  int gep_num = FT->GepNum;
  Value *ctx_val_64 = Builder.getInt64(0);
  Value *mpx_Index = ptr_OriginAddr;
  Value *mpx_Index_64 = NULL;
  Value *ptr_Value = ptr_OriginVal;
  if(mpx_Index && !ptr_OriginVal->getType()->getPointerElementType()->isFunctionTy()) {
    int traversed_Inst_count = FT->TInstNum;
    int over_flag = 0;
    for(; over_flag < FT->TInstNum; over_flag++) {
      if(isa<LoadInst>(FT->TInst[over_flag]))
        break;
    }
    if(FT->GepNum != type_gep_offset && type_gep_offset > 0) {
      traversed_Inst_count = FT->GInT[type_gep_offset+1] - 1;
    }
    for(; traversed_Inst_count > over_flag; traversed_Inst_count-- ) {
      int match_flag = -1;
      if(isa<GEPOperator>( FT->TInst[traversed_Inst_count] )) {
        auto *gep_vl = FT->TInst[traversed_Inst_count]->getOperand(0);
        auto *gep_val_type = gep_vl->getType();
        std::vector<Value *> indexList = {};
        unsigned int offset_size = dyn_cast<GEPOperator>(FT->TInst[traversed_Inst_count])->getNumOperands();
        unsigned int gep_argcounter = 0;
        while(gep_argcounter < (offset_size-1)) {
          Value *arg = dyn_cast<GEPOperator>(FT->TInst[traversed_Inst_count])->getOperand(gep_argcounter+1);
          indexList.push_back(arg);
          gep_argcounter++;
        }
        if(mpx_Index->getType() != gep_val_type && isa<PointerType>(mpx_Index->getType())) {
          mpx_Index = Builder.CreateBitCast(mpx_Index, gep_val_type);
        }
        mpx_Index = Builder.CreateInBoundsGEP(mpx_Index, indexList);
      } else if( isa<LoadInst>(FT->TInst[traversed_Inst_count]) ) {
        if(isa<GEPOperator>(FT->TInst[traversed_Inst_count]->getOperand(0))
            && !isa<GetElementPtrInst>(FT->TInst[traversed_Inst_count]->getOperand(0)) ) {
          std::vector<Value *> indexList = {};
          unsigned int offset_size = dyn_cast<GEPOperator>(FT->TInst[traversed_Inst_count]->getOperand(0))->getNumOperands();
          unsigned int gep_argcounter = 0;
          auto *gep_vl = dyn_cast<GEPOperator>(FT->TInst[traversed_Inst_count]->getOperand(0));
          while(gep_argcounter < (offset_size-1)) {
            Value *arg = gep_vl->getOperand(gep_argcounter+1);
            indexList.push_back(arg);
            gep_argcounter++;
          }
          if(gep_vl->getOperand(0)->getType() != mpx_Index->getType())
            mpx_Index = Builder.CreateBitCast(mpx_Index, gep_vl->getOperand(0)->getType());
            mpx_Index = Builder.CreateInBoundsGEP(mpx_Index, indexList);
        }
        if(traversed_Inst_count == match_flag) continue;
        bool Is_Store_match_Load = false;
        for( int current_count = traversed_Inst_count - 1; current_count > 1; current_count--) {
          if(isa<GEPOperator>(FT->TInst[current_count])) {
            Is_Store_match_Load = false;
            break;
          } else if( isa<LoadInst>(FT->TInst[current_count])) {
            Is_Store_match_Load = false;
            break;
          } else if( isa<StoreInst>(FT->TInst[current_count]) ) {
            match_flag = current_count;
            Is_Store_match_Load = true;
            break;
         }
        }
        if(!Is_Store_match_Load) {
          unsigned int load_alignment = dyn_cast<LoadInst>(FT->TInst[traversed_Inst_count])->getAlignment();
          Type *load_type = dyn_cast<LoadInst>(FT->TInst[traversed_Inst_count])->getPointerOperandType();
          if(load_type != mpx_Index->getType() && isa<PointerType>(mpx_Index->getType()))
            mpx_Index = Builder.CreateBitCast(mpx_Index,load_type);
          mpx_Index = Builder.CreateAlignedLoad(mpx_Index,load_alignment);
        }
      } else if( isa<StoreInst>(FT->TInst[traversed_Inst_count]) ){
        if(traversed_Inst_count == match_flag)
          continue;
        bool Is_Store_match_Load = false;
        for( int current_count = traversed_Inst_count - 1; current_count > 1; current_count--) {
          if(isa<GEPOperator>(FT->TInst[current_count])) {
            Is_Store_match_Load = false;
            break;
          } else if( isa<LoadInst>(FT->TInst[current_count])) {
            match_flag = current_count;
            Is_Store_match_Load = true;
            break;
          } else if( isa<StoreInst>(FT->TInst[current_count]) ) {
            Is_Store_match_Load = false;
            break;
          }
        }
      } else {
        continue;
      }
    }
    unsigned int load_alignment = dyn_cast<LoadInst>(FT->TInst[over_flag])->getAlignment();
    Type *load_type = dyn_cast<LoadInst>(FT->TInst[over_flag])->getPointerOperandType();
    if(load_type != mpx_Index->getType() && isa<PointerType>(mpx_Index->getType()) )
      mpx_Index = Builder.CreateBitCast(mpx_Index,load_type);
    ptr_Value = Builder.CreateAlignedLoad(mpx_Index,load_alignment);
  }
   if(ctx)
     ctx_val_64 = Builder.CreateIntCast(ctx, Type::getInt64Ty(context), false);
   Value *ptr_Value_64 = Builder.CreatePtrToInt(ptr_Value,Type::getInt64Ty(context));

   //Origin ID Creator
   std::string fn = Origin_Inst->getFunction()->getName().str();
   std::string st;
   raw_string_ostream rst(st);
   Origin_Inst->print(rst);
   std::string key = fn + st;
   unsigned long origin = hash_fn(key) % 1000000000;
   Value *p_origin = ConstantInt::get(IntegerType::getInt64Ty(context), origin, false);
   Value *p_origin_64 = Builder.CreateIntCast(p_origin, IntegerType::getInt64Ty(context), false);

   if(!mpx_Index) {
     mpx_Index_64 = FT->TInst[0]->getPrevNode()->getOperand(0);
   } else {
     mpx_Index = EmitCastToVoidPtr(mpx_Index, InsertPoint);
     mpx_Index_64 = Builder.CreatePtrToInt(mpx_Index, Type::getInt64Ty(context));
   }

   Value *icall_counter = ConstantInt::get(Type::getInt64Ty(context), (iCallNum-1), false);
   FunctionType *type = FunctionType::get(
       Type::getVoidTy(context), {Type::getInt64Ty(context), Type::getInt64Ty(context),Type::getInt64Ty(context),Type::getInt64Ty(context),Type::getInt64Ty(context)},
       false);
   Constant *constant = M->getOrInsertFunction("update_mpx_table",type);
   if(Function *fun = dyn_cast<Function>(constant)) {
     CallInst *upcall = Builder.CreateCall(fun,{mpx_Index_64, ptr_Value_64, p_origin_64, ctx_val_64, icall_counter});
     upcall->setTailCallKind(CallInst::TailCallKind::TCK_NoTail);
   } else {
     errs() << "UPDATECALL ERROR!" << "\n";
  }
}

Value* Hello::EmitCastToVoidPtr(Value *value, Instruction *InsertPoint) {
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

bool Hello::IssameInst(Instruction *Inst, GetElementPtrInst *GepInst) {
  if(!isa<GetElementPtrInst>(Inst))
      return false;

  if(Inst->getType() != GepInst->getType())
    return false;

  int Inst_op_num = dyn_cast<GetElementPtrInst>(Inst)->getNumOperands();
  int GepInst_op_num = GepInst->getNumOperands();
  if(Inst_op_num != GepInst_op_num)
    return false;
  for(int i=0;i<Inst_op_num;i++) {
    if(dyn_cast<GetElementPtrInst>(Inst)->getOperand(i)->getType() != GepInst->getOperand(i)->getType())
      return false;
    if(isa<ConstantInt>(GepInst->getOperand(i))
        && dyn_cast<ConstantInt>(dyn_cast<GetElementPtrInst>(Inst)->getOperand(i))->getZExtValue() != dyn_cast<ConstantInt>(GepInst->getOperand(i))->getZExtValue() ) {
          return false;
    }
  }
  return true;
}


char Hello::ID = 0;
static RegisterPass<Hello>
Y("hello", "Hello World Pass (with getAnalysisUsage implemented)");
