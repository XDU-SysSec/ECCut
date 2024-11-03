
#include "DDA/DDAPass.h"
#include "DDA/ContextDDA.h"
#include "DDA/DDAClient.h"
#include "DDA/FlowDDA.h"
#include "MemoryModel/PointerAnalysis.h"
#include <limits.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/CommandLine.h>
#include <sstream>
#include <llvm/IR/Constant.h>
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

char DDAPass::ID = 0;
typedef std::set<unsigned long> OriginID;
OriginID originId;

extern mapIcallInstNum mapIcallInst;
extern IcallToUpdate icalltoupdate;

IcallToGlobaltarget icalltoglobaltarget;
OriginTargetSet origintotargetset;

static cl::opt<unsigned> maxPathLen("maxpath", cl::init(100000),
                                    cl::desc("Maximum path limit for DDA"));

static cl::opt<unsigned>
        maxContextLen("maxcxt", cl::init(3),
                      cl::desc("Maximum context limit for DDA"));

static cl::opt<string> userInputQuery(
        "query", cl::init("all"),
        cl::desc("Please specify queries by inputing their pointer ids"));

static cl::opt<bool> insenRecur(
        "inrecur", cl::init(false),
        cl::desc("Mark context insensitive SVFG edges due to function recursions"));

static cl::opt<bool> insenCycle(
        "incycle", cl::init(false),
        cl::desc("Mark context insensitive SVFG edges due to value-flow cycles"));

static cl::opt<bool> printCPts("cpts", cl::init(false),
                               cl::desc("Dump conditional points-to set "));

//static cl::opt<bool>
//        printQueryPts("print-query-pts", cl::init(false),
//                      cl::desc("Dump queries' conditional points-to set "));

static cl::opt<bool> WPANUM("wpanum", cl::init(false),
                            cl::desc("collect WPA FS number only "));

static RegisterPass<DDAPass> DDAPA("dda",
                                   "Demand-driven Pointer Analysis Pass");

/// register this into alias analysis group
// static RegisterAnalysisGroup<AliasAnalysis> AA_GROUP(DDAPA);

static cl::bits<PointerAnalysis::PTATY> DDASelected(
        cl::desc("Select pointer analysis"),
        cl::values(clEnumValN(PointerAnalysis::FlowS_DDA, "dfs",
                              "Demand-driven flow sensitive analysis"),
                   clEnumValN(PointerAnalysis::Cxt_DDA, "cxt",
                              "Demand-driven context- flow- sensitive analysis")));

DDAPass::~DDAPass() {
    // _pta->dumpStat();
    if (_client != NULL)
        delete _client;
}

// [OS-CFI] getHashID(): returns an unique id for an instruction
unsigned long DDAPass::getHashID(const Instruction *inst) {
    std::hash<std::string> hash_fn;
    string str;
    raw_string_ostream rso(str);
    inst->print(rso);
    str += ("[" + inst->getParent()->getParent()->getName().str() + "]");
    return (hash_fn(str) % HASH_ID_RANGE);
}

bool DDAPass::runOnModule(SVFModule module) {
    /// initialization for llvm alias analyzer
    // InitializeAliasAnalysis(this, SymbolTableInfo::getDataLayout(&module));

    // [OS-CFI] list address-taken functions
    unsigned int nModule = module.getModuleNum();
    for (unsigned int im = 0; im < nModule; ++im) {
        Module *md = module.getModule(im);

        for (Module::iterator it = md->begin(); it != md->end(); ++it) {
            Function *fn = &(*it);
            setFunc.insert(fn);
            if (fn->hasAddressTaken()) {
                setAddrFunc.insert(fn);
            }
        }
    }

    selectClient(module);

    for (u32_t i = PointerAnalysis::FlowS_DDA; i < PointerAnalysis::Default_PTA;
         i++) {
        if (DDASelected.isSet(i))
            runPointerAnalysis(module, i);
    }

    return false;
}

/// select a client to initialize queries
void DDAPass::selectClient(SVFModule module) {

    if (!userInputQuery.empty()) {
        /// solve function pointer
        if (userInputQuery == "funptr") {
            _client = new FunptrDDAClient(module);
        }
            /// allow user specify queries
        else {
            _client = new DDAClient(module);
            if (userInputQuery != "all") {
                u32_t buf; // Have a buffer
                stringstream ss(
                        userInputQuery); // Insert the user input string into a stream
                while (ss >> buf)
                    _client->setQuery(buf);
            }
        }
    } else {
        assert(false && "Please specify query options!");
    }

    _client->initialise(module);
}

/// Create pointer analysis according to specified kind and analyze the module.
void DDAPass::runPointerAnalysis(SVFModule module, u32_t kind) {

    VFPathCond::setMaxPathLen(maxPathLen);
    ContextCond::setMaxCxtLen(maxContextLen);

    /// Initialize pointer analysis.
    switch (kind) {
        case PointerAnalysis::Cxt_DDA: {
            _pta = new ContextDDA(module, _client);
            break;
        }
        case PointerAnalysis::FlowS_DDA: {
            _pta = new FlowDDA(module, _client);
            break;
        }
        default:
            outs() << "This pointer analysis has not been implemented yet.\n";
            break;
    }

    if (WPANUM) {
        _client->collectWPANum(module);
    } else {
        _pta->disablePrintStat();
        /// initialize
        _pta->initialize(module);
        /// compute points-to
        answerQueries(_pta);
//        /// finalize
        _pta->finalize();
//        if (printCPts)
//            _pta->dumpCPts();
//
        if (_pta->printStat())
            _client->performStat(_pta);

//        if (printQueryPts)
//            printQueryPTS();

        // [OS-CFI] SUPA completes process and it is time to compute our CFGs
        computeCFG(module);

        // [OS-CFI] Process the labeling
        createLabelForValue(module);

        // [OS-CFI] print out our CFGs
//        dumpSUPACFG();

        dumpoCFG();
        dumpatCFG();
//        dumpatCFG();
//        dumpGlobal();

    }
}

/*!
 * Initialize queries
 */
void DDAPass::answerQueries(PointerAnalysis *pta) {

    DDAStat *stat = static_cast<DDAStat *>(pta->getStat());
    u32_t vmrss = 0;
    u32_t vmsize = 0;
    analysisUtil::getMemoryUsageKB(&vmrss, &vmsize);
    stat->setMemUsageBefore(vmrss, vmsize);
    _client->answerQueries(pta);

    vmrss = vmsize = 0;
    analysisUtil::getMemoryUsageKB(&vmrss, &vmsize);
    stat->setMemUsageAfter(vmrss, vmsize);
}

/*!
 * Initialize context insensitive Edge for DDA
 */
void DDAPass::initCxtInsensitiveEdges(PointerAnalysis *pta, const SVFG *svfg,
                                      const SVFGSCC *svfgSCC,
                                      SVFGEdgeSet &insensitveEdges) {
    if (insenRecur)
        collectCxtInsenEdgeForRecur(pta, svfg, insensitveEdges);
    else if (insenCycle)
        collectCxtInsenEdgeForVFCycle(pta, svfg, svfgSCC, insensitveEdges);
}

/*!
 * Whether SVFG edge in a SCC cycle
 */
bool DDAPass::edgeInSVFGSCC(const SVFGSCC *svfgSCC, const SVFGEdge *edge) {
    return (svfgSCC->repNode(edge->getSrcID()) ==
            svfgSCC->repNode(edge->getDstID()));
}

/*!
 *  Whether call graph edge in SVFG SCC
 */
bool DDAPass::edgeInCallGraphSCC(PointerAnalysis *pta, const SVFGEdge *edge) {
    const BasicBlock *srcBB = edge->getSrcNode()->getBB();
    const BasicBlock *dstBB = edge->getDstNode()->getBB();

    if (srcBB && dstBB)
        return pta->inSameCallGraphSCC(srcBB->getParent(), dstBB->getParent());

    assert(edge->isRetVFGEdge() == false &&
           "should not be an inter-procedural return edge");

    return false;
}

/*!
 * Mark insensitive edge for function recursions
 */
void DDAPass::collectCxtInsenEdgeForRecur(PointerAnalysis *pta,
                                          const SVFG *svfg,
                                          SVFGEdgeSet &insensitveEdges) {

    for (SVFG::SVFGNodeIDToNodeMapTy::const_iterator it = svfg->begin(),
                 eit = svfg->end();
         it != eit; ++it) {

        SVFGEdge::SVFGEdgeSetTy::const_iterator edgeIt = it->second->InEdgeBegin();
        SVFGEdge::SVFGEdgeSetTy::const_iterator edgeEit = it->second->InEdgeEnd();
        for (; edgeIt != edgeEit; ++edgeIt) {
            const SVFGEdge *edge = *edgeIt;
            if (edge->isCallVFGEdge() || edge->isRetVFGEdge()) {
                if (edgeInCallGraphSCC(pta, edge))
                    insensitveEdges.insert(edge);
            }
        }
    }
}

/*!
 * Mark insensitive edge for value-flow cycles
 */
void DDAPass::collectCxtInsenEdgeForVFCycle(PointerAnalysis *pta,
                                            const SVFG *svfg,
                                            const SVFGSCC *svfgSCC,
                                            SVFGEdgeSet &insensitveEdges) {

    std::set<NodePair> insensitvefunPairs;

    for (SVFG::SVFGNodeIDToNodeMapTy::const_iterator it = svfg->begin(),
                 eit = svfg->end();
         it != eit; ++it) {

        SVFGEdge::SVFGEdgeSetTy::const_iterator edgeIt = it->second->InEdgeBegin();
        SVFGEdge::SVFGEdgeSetTy::const_iterator edgeEit = it->second->InEdgeEnd();
        for (; edgeIt != edgeEit; ++edgeIt) {
            const SVFGEdge *edge = *edgeIt;
            if (edge->isCallVFGEdge() || edge->isRetVFGEdge()) {
                if (this->edgeInSVFGSCC(svfgSCC, edge)) {

                    const BasicBlock *srcBB = edge->getSrcNode()->getBB();
                    const BasicBlock *dstBB = edge->getDstNode()->getBB();

                    if (srcBB && dstBB) {
                        NodeID src = pta->getPTACallGraph()
                                ->getCallGraphNode(srcBB->getParent())
                                ->getId();
                        NodeID dst = pta->getPTACallGraph()
                                ->getCallGraphNode(dstBB->getParent())
                                ->getId();
                        insensitvefunPairs.insert(std::make_pair(src, dst));
                        insensitvefunPairs.insert(std::make_pair(dst, src));
                    } else
                        assert(edge->isRetVFGEdge() == false &&
                               "should not be an inter-procedural return edge");
                }
            }
        }
    }

    for (SVFG::SVFGNodeIDToNodeMapTy::const_iterator it = svfg->begin(),
                 eit = svfg->end();
         it != eit; ++it) {
        SVFGEdge::SVFGEdgeSetTy::const_iterator edgeIt = it->second->InEdgeBegin();
        SVFGEdge::SVFGEdgeSetTy::const_iterator edgeEit = it->second->InEdgeEnd();
        for (; edgeIt != edgeEit; ++edgeIt) {
            const SVFGEdge *edge = *edgeIt;

            if (edge->isCallVFGEdge() || edge->isRetVFGEdge()) {
                const BasicBlock *srcBB = edge->getSrcNode()->getBB();
                const BasicBlock *dstBB = edge->getDstNode()->getBB();

                if (srcBB && dstBB) {
                    NodeID src = pta->getPTACallGraph()
                            ->getCallGraphNode(srcBB->getParent())
                            ->getId();
                    NodeID dst = pta->getPTACallGraph()
                            ->getCallGraphNode(dstBB->getParent())
                            ->getId();
                    if (insensitvefunPairs.find(std::make_pair(src, dst)) !=
                        insensitvefunPairs.end())
                        insensitveEdges.insert(edge);
                    else if (insensitvefunPairs.find(std::make_pair(dst, src)) !=
                             insensitvefunPairs.end())
                        insensitveEdges.insert(edge);
                }
            }
        }
    }
}

/*!
 * Return alias results based on our points-to/alias analysis
 * TODO: Need to handle PartialAlias and MustAlias here.
 */
llvm::AliasResult DDAPass::alias(const Value *V1, const Value *V2) {
    PAG *pag = _pta->getPAG();

    /// TODO: When this method is invoked during compiler optimizations, the IR
    ///       used for pointer analysis may been changed, so some Values may not
    ///       find corresponding PAG node. In this case, we only check alias
    ///       between two Values if they both have PAG nodes. Otherwise, MayAlias
    ///       will be returned.
    if (pag->hasValueNode(V1) && pag->hasValueNode(V2)) {
        PAGNode *node1 = pag->getPAGNode(pag->getValueNode(V1));
        if (pag->isValidTopLevelPtr(node1))
            _pta->computeDDAPts(node1->getId());

        PAGNode *node2 = pag->getPAGNode(pag->getValueNode(V2));
        if (pag->isValidTopLevelPtr(node2))
            _pta->computeDDAPts(node2->getId());

        return _pta->alias(V1, V2);
    }

    return MayAlias;
}

/*!
 * Print queries' pts
 */
void DDAPass::printQueryPTS() {
    outs() << "+++++++++++++++++++++++++++[SVF] printQueryPTS [SVF]+++++++++++++++++++++++++++" << "\n";
    const NodeSet &candidates = _client->getCandidateQueries();
    for (NodeSet::iterator it = candidates.begin(), eit = candidates.end();
         it != eit; ++it) {
        const PointsTo &pts = _pta->getPts(*it);
        _pta->dumpPts(*it, pts);
    }
    outs() << "\n";
}

void DDAPass::createLabelForValue(SVFModule SM) {
    llvm::Module *M = SM.getModule(0);
    PointerType *int32PtTy = Type::getInt32PtrTy(M->getContext());
    IntegerType *int32Ty = Type::getInt32Ty(M->getContext());

    // list the BlockAddress from BasicBlock
    std::vector<Constant *> listBA;

    for (ValToIDMapIt fit = mapValID.begin(); fit != mapValID.end(); ++fit) {
        Value *val = (Value *)fit->first;
        if (isa<Constant>(val)) {
            Constant *C = dyn_cast<Constant>(val);
            unsigned long id = fit->second;

            Constant *CConst =
                    ConstantExpr::getCast(Instruction::BitCast, C, int32PtTy);

            Constant *tag_id = ConstantInt::get(int32Ty, id, false);
            Constant *tag = ConstantFolder().CreateIntToPtr(tag_id, int32PtTy);

            listBA.push_back(tag);
            listBA.push_back(CConst);
        }
    }

    ArrayRef<Constant *> blockArray(listBA);
    // create the constant type and array
    ArrayType *pArrTy = ArrayType::get(int32PtTy, listBA.size());
    Constant *blockItems = ConstantArray::get(pArrTy, blockArray);

    GlobalVariable *gvar_target_data =
            new GlobalVariable(*M, blockItems->getType(), true,
                               GlobalValue::ExternalLinkage, blockItems, "GL_TABLE");
    gvar_target_data->setSection("cfg_label_tracker");
}

// [OS-CFI] isTypeMatch(): return true if params/args and return types matched
// between an indirect call and a function signature
bool DDAPass::isTypeMatch(const Instruction *sink, const Value *source) {
    int nFnArg = 0, nCallArg = 0;
    vector<const Type *> fnArgList, callArgList;
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
        return false;
    }
    return false;
}

// [OS-CFI] use address-taken type check entry for SUPA points-to set empty sinks
bool DDAPass::fillEmptyPointsToSet(const Instruction *iCallInst) {
    bool IsTypeBase = false;
    string str;
    for (FuncSetIt it = setAddrFunc.begin(); it != setAddrFunc.end(); ++it) {
        Function *val = *it;
        if (isTypeMatch(iCallInst, val)) {
            IsTypeBase = true;
            atCFG *atItem = (atCFG *)malloc(sizeof(atCFG));
            atItem->type = UNDER_APPROXIMATE;
            atItem->iCallInst = iCallInst;
			//ECCut
            const llvm::Instruction *rcall = iCallInst->getPrevNode();
            unsigned long iCallID = 0;
            if (isa<CallInst>(rcall)) {
                const CallInst *call = dyn_cast<CallInst>(rcall);
                if (call->getCalledFunction() &&
                    (call->getCalledFunction()->getName() == "pcall_reference_monitor" ||
                     call->getCalledFunction()->getName() == "vcall_reference_monitor") &&
                    isa<ConstantInt>(call->getOperand(0))) {
                    ConstantInt *cint = dyn_cast<ConstantInt>(rcall->getOperand(0));
                    iCallID = cint->getZExtValue();
                }
            }

            atItem->iCallID = iCallID;
//            atItem->iCallID = getHashID(iCallInst);
            atItem->iCallTarget = val;
            atItem->iCallTargetID = val->getValueID();\
            if(IsNotRepeat(atItem)) {
                mapValID[val] = val->getValueID();
                atCFGList.push_back(atItem);
            }
        }
    }
    return IsTypeBase;
}

void DDAPass::computeCFG(SVFModule M) {
    const NodeSet &candidates = _client->getCandidateQueries();
    outs() << "[CFI] Number of collected candidate queries is " << candidates.size() << "\n";
    outs() << "[CFI] Number of collected icallinst num is " << mapIcallInst.size() << "\n";
    int icall_num = 0;
    for(; icall_num < mapIcallInst.size(); ) {

        llvm::Instruction *iCallInst = mapIcallInst[icall_num];
         outs()<< "*****************************\n";
         outs() << "icall_num: " <<icall_num << " " << *iCallInst << "\n";

        if (iCallInst == nullptr) {
            outs()<<"icallInst is nullptr\n";
            icall_num++;
            continue;
        }

        bool IsVcall = false;
        unsigned long iCallID = 0;
        llvm::Instruction *firstInst = iCallInst->getParent()->getFirstNonPHI();
        llvm::Instruction *rCallInst = iCallInst->getPrevNode();
        int n = 5;
        while(n--){
            if(isa<CallInst>(rCallInst) || rCallInst == firstInst)
                break;
            rCallInst = rCallInst->getPrevNode();
        }
//        if(!isa<CallInst>(rCallInst))
//            rCallInst = rCallInst->getPrevNode();
//        outs()<<"rcall: "<<*rCallInst<<"\n";
        Function *P_REF = M.getFunction("pcall_reference_monitor");
        Function *V_REF = M.getFunction("vcall_reference_monitor");
        if (isa<CallInst>(rCallInst)) {
            CallInst *call = dyn_cast<CallInst>(rCallInst);
            if (call->getCalledFunction() && call->getCalledFunction() == V_REF &&
                isa<ConstantInt>(call->getOperand(0))) {
                IsVcall = true;
                ConstantInt *cint = dyn_cast<ConstantInt>(rCallInst->getOperand(0));
                iCallID = cint->getZExtValue();
            } else if(call->getCalledFunction() && call->getCalledFunction() == P_REF
                        && isa<ConstantInt>(call->getOperand(0))) {
                ConstantInt *cint = dyn_cast<ConstantInt>(rCallInst->getOperand(0));
                iCallID = cint->getZExtValue();
            }
        } else {
            outs() << "rcall is error: " << *rCallInst << "\n";
            if ( !fillEmptyPointsToSet(iCallInst) )
                outs()<<"icallInst has no typebase\n";
            icall_num++;
            continue;
        }

        UpdateCallSet updatecall;
        for(IcallToUpdateIt it = icalltoupdate.begin(); it != icalltoupdate.end(); it++) {
            if (it->first == iCallID) {
                updatecall = it->second;
            }
        }

        if(updatecall.empty()) {
            outs() << icall_num << " icall has no origin\n";
            if ( !fillEmptyPointsToSet(iCallInst) )
                outs()<<"icallInst has no typebase\n";
            icall_num++;
            continue;
        }

        llvm::Value *Icall_ptr_value = NULL;
        if( isa<CallInst>(iCallInst) )
            Icall_ptr_value = dyn_cast<CallInst>(iCallInst)->getCalledValue();
        else if ( isa<InvokeInst>(iCallInst) )
            Icall_ptr_value = dyn_cast<InvokeInst>(iCallInst)->getCalledValue();
        NodeID Icall_pNodeId = _pta->getPAG()->getValueNode(Icall_ptr_value);

        if(!Icall_pNodeId) {
            outs() << icall_num << " icall has no icall id\n";
            if ( !fillEmptyPointsToSet(iCallInst) ) {
                icall_num++;
                outs() << "icallInst has no typebase\n";
                continue;
            }
        }
        if(cast<CallInst>(rCallInst)->getCalledFunction() == P_REF)
            _pta->computeDDAPts(Icall_pNodeId, 2);

        //dump global
        Icall icall;
        Global global;
        icall.insert(make_pair(icall_num,iCallID));

        if(!IcallToTargetset.empty() && !IsVcall) {

            //icall_target is in IcallToTargetset, origin target is in valtotargetset
            for (ValToTargetSetIt pit = IcallToTargetset.begin(); pit != IcallToTargetset.end(); pit++) {
                if (_pta->getValueFromNodeID(*pit) &&
                    isa<Function>(_pta->getValueFromNodeID(*pit))) {
                    unsigned long target = *pit; // origin sensitive tuple target
                    string icall_target_name = _pta->getValueFromNodeID(*pit)->getName();

                    for (UpdateCallSetIt osit = updatecall.begin(); osit != updatecall.end(); osit++) {
                        auto oInst = dyn_cast<Instruction>(*osit);


                        auto OriginID = dyn_cast<ConstantInt>(oInst->getOperand(2));
                        unsigned long originID = OriginID->getZExtValue();

                        auto ctx = oInst->getOperand(3);

                        bool cfgok = false;
                        auto ptr_value_64 = oInst->getOperand(1);
                        auto ptr_value = dyn_cast<PtrToIntOperator>(ptr_value_64)->getOperand(0);

                        NodeID pNodeId = _pta->getPAG()->getValueNode(ptr_value);

                        if (isTypeMatch(iCallInst, _pta->getValueFromNodeID(*pit))) {
                            if (ptr_value->getType()->getPointerElementType()->isFunctionTy() &&
                                    ptr_value->hasName()) {
                                string ptr_value_name = ptr_value->getName();

                                if (ptr_value_name == icall_target_name) {
                                    cfgok = true;
                                }
                            } else if (isa<llvm::PointerType>(ptr_value->getType()) && pNodeId != 0) {
                                _pta->computeDDAPts(pNodeId,1);
                            }
                        }
                        if (valtotargetset.empty() && !cfgok)
                            continue;



                        bool TargetMatchOrigin = false;
                        for (auto vtIt = valtotargetset.begin(); vtIt != valtotargetset.end(); vtIt++) {
                            if(_pta->getValueFromNodeID(*vtIt) && isa<Function>(_pta->getValueFromNodeID(*vtIt))) {
                                string origin_target_name = _pta->getValueFromNodeID(*vtIt)->getName();
                                if (origin_target_name == icall_target_name) {
                                    TargetMatchOrigin = true;
                                }
                            }
                        }

                        if (valtotargetset.count(*pit) || TargetMatchOrigin || cfgok) {
                            oCFG *oItem = (oCFG *) malloc(sizeof(oCFG));
                            const Value *func = _pta->getValueFromNodeID(*pit);
                            if (func && func->hasName()) {
                                if (!isa<ConstantInt>(ctx)) {
                                    Process_GepInst process_gep;

                                    Value *global_inst = dyn_cast<Instruction>(ptr_value)->getOperand(0);
                                    Value *gl_struct_val;

                                    while (1) {
                                        if (dyn_cast<Instruction>(global_inst)->isTerminator())
                                            break;

                                        gl_struct_val = dyn_cast<Instruction>(global_inst)->getOperand(0);
                                        if (isa<BitCastOperator>(gl_struct_val))
                                            gl_struct_val = dyn_cast<BitCastOperator>(gl_struct_val)->getOperand(0);
                                        if (isa<llvm::GlobalVariable>(gl_struct_val)) {
                                            break;
                                        } else {
                                            process_gep.insert(global_inst);
                                            global_inst = gl_struct_val;
                                        }
                                    }
                                    std::vector<unsigned long> ctx_array;

                                    if (isa<GlobalVariable>(gl_struct_val) )
                                        FindValInGlobalStruct(func, gl_struct_val, ctx_array, &process_gep);

                                    for (std::vector<unsigned long>::iterator it = ctx_array.begin();
                                         it != ctx_array.end(); it++) {
                                        oItem->iCallInst = iCallInst;
                                        oItem->iCallID = iCallID;
                                        oItem->iCallTarget = func;
                                        oItem->iCallTargetID = *pit;
                                        oItem->originCTXID = *it;
                                        oItem->originCTX = ctx;
                                        oItem->originID = originID;

                                        if (IsNotRepeat(oItem)) {
                                            mapValID[func] = *pit;
                                            oCFGList.push_back(oItem);
                                        }
                                    }
                                }
                                else {
                                    oItem->iCallInst = iCallInst;
                                    oItem->iCallID = iCallID;
                                    oItem->iCallTarget = func;
                                    oItem->iCallTargetID = *pit;
                                    oItem->originCTXID = 0;
                                    oItem->originCTX = nullptr;
                                    oItem->originID = originID;

                                    if (IsNotRepeat(oItem)) {
                                        mapValID[func] = *pit;
                                        oCFGList.push_back(oItem);
                                    }
                                }
                            }
                        }

                        valtotargetset.clear();
                    }
                }

                else if (_pta->getValueFromNodeID(*pit) &&
                           isa<GlobalVariable>(_pta->getValueFromNodeID(*pit))) {

                    auto Global_target = cast<GlobalVariable>(_pta->getValueFromNodeID(*pit));

                    for (UpdateCallSetIt osit = updatecall.begin(); osit != updatecall.end(); osit++) {
                        auto oInst = dyn_cast<Instruction>(*osit);

                        auto OriginID = dyn_cast<ConstantInt>(oInst->getOperand(2));
                        unsigned long originID = OriginID->getZExtValue();



                        bool cfgok = false;
                        auto ptr_value_64 = oInst->getOperand(1);
                        auto ptr_value = dyn_cast<PtrToIntOperator>(ptr_value_64)->getOperand(0);

                        NodeID pNodeId = _pta->getPAG()->getValueNode(ptr_value);

                        oCFG *oItem = (oCFG *) malloc(sizeof(oCFG));
                        const Value *func = ptr_value;
                        if (ptr_value->getType()->getPointerElementType()->isFunctionTy() && ptr_value->hasName()) {
                            oItem->iCallInst = iCallInst;
                            oItem->iCallID = iCallID;
                            oItem->iCallTarget = func;
                            oItem->iCallTargetID = pNodeId;
                            oItem->originCTXID = 0;
                            oItem->originCTX = nullptr;
                            oItem->originID = originID;

                            if (IsNotRepeat(oItem)) {
                                mapValID[func] = pNodeId;
                                oCFGList.push_back(oItem);
                            }
                        }
                        else if (isa<llvm::PointerType>(ptr_value->getType()) && pNodeId != 0) {
                            _pta->computeDDAPts(pNodeId,1);

                            for(auto origin_it = valtotargetset.begin(); origin_it != valtotargetset.end(); origin_it++) {
                                if(_pta->getValueFromNodeID(*origin_it) && isa<Function>(_pta->getValueFromNodeID(*origin_it))
                                   && isTypeMatch(iCallInst, _pta->getValueFromNodeID(*origin_it))) {
                                    auto func = cast<Function>(_pta->getValueFromNodeID(*origin_it));
                                    string icall_target_name = func->getName();
                                    oCFG *oItem = (oCFG *) malloc(sizeof(oCFG));
                                    oItem->iCallInst = iCallInst;
                                    oItem->iCallID = iCallID;
                                    oItem->iCallTarget = func;
                                    oItem->iCallTargetID = func->getValueID();
                                    oItem->originCTXID = 0;
                                    oItem->originCTX = nullptr;
                                    oItem->originID = originID;

                                    if (IsNotRepeat(oItem)) {
                                        mapValID[func] = func->getValueID();
                                        oCFGList.push_back(oItem);
                                    }
                                }
                            }
                            valtotargetset.clear();
                        }
                    }
                }
            }




            // list CI-CFG using SUPA
            for (ValToTargetSetIt pit = IcallToTargetset.begin(); pit != IcallToTargetset.end(); pit++) {

                if (_pta->getValueFromNodeID(*pit) &&
                    (isa<GlobalValue>(_pta->getValueFromNodeID(*pit)) ||
                     isa<Function>(_pta->getValueFromNodeID(*pit)))) {
                    // if the points-to set is overapproximated, then type mismatch can
                    // detect it
                    if (isTypeMatch(iCallInst, _pta->getValueFromNodeID(*pit))
                        && !IsIcallInOCFG(iCallID)
                        && isa<Function>(_pta->getValueFromNodeID(*pit))) {
                        atCFG *atItem = (atCFG *) malloc(sizeof(atCFG));
                        atItem->type = OVER_APPROXIMATE;
                        atItem->iCallInst = iCallInst;
                        atItem->iCallID = iCallID;

                        atItem->iCallTarget = _pta->getValueFromNodeID(*pit);
                        atItem->iCallTargetID = *pit;
                        if(IsNotRepeat(atItem)) {
                            mapValID[atItem->iCallTarget] = *pit;
                            atCFGList.push_back(atItem);
                        }
                    }
                }
            }
        }

        else {

            outs() << icall_num << " icall has no target\n";

            for (UpdateCallSetIt osit = updatecall.begin(); osit != updatecall.end(); osit++) {
                auto oInst = dyn_cast<Instruction>(*osit);

                auto OriginID = dyn_cast<ConstantInt>(oInst->getOperand(2));
                unsigned long originID = OriginID->getZExtValue();

                auto ptr_value_64 = oInst->getOperand(1);
                auto ptr_value = dyn_cast<PtrToIntOperator>(ptr_value_64)->getOperand(0);


                NodeID pNodeId = _pta->getPAG()->getValueNode(ptr_value);

                if (ptr_value->getType()->getPointerElementType()->isFunctionTy() && ptr_value->hasName()) {
                    string icall_target_name = ptr_value->getName();
                    oCFG *oItem = (oCFG *) malloc(sizeof(oCFG));
                    const Value *func = ptr_value;
                    oItem->iCallInst = iCallInst;
                    oItem->iCallID = iCallID;
                    oItem->iCallTarget = func;
                    oItem->iCallTargetID = pNodeId;
                    oItem->originCTXID = 0;
                    oItem->originCTX = nullptr;
                    oItem->originID = originID;

                    if (IsNotRepeat(oItem)) {
                        mapValID[func] = pNodeId;
                        oCFGList.push_back(oItem);
                    }
                }
                else if (isa<llvm::PointerType>(ptr_value->getType()) && pNodeId != 0) {
                    _pta->computeDDAPts(pNodeId,1);

                    for(auto origin_it = valtotargetset.begin(); origin_it != valtotargetset.end(); origin_it++) {
                        if(_pta->getValueFromNodeID(*origin_it) && isa<Function>(_pta->getValueFromNodeID(*origin_it))
                                && isTypeMatch(iCallInst, _pta->getValueFromNodeID(*origin_it))) {
                            auto func = cast<Function>(_pta->getValueFromNodeID(*origin_it));
                            string icall_target_name = func->getName();
                            oCFG *oItem = (oCFG *) malloc(sizeof(oCFG));
                            oItem->iCallInst = iCallInst;
                            oItem->iCallID = iCallID;
                            oItem->iCallTarget = func;
                            oItem->iCallTargetID = func->getValueID();
                            oItem->originCTXID = 0;
                            oItem->originCTX = nullptr;
                            oItem->originID = originID;

                            if (IsNotRepeat(oItem)) {
                                mapValID[func] = func->getValueID();
                                oCFGList.push_back(oItem);
                            }
                        }
                    }
                    valtotargetset.clear();
                }
            }

        }

        if( (!IsIcallInOCFG(iCallID) && !IsIcallInatCFG(iCallID)) ){
            outs()<<"icallInst has no os-cfg\n";
            if ( !fillEmptyPointsToSet(iCallInst) );
                outs()<<"icallInst has no typebase\n";
        }

        IcallToTargetset.clear();
        outs()<<"*************************\n";
        icall_num++;
    }

}


bool DDAPass::findFuncInVtable(const llvm::GlobalVariable* vtable, std::string Func_name) {
    outs()<<"find vtable\n";
    if(vtable->hasExternalLinkage() ) {
        return false;
    }

    const Constant* global_init = vtable->getInitializer();
    if (isa<ConstantAggregateZero>(global_init)) {

        return false;
    }


    const ConstantStruct* CS = dyn_cast<ConstantStruct>(global_init);
    for(int i =0 ;i<CS->getNumOperands();i++) {
        ConstantArray* Vtable_Array = dyn_cast<ConstantArray>(CS->getOperand(i));

        for (unsigned long index = 0; index != Vtable_Array->getNumOperands(); index++) {

            Constant *internal_global_var = Vtable_Array->getAggregateElement(index);

            if (internal_global_var->hasName() && internal_global_var->getName() == Func_name) {
                return true;
            } else if (isa<BitCastOperator>(internal_global_var)) {
                if (dyn_cast<BitCastOperator>(internal_global_var)->getOperand(0)->hasName()
                    && dyn_cast<BitCastOperator>(internal_global_var)->getOperand(0)->getName() == Func_name) {

                    return true;
                }
            }
        }
    }
    return false;
}

void DDAPass::FindValInGlobalStruct(const Value *Func, Value *global_var, std::vector<unsigned long> &vec, Process_GepInst *Process_Gep) {
    if (!vec.empty())
        vec.clear();
    auto Val_name = Func->getName();
    auto global_init = dyn_cast<GlobalVariable>(global_var)->getInitializer();
    if (dyn_cast<ConstantAggregateZero>(global_init)) {
        vec.push_back(0);
        return;
    }
    auto array_var = dyn_cast<ConstantArray>(global_init);
    assert(array_var != nullptr);

    Type *global_type = global_var->getType();
    while (global_type->isPointerTy())
        global_type = global_type->getPointerElementType();

    auto array_type = dyn_cast<ArrayType>(global_type);
    assert(array_type != nullptr);

    unsigned long Prev_Load_Offset = 0;
    bool Is_Prev_Load = false;

    for (unsigned long index = 0; index != array_type->getArrayNumElements(); index++) {
        Constant *internal_global_var = array_var->getAggregateElement(index);

        for (Process_GepInstIt it = Process_Gep->begin(); it != Process_Gep->end(); it++) {
            if (isa<llvm::GEPOperator>(*it)
                && !isa<GlobalVariable>(dyn_cast<GEPOperator>(*it)->getOperand(0))) {
                int offset_size = dyn_cast<GEPOperator>(*it)->getNumOperands();

                auto *gep_offset = dyn_cast<GEPOperator>(*it)->getOperand(offset_size - 1);

                if( !isa<ConstantInt>(gep_offset) ) {
                    outs()<<**it<<"\n";
                    outs()<<"not constantint\n";
                    vec.push_back(0);
                    return;
                }

                if (Is_Prev_Load) {
                    unsigned long add_gep_offset = Prev_Load_Offset + get_struct_mem_offset(*it);
                    unsigned long struct_size = sizeof(*dyn_cast<GEPOperator>(*it)->getOperand(0)->getType()->getPointerElementType());

                    if(globalstructsize.find(global_var) != globalstructsize.end() ) {
                        struct_size = globalstructsize[global_var];
                    } else {
                        if (dyn_cast<ConstantInt>(gep_offset)->getZExtValue())
                            struct_size =
                                    get_struct_mem_offset(*it) / dyn_cast<ConstantInt>(gep_offset)->getZExtValue();
                        globalstructsize.insert(make_pair(global_var, struct_size));
                    }
                    unsigned long Load_add_Gep_offset = add_gep_offset/struct_size;

                    internal_global_var = internal_global_var->getAggregateElement(Load_add_Gep_offset );
                    Is_Prev_Load = false;
                } else {
                    internal_global_var = internal_global_var->getAggregateElement(cast<ConstantInt>(gep_offset));
                }
            }
            if (isa<llvm::LoadInst>(*it)) {
                Is_Prev_Load = true;
                Value *Gep_GL_Var;

                if (isa<Operator>(internal_global_var)) {
                    Value *Gep_Offset = internal_global_var->getOperand(internal_global_var->getNumOperands() - 1);
                    while (!isa<ConstantInt>(Gep_Offset)) {
                        int gep_opnum = dyn_cast<Operator>(Gep_Offset)->getNumOperands();
                        Gep_Offset = dyn_cast<Operator>(Gep_Offset)->getOperand(gep_opnum - 1);
                    }
                    Prev_Load_Offset = dyn_cast<ConstantInt>(Gep_Offset)->getZExtValue();

                    Gep_GL_Var = internal_global_var->getOperand(0);
                    while (!isa<GlobalVariable>(Gep_GL_Var)) {
                        Gep_GL_Var = dyn_cast<Operator>(Gep_GL_Var)->getOperand(0);
                    }

                    auto Gep_Gl_Init = dyn_cast<GlobalVariable>(Gep_GL_Var)->getInitializer();
                    if (dyn_cast<ConstantAggregateZero>(Gep_Gl_Init)) {
                        return;
                    }
                    internal_global_var = dyn_cast<Constant>(Gep_Gl_Init);
                    assert(internal_global_var != nullptr);
                }
            }
        }

        if( isa<BitCastOperator>(internal_global_var) ) {
            auto *func_op = dyn_cast<BitCastOperator>(internal_global_var)->getOperand(0);
            if(func_op->hasName() && func_op->getName() == Val_name) {
                vec.push_back(index+1);
            }
        }
        else if (internal_global_var->hasName() && internal_global_var->getName() == Val_name)
            vec.push_back(index+1);
    }

    if (vec.empty())
        vec.push_back(0);
}

bool DDAPass::IsIcallInOCFG(unsigned long IcallID) {
    for (std::vector<oCFG *>::iterator cit = oCFGList.begin();
         cit != oCFGList.end(); cit++) {
        oCFG *item = *cit;
        if (item->iCallID == IcallID) {
            return true;
        }
    }
    return false;
}

bool DDAPass::IsIcallInatCFG(unsigned long IcallID) {
    for (std::vector<atCFG *>::iterator cit = atCFGList.begin();
         cit != atCFGList.end(); cit++) {
        atCFG *item = *cit;
        if (item->iCallID == IcallID) {
            return true;
        }
    }
    return false;
}


int64_t DDAPass::get_struct_mem_offset(Value *getelementptr_inst) {
    int64_t former_offset = 0,after_offset = 0;
    APInt ap_offset(64,0,true);

    auto *dl = new DataLayout(dyn_cast<GetElementPtrInst>(getelementptr_inst)->getModule());

    former_offset = ap_offset.getSExtValue();


    auto cal = dyn_cast<GEPOperator>(getelementptr_inst)->accumulateConstantOffset(*dl, ap_offset);
    if(cal) {
        after_offset = ap_offset.getSExtValue();
        return after_offset - former_offset;
    } else {
        outs() << "Failed to get offset!\n";
        return -1;
    }
}

bool DDAPass::IsNotRepeat(oCFG *oItem) {
    for (std::vector<oCFG *>::iterator oit = oCFGList.begin();
         oit != oCFGList.end(); ++oit) {
        oCFG *item = *oit;
        if (item->iCallID == oItem->iCallID &&
            item->originID == oItem->originID &&
            item->iCallTarget->getName() == oItem->iCallTarget->getName() &&
            item->originCTXID == oItem->originCTXID)
            return false;
    }
    return true;
}

bool DDAPass::IsNotRepeat(atCFG *atItem) {
    for (auto cit = atCFGList.begin();
         cit != atCFGList.end(); ++cit) {
        atCFG *item = *cit;
        if (item->iCallID == atItem->iCallID &&
            item->iCallTarget->getName() == atItem->iCallTarget->getName() )
            return false;
    }
    return true;
}

// [OS-CFI] dumpSUPACFG(): print CI-CFG based on SUPA only analysis
void DDAPass::dumpSUPACFG() {
    if(supaCFGList.empty()) {
        return;
    }
    std::hash<std::string> hash_fn;

    unsigned long icall = supaCFGList[0]->iCallID;
    unsigned long  cfg_num = 0;
    int conut = 1 ;
    double EC_Sum = 0;

//    if (DUMP_CFG_DEBUG)
        outs() << "++++++++++++++[SUPA] Dump CI-CFG [SUPA]++++++++++++++\n";
    outs() << "icall:" << *supaCFGList[0]->iCallInst << "\n";
    outs() << "icall ID: " << supaCFGList[0]->iCallID << "\n";
    for (std::vector<supaCFG *>::iterator cit = supaCFGList.begin();
         cit != supaCFGList.end(); ++cit) {
        supaCFG *item = *cit;

        if (item->iCallID == icall) {
            cfg_num++;
        } else {
            cfg_num = 1;
            icall = item->iCallID;
        }

        if (DUMP_CFG_DEBUG) {
            outs() << "iCall Target: " << (item->iCallTarget)->getName() << "\n";
        }

        int T;
        if (isa<Function>(item->iCallTarget)) {
            T = FUNC;
        } else {
            T = VT;
        }

    }
}

// [OS-CFI] dumpatCFG(): print CI-CFG based on Address Taken and Type Check CFG
void DDAPass::dumpatCFG() {
    if(atCFGList.empty()) {
        return;
    }
    std::hash<std::string> hash_fn;

    if (DUMP_CFG_DEBUG)
        outs() << "++++++++++++++[ECCut] Address Taken and Type Checked CFG [ECCut]++++++++++++++\n";
    for (std::vector<atCFG *>::iterator cit = atCFGList.begin();
         cit != atCFGList.end(); ++cit) {
        atCFG *item = *cit;

        if (DUMP_CFG_DEBUG) {
            outs() << "iCall Target: " << (item->iCallTarget)->getName() << "\n";
        }
        errs() << ATCFG << "\t" << item->iCallID << "\t"
               << item->iCallTargetID << "\n";
    }
}

// [OS-CFI] dumpoCFG(): print origin sensitive CFG
void DDAPass::dumpoCFG() {
    if(oCFGList.empty()) {
        return;
    }

    std::hash<std::string> hash_fn;
    for (std::vector<oCFG *>::iterator cit = oCFGList.begin();
         cit != oCFGList.end(); cit++) {
        oCFG *item = *cit;

        if (DUMP_CFG_DEBUG) {
            outs() << "iCall Instruction: " << *(item->iCallInst) << "\n";
            outs() << "iCall Target: " << item->iCallTarget->getName() << "\n";
            outs() << "iCall ID: " << item->iCallID << "\n";
            if (item->originCTX) {
                outs() << "Origin CS Instruction: " << *(item->originCTX) << "\n";
                outs() << "Origin CTX ID : " << item->originCTXID << "\n";
            }

            outs() << "iCall Target ID: " << item->iCallTargetID << "\n";
            outs() << "iCall Origin ID: " << item->originID << "\n";
            int flag = 1;
            for (OriginID::iterator oidit = originId.begin(); oidit != originId.end(); oidit++) {
                if (*oidit == item->originID) {
                    flag = 0;
                    break;
                }
            }
            if (flag) {
                originId.insert(item->originID);
            }
            outs() << "\n";
        }

        errs() << OCFG << "\t" << item->iCallID << "\t"
               << item->iCallTargetID << "\t" << item->originID;
        if (item->originCTX) {
            errs() << "\t" << item->originCTXID;
        }
        errs() << "\n";
    }

    }
}

