/*
 * Origin-sensitive Control Flow Integrity
 * Author: Mustakimur R. Khandaker (mrk15e@my.fsu.edu)
 * Affliation: Florida State University
 */
#include "DDA/ContextDDA.h"
#include "DDA/DDAClient.h"
#include "DDA/FlowDDA.h"
#include <llvm/Support/CommandLine.h>

#include "your_path_to_llvm-src/llvm/lib/Transforms/LookOrigin/LookOrigin.h"
using namespace llvm;

static cl::opt<unsigned long long>
    cxtBudget("cxtbg", cl::init(10000),
              cl::desc("Maximum step budget of context-sensitive traversing"));




//[ECCut]
ValToTargetSet valtotargetset;
ValToTargetSet IcallToTargetset;
/*!
 * Constructor
 */
ContextDDA::ContextDDA(SVFModule m, DDAClient *client)
    : CondPTAImpl<ContextCond>(PointerAnalysis::Cxt_DDA),
      DDAVFSolver<CxtVar, CxtPtSet, CxtLocDPItem>(), _client(client) {
  flowDDA = new FlowDDA(m, client);
}

/*!
 * Destructor
 */
ContextDDA::~ContextDDA() {
  if (flowDDA)
    delete flowDDA;
  flowDDA = NULL;
}

/*!
 * Analysis initialization
 */
void ContextDDA::initialize(SVFModule module) {
  CondPTAImpl<ContextCond>::initialize(module);
  buildSVFG(module);
  setCallGraph(getPTACallGraph());
  setCallGraphSCC(getCallGraphSCC());
  stat = setDDAStat(new DDAStat(this));
  flowDDA->initialize(module);
}

// [OS-CFI] getSVFGForCandidateNode(): return SVFG node of candidate node id
const SVFGNode *ContextDDA::getSVFGForCandidateNode(NodeID id) {
  return candidateSVFG[id];
}

// [OS-CFI] getOriginSensitiveTupleSet(): return OriginSensitiveTupleSet of a
// candidate node id
OriginSensitiveTupleSet *ContextDDA::getOriginSensitiveTupleSet(NodeID id) {
  return mapSOrgSenTupSet[id];
}

// [OS-CFI] getCSSensitiveSet(): return CallStackSet of a
// candidate node id
CallStackSet *ContextDDA::getCSSensitiveSet(NodeID id) { return mapCSSen[id]; }

/*!
 * Compute points-to set for a context-sensitive pointer
 */
const CxtPtSet &ContextDDA::computeDDAPts(const CxtVar &var, int flag) {

  resetQuery();
  LocDPItem::setMaxBudget(cxtBudget);

  NodeID id = var.get_id();
  PAGNode *node = getPAG()->getPAGNode(id);
  //[OS-CFI] map candidate node to their SVFG
  const SVFGNode *snode = getDefSVFGNode(node);
  setCurCandidate(id, snode);

  CxtLocDPItem dpm = getDPIm(var, snode);
  // [OS-CFI] Debug code:
  if (DEBUG_SOLVER) {
    llvm::outs() << "[OS-CFI] ComputerDDAPts() for node[" << dpm.getCurNodeID()
                 << "] => SVFG: " << *snode << "["
                 << snode->getBB()
                        ->getFirstNonPHIOrDbgOrLifetime()
                        ->getFunction()
                        ->getName()
                 << "]\n";
  }

  // start DDA analysis
  DOTIMESTAT(double start = DDAStat::getClk());
  const CxtPtSet &cpts = findPT(dpm);
  DOTIMESTAT(ddaStat->_AnaTimePerQuery = DDAStat::getClk() - start);
  DOTIMESTAT(ddaStat->_TotalTimeOfQueries += ddaStat->_AnaTimePerQuery);

  if (isOutOfBudgetQuery() == false)
    unionPts(var, cpts);
  else
    handleOutOfBudgetDpm(dpm);

  if (this->printStat())
    DOSTAT(stat->performStatPerQuery(id));
  DBOUT(DGENERAL, stat->printStatPerQuery(id, getBVPointsTo(getPts(var))));

//[ECCut]
    if(flag == 1) {
        if(this->getPts(var).empty() ) {
            CxtPtSet cset = {};
            return cset;
        }
        for (CPtSet::iterator iit = this->getPts(var).begin(); iit != this->getPts(var).end(); iit++) {
            valtotargetset.insert(iit->get_id());
        }
        return this->getPts(var);
    } else if(flag == 2) {
        if( this->getPts(var).empty() ) {
            CxtPtSet cset = {};
            return cset;
        }
        for (CPtSet::iterator iit = this->getPts(var).begin(); iit != this->getPts(var).end(); iit++) {
            IcallToTargetset.insert(iit->get_id());
        }
        return this->getPts(var);
    }

  return this->getPts(var);
}


/*!
 *  Compute points-to set for an unconditional pointer
 */
void ContextDDA::computeDDAPts(NodeID id, int flag) {
    ContextCond cxt;
    CxtVar var(cxt, id);
    computeDDAPts(var,flag);

}

/*!
 * Handle out-of-budget dpm
 */
void ContextDDA::handleOutOfBudgetDpm(const CxtLocDPItem &dpm) {

  DBOUT(
      DGENERAL,
      outs()
          << "~~~Out of budget query, downgrade to flow sensitive analysis \n");
  flowDDA->computeDDAPts(dpm.getCurNodeID());
  const PointsTo &flowPts = flowDDA->getPts(dpm.getCurNodeID());
  CxtPtSet cxtPts;
  for (PointsTo::iterator it = flowPts.begin(), eit = flowPts.end(); it != eit;
       ++it) {
    ContextCond cxt;
    CxtVar var(cxt, *it);
    cxtPts.set(var);
  }
  updateCachedPointsTo(dpm, cxtPts);
  unionPts(dpm.getCondVar(), cxtPts);
  addOutOfBudgetDpm(dpm);
}

/*!
 * context conditions of local(not in recursion)  and global variables are
 * compatible
 */
bool ContextDDA::isCondCompatible(const ContextCond &cxt1,
                                  const ContextCond &cxt2,
                                  bool singleton) const {
  if (singleton)
    return true;

  int i = cxt1.cxtSize() - 1;
  int j = cxt2.cxtSize() - 1;
  for (; i >= 0 && j >= 0; i--, j--) {
    if (cxt1[i] != cxt2[j])
      return false;
  }
  return true;
}

/*!
 * Generate field objects for structs
 */
CxtPtSet ContextDDA::processGepPts(const GepSVFGNode *gep,
                                   const CxtPtSet &srcPts) {
  CxtPtSet tmpDstPts;
  for (CxtPtSet::iterator piter = srcPts.begin(); piter != srcPts.end();
       ++piter) {

    CxtVar ptd = *piter;
    if (isBlkObjOrConstantObj(ptd.get_id()))
      tmpDstPts.set(ptd);
    else {
      if (isa<VariantGepPE>(gep->getPAGEdge())) {
        setObjFieldInsensitive(ptd.get_id());
        CxtVar var(ptd.get_cond(), getFIObjNode(ptd.get_id()));
        tmpDstPts.set(var);
      } else if (const NormalGepPE *normalGep =
                     dyn_cast<NormalGepPE>(gep->getPAGEdge())) {
        CxtVar var(ptd.get_cond(),
                   getGepObjNode(ptd.get_id(), normalGep->getLocationSet()));
        tmpDstPts.set(var);
      } else
        assert(false && "new gep edge?");
    }
  }

  // [OS-CFI] ToDo
  for (CxtPtSet::iterator siter = srcPts.begin(); siter != srcPts.end();
       ++siter) {
    CxtVar src = *siter;
    for (CxtPtSet::iterator diter = tmpDstPts.begin(); diter != tmpDstPts.end();
         ++diter) {
      CxtVar dst = *diter;
      if (mapNodeStore.count(src.get_id()) > 0) {
        mapNodeStore[dst.get_id()] = mapNodeStore[src.get_id()];
        if (DEBUG_SOLVER) {
          llvm::outs() << "[OS-CFI] mapNodeStore[" << dst.get_id()
                       << "] = mapNodeStore[" << src.get_id() << "]\n";
          if (DEBUG_DETAILS) {
            llvm::outs() << "[OS-CFI] Store Instruction: "
                         << *(mapNodeStore[src.get_id()]->getInst()) << "\n";
          }
        }
        if (mapTOrgCtx.count(src.get_id()) > 0) {
          for (InstructionSetIt it = mapTOrgCtx[src.get_id()].begin();
               it != mapTOrgCtx[src.get_id()].end(); it++) {
            if (*it != nullptr) {
//              mapSOrgSenTupSet[getCurCandidate()]->insert(std::make_tuple(
//                  dst.get_id()));
              if (DEBUG_SOLVER) {
                llvm::outs()
                    << "[OS-CFI] mapSOrgSenTupSet[" << getCurCandidate()
                    << "] <= <" << dst.get_id() << ", "
                    << "mapNodeStore[" << src.get_id() << "], "
                    << "mapTOrgCtx[" << src.get_id() << "]"
                    << ">\n";
              }
            } else {
//              mapSOrgSenTupSet[getCurCandidate()]->insert(std::make_tuple(
//                  dst.get_id()));
              if (DEBUG_SOLVER) {
                llvm::outs()
                    << "[oCFG-Count] mapSOrgSenTupSet[" << getCurCandidate()
                    << "] <= <" << dst.get_id() << ", "
                    << "mapNodeStore[" << src.get_id() << "], "
                    << "nullptr]"
                    << ">\n";
              }
            }
          }
        } else {
//          mapSOrgSenTupSet[getCurCandidate()]->insert(std::make_tuple(
//              dst.get_id()));
          if (DEBUG_SOLVER) {
            llvm::outs() << "[oCFG-Count] mapSOrgSenTupSet["
                         << getCurCandidate() << "] <= <" << dst.get_id()
                         << ", "
                         << "mapNodeStore[" << src.get_id() << "], "
                         << "nullptr]"
                         << ">\n";
          }
        }
      }
    }
  }

  DBOUT(DDDA, outs() << "\t return created gep objs ");
  DBOUT(DDDA, outs() << srcPts.toString());
  DBOUT(DDDA, outs() << " --> ");
  DBOUT(DDDA, outs() << tmpDstPts.toString());
  DBOUT(DDDA, outs() << "\n");
  return tmpDstPts;
}

bool ContextDDA::testIndCallReachability(CxtLocDPItem &dpm,
                                         const llvm::Function *callee,
                                         llvm::CallSite cs) {
  if (getPAG()->isIndirectCallSites(cs)) {
    NodeID id = getPAG()->getFunPtr(cs);
    PAGNode *node = getPAG()->getPAGNode(id);
    CxtVar funptrVar(dpm.getCondVar().get_cond(), id);
    CxtLocDPItem funptrDpm = getDPIm(funptrVar, getDefSVFGNode(node));
    PointsTo pts = getBVPointsTo(findPT(funptrDpm));
    if (pts.test(getPAG()->getObjectNode(callee)))
      return true;
    else
      return false;
  }
  return true;
}

/*!
 * get callsite id from call, return 0 if it is a spurious call edge
 * translate the callsite id from pre-computed callgraph on SVFG to the one on
 * current callgraph
 */
CallSiteID ContextDDA::getCSIDAtCall(CxtLocDPItem &dpm, const SVFGEdge *edge) {

  CallSiteID svfg_csId = 0;
  if (const CallDirSVFGEdge *callEdge = dyn_cast<CallDirSVFGEdge>(edge))
    svfg_csId = callEdge->getCallSiteId();
  else
    svfg_csId = cast<CallIndSVFGEdge>(edge)->getCallSiteId();

  CallSite cs = getSVFG()->getCallSite(svfg_csId);
  const Function *callee = edge->getDstNode()->getBB()->getParent();

  if (getPTACallGraph()->hasCallSiteID(cs, callee)) {
    return getPTACallGraph()->getCallSiteID(cs, callee);
  }

  return 0;
}

/*!
 * get callsite id from return, return 0 if it is a spurious return edge
 * translate the callsite id from pre-computed callgraph on SVFG to the one on
 * current callgraph
 */
CallSiteID ContextDDA::getCSIDAtRet(CxtLocDPItem &dpm, const SVFGEdge *edge) {

  CallSiteID svfg_csId = 0;
  if (const RetDirSVFGEdge *retEdge = dyn_cast<RetDirSVFGEdge>(edge))
    svfg_csId = retEdge->getCallSiteId();
  else
    svfg_csId = cast<RetIndSVFGEdge>(edge)->getCallSiteId();

  CallSite cs = getSVFG()->getCallSite(svfg_csId);
  const Function *callee = edge->getSrcNode()->getBB()->getParent();

  if (getPTACallGraph()->hasCallSiteID(cs, callee)) {
    return getPTACallGraph()->getCallSiteID(cs, callee);
  }

  return 0;
}

/// Handle conditions during backward traversing
bool ContextDDA::handleBKCondition(CxtLocDPItem &dpm, const SVFGEdge *edge) {
  _client->handleStatement(edge->getSrcNode(), dpm.getCurNodeID());

  if (edge->isCallVFGEdge()) {
    /// we don't handle context in recursions, they treated as assignments
    if (CallSiteID csId = getCSIDAtCall(dpm, edge)) {

      if (isEdgeInRecursion(csId)) {
        DBOUT(DDDA,
              outs() << "\t\t call edge "
                     << getPTACallGraph()->getCallerOfCallSite(csId)->getName()
                     << "=>"
                     << getPTACallGraph()->getCalleeOfCallSite(csId)->getName()
                     << "in recursion \n");
        popRecursiveCallSites(dpm);
      } else {
        if (dpm.matchContext(csId) == false) {
          DBOUT(DDDA, outs()
                          << "\t\t context not match, edge " << edge->getDstID()
                          << " --| " << edge->getSrcID() << " \t");
          DBOUT(DDDA, dumpContexts(dpm.getCond()));
          return false;
        }

        DBOUT(DDDA, outs() << "\t\t match contexts ");
        DBOUT(DDDA, dumpContexts(dpm.getCond()));
      }
    }
  }

  else if (edge->isRetVFGEdge()) {
    /// we don't handle context in recursions, they treated as assignments
    if (CallSiteID csId = getCSIDAtRet(dpm, edge)) {

      if (isEdgeInRecursion(csId)) {
        DBOUT(DDDA,
              outs() << "\t\t return edge "
                     << getPTACallGraph()->getCalleeOfCallSite(csId)->getName()
                     << "=>"
                     << getPTACallGraph()->getCallerOfCallSite(csId)->getName()
                     << "in recursion \n");
        popRecursiveCallSites(dpm);
      } else {
        /// TODO: When this call site id is contained in current call string, we
        /// may find a recursion. Try
        ///       to solve this later.
        if (dpm.getCond().containCallStr(csId)) {
          outOfBudgetQuery = true;
          analysisUtil::wrnMsg(
              "Call site ID is contained in call string. Is this a recursion?");
          return false;
        } else {
          assert(dpm.getCond().containCallStr(csId) == false &&
                 "contain visited call string ??");
          if (dpm.pushContext(csId)) {
            DBOUT(DDDA, outs() << "\t\t push context ");
            DBOUT(DDDA, dumpContexts(dpm.getCond()));
          } else {
            DBOUT(DDDA, outs() << "\t\t context is full ");
            DBOUT(DDDA, dumpContexts(dpm.getCond()));
          }
        }
      }
    }
  }

  return true;
}

/// we exclude concrete heap given the following conditions:
/// (1) concrete calling context (not involved in recursion and not exceed the
/// maximum context limit) (2) not inside loop
bool ContextDDA::isHeapCondMemObj(const CxtVar &var,
                                  const StoreSVFGNode *store) {
  const MemObj *mem = _pag->getObject(getPtrNodeID(var));
  assert(mem && "memory object is null??");
  if (mem->isHeap()) {
    if (const Instruction *mallocSite =
            dyn_cast<Instruction>(mem->getRefVal())) {
      const Function *fun = mallocSite->getParent()->getParent();
      if (_ander->isInRecursion(fun))
        return true;
      if (var.get_cond().isConcreteCxt() == false)
        return true;
      if (loopInfoBuilder.getLoopInfo(fun)->getLoopFor(mallocSite->getParent()))
        return true;
    }
  }
  return false;
}
