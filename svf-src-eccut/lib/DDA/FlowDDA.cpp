/*
 * Origin-sensitive Control Flow Integrity
 * Author: Mustakimur R. Khandaker (mrk15e@my.fsu.edu)
 * Affliation: Florida State University
 */
#include "DDA/FlowDDA.h"
#include "DDA/DDAClient.h"
#include "Util/AnalysisUtil.h"
#include <llvm/Support/CommandLine.h>

using namespace llvm;
using namespace std;
using namespace analysisUtil;

static cl::opt<unsigned long long>
    flowBudget("flowbg", cl::init(10000),
               cl::desc("Maximum step budget of flow-sensitive traversing"));

/*!
 * Compute points-to set for queries
 */
void FlowDDA::computeDDAPts(NodeID id) {
  resetQuery();
  LocDPItem::setMaxBudget(flowBudget);

  PAGNode *node = getPAG()->getPAGNode(id);

  //[OS-CFI] map candidate node to their SVFG
  const SVFGNode *snode = getDefSVFGNode(node);
  setCurCandidate(id, snode);

  LocDPItem dpm = getDPIm(node->getId(), getDefSVFGNode(node));

  /// start DDA analysis
  DOTIMESTAT(double start = DDAStat::getClk());
  const PointsTo &pts = findPT(dpm);
  DOTIMESTAT(ddaStat->_AnaTimePerQuery = DDAStat::getClk() - start);
  DOTIMESTAT(ddaStat->_TotalTimeOfQueries += ddaStat->_AnaTimePerQuery);

  if (isOutOfBudgetQuery() == false)
    unionPts(node->getId(), pts);
  else
    handleOutOfBudgetDpm(dpm);

  if (this->printStat())
    DOSTAT(stat->performStatPerQuery(node->getId()));

  DBOUT(DGENERAL, stat->printStatPerQuery(id, getPts(id)));
}

/*!
 * Handle out-of-budget dpm
 */
void FlowDDA::handleOutOfBudgetDpm(const LocDPItem &dpm) {
  DBOUT(DGENERAL,
        outs() << "~~~Out of budget query, downgrade to andersen analysis \n");
  PointsTo &anderPts = getAndersenAnalysis()->getPts(dpm.getCurNodeID());
  updateCachedPointsTo(dpm, anderPts);
  unionPts(dpm.getCurNodeID(), anderPts);
  addOutOfBudgetDpm(dpm);
}

bool FlowDDA::testIndCallReachability(LocDPItem &dpm,
                                      const llvm::Function *callee,
                                      CallSiteID csId) {

  CallSite cs = getSVFG()->getCallSite(csId);

  if (getPAG()->isIndirectCallSites(cs)) {
    if (getPTACallGraph()->hasIndCSCallees(cs)) {
      const FunctionSet &funset = getPTACallGraph()->getIndCSCallees(cs);
      if (funset.find(callee) != funset.end())
        return true;
    }

    return false;
  } else // if this is an direct call
    return true;
}

bool FlowDDA::handleBKCondition(LocDPItem &dpm, const SVFGEdge *edge) {
  _client->handleStatement(edge->getSrcNode(), dpm.getCurNodeID());
  //    CallSiteID csId = 0;
  //
  //    if (edge->isCallVFGEdge()) {
  //        /// we don't handle context in recursions, they treated as
  //        assignments if (const CallDirSVFGEdge* callEdge =
  //        dyn_cast<CallDirSVFGEdge>(edge))
  //            csId = callEdge->getCallSiteId();
  //        else
  //            csId = cast<CallIndSVFGEdge>(edge)->getCallSiteId();
  //
  //        const Function* callee = edge->getDstNode()->getBB()->getParent();
  //        if(testIndCallReachability(dpm,callee,csId)==false){
  //            return false;
  //        }
  //
  //    }
  //
  //    else if (edge->isRetVFGEdge()) {
  //        /// we don't handle context in recursions, they treated as
  //        assignments if (const RetDirSVFGEdge* retEdge =
  //        dyn_cast<RetDirSVFGEdge>(edge))
  //            csId = retEdge->getCallSiteId();
  //        else
  //            csId = cast<RetIndSVFGEdge>(edge)->getCallSiteId();
  //
  //        const Function* callee = edge->getSrcNode()->getBB()->getParent();
  //        if(testIndCallReachability(dpm,callee,csId)==false){
  //            return false;
  //        }
  //
  //    }

  return true;
}

/*!
 * Generate field objects for structs
 */
PointsTo FlowDDA::processGepPts(const GepSVFGNode *gep,
                                const PointsTo &srcPts) {
  PointsTo tmpDstPts;
  for (PointsTo::iterator piter = srcPts.begin(); piter != srcPts.end();
       ++piter) {
    NodeID ptd = *piter;
    if (isBlkObjOrConstantObj(ptd))
      tmpDstPts.set(ptd);
    else {
      if (isa<VariantGepPE>(gep->getPAGEdge())) {
        setObjFieldInsensitive(ptd);
        tmpDstPts.set(getFIObjNode(ptd));
      } else if (const NormalGepPE *normalGep =
                     dyn_cast<NormalGepPE>(gep->getPAGEdge())) {
        NodeID fieldSrcPtdNode =
            getGepObjNode(ptd, normalGep->getLocationSet());
        tmpDstPts.set(fieldSrcPtdNode);
      } else
        assert(false && "new gep edge?");
    }
  }

  // [OS-CFI] ToDo
  for (PointsTo::iterator siter = srcPts.begin(); siter != srcPts.end();
       ++siter) {
    NodeID src = *siter;
    for (PointsTo::iterator diter = tmpDstPts.begin(); diter != tmpDstPts.end();
         ++diter) {
      NodeID dst = *diter;
      if (mapNodeStore.count(src) > 0) {
        mapNodeStore[dst] = mapNodeStore[src];
        if (DEBUG_SOLVER) {
          llvm::outs() << "[OS-CFI] mapNodeStore[" << dst << "] = mapNodeStore["
                       << src << "]\n";
          if (DEBUG_DETAILS) {
            llvm::outs() << "[OS-CFI] Store Instruction: "
                         << *(mapNodeStore[src]->getInst()) << "\n";
          }
        }
        if (mapTOrgCtx.count(src) > 0) {
          for (InstructionSetIt it = mapTOrgCtx[src].begin();
               it != mapTOrgCtx[src].end(); it++) {
            if (*it != nullptr) {
//              mapSOrgSenTupSet[getCurCandidate()]->insert(
//                  std::make_tuple(dst));
              if (DEBUG_SOLVER) {
                llvm::outs() << "[OS-CFI] mapSOrgSenTupSet["
                             << getCurCandidate() << "] <= <" << dst << ", "
                             << "mapNodeStore[" << src << "], "
                             << "mapTOrgCtx[" << src << "]"
                             << ">\n";
              }
            } else {
//              mapSOrgSenTupSet[getCurCandidate()]->insert(
//                  std::make_tuple(dst));
              if (DEBUG_SOLVER) {
                llvm::outs() << "[oCFG-Count] mapSOrgSenTupSet["
                             << getCurCandidate() << "] <= <" << dst << ", "
                             << "mapNodeStore[" << src << "], "
                             << "nullptr]"
                             << ">\n";
              }
            }
          }
        } else {
//          mapSOrgSenTupSet[getCurCandidate()]->insert(
//              std::make_tuple(dst));
          if (DEBUG_SOLVER) {
            llvm::outs() << "[oCFG-Count] mapSOrgSenTupSet["
                         << getCurCandidate() << "] <= <" << dst << ", "
                         << "mapNodeStore[" << src << "], "
                         << "nullptr]"
                         << ">\n";
          }
        }
      }
    }
  }

  DBOUT(DDDA, outs() << "\t return created gep objs {");
  DBOUT(DDDA, analysisUtil::dumpSet(srcPts));
  DBOUT(DDDA, outs() << "} --> {");
  DBOUT(DDDA, analysisUtil::dumpSet(tmpDstPts));
  DBOUT(DDDA, outs() << "}\n");
  return tmpDstPts;
}

/// we exclude concrete heap here following the conditions:
/// (1) local allocated heap and
/// (2) not escaped to the scope outside the current function
/// (3) not inside loop
/// (4) not involved in recursion
bool FlowDDA::isHeapCondMemObj(const NodeID &var, const StoreSVFGNode *store) {
  const MemObj *mem = _pag->getObject(getPtrNodeID(var));
  assert(mem && "memory object is null??");
  if (mem->isHeap()) {
    //        if(const Instruction* mallocSite =
    //        dyn_cast<Instruction>(mem->getRefVal())) {
    //            const Function* fun = mallocSite->getParent()->getParent();
    //            const Function* curFun = store->getBB() ?
    //            store->getBB()->getParent() : NULL; if(fun!=curFun)
    //                return true;
    //            if(_callGraphSCC->isInCycle(_callGraph->getCallGraphNode(fun)->getId()))
    //                return true;
    //            if(loopInfoBuilder.getLoopInfo(fun)->getLoopFor(mallocSite->getParent()))
    //                return true;
    //
    //            return false;
    //        }
    return true;
  }
  return false;
}
