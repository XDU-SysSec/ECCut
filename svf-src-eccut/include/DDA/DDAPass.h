/*
 * Origin-sensitive Control Flow Integrity
 * Author: Mustakimur R. Khandaker (mrk15e@my.fsu.edu)
 * Affliation: Florida State University
 */
#ifndef WPA_H_
#define WPA_H_

#include "DDA/DDAClient.h"
#include "MemoryModel/PointerAnalysis.h"
#include "Util/SCC.h"
#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/Pass.h>
#include <queue>
#define DUMP_CFG_DEBUG 1       // [OS-CFI] CFG DUMP FLAG
#define HASH_ID_RANGE 10000000 // [OS-CFI] unique hash id range

class SVFG;
class SVFGEdge;

/*!
 * Demand-Driven Pointer Analysis.
 * This class performs various pointer analysis on the given module.
 */

// [OS-CFI] we fix SUPA errored points-to for two reasons: 1) it points-to all
// address-taken 2) it points-to empty
typedef enum FIX_TYPE { OVER_APPROXIMATE = 1, UNDER_APPROXIMATE = 2 } fixType;
// [OC-CFI] we will have four different kinds of CFG
// 1) SUPA generated CI-CFG (Error included)
// 2) Origin Sensitive CFG (oCFG)
// 3) Callsite Sensitive CFG (cCFG)
// 4) Type and address taken CFG (Fix the error)
typedef enum CFG_TYPE { OCFG = 1, ATCFG = 2 } cfgType;
typedef enum TARGET_TYPE { FUNC = 1, VT = 2 } targetType;

// [OS-CFI] oCFG Data Structure
// Origin Context can be null
typedef struct oCFGData {
  const llvm::Instruction *iCallInst;
  unsigned long iCallID;
  const llvm::Value *iCallTarget;
  unsigned long iCallTargetID;
  unsigned long originID;
  const llvm::Value *originCTX;
  unsigned long originCTXID;
} oCFG;

// [OS-CFI] cCFG Data Structure
// # of callsite will be variable
typedef struct cCFGData {
  const llvm::Instruction *iCallInst;
  unsigned long iCallID;
  const llvm::Value *iCallTarget;
  unsigned long iCallTargetID;
  std::vector<const llvm::Instruction *> *cInstStack;
  std::vector<unsigned long> *cIDStack;
} cCFG;

// [OS-CFI] Address Taken and Type Check CFG Data Structure
// the fixType is the enum defined above
typedef struct AddrTypeData {
  fixType type;
  const llvm::Instruction *iCallInst;
  unsigned long iCallID;
  const llvm::Value *iCallTarget;
  unsigned long iCallTargetID;
} atCFG;

// [OS-CFI] CI-CFG Data Structure
// Generated by SUPA
typedef struct SUPAData {
  const llvm::Instruction *iCallInst;
  unsigned long iCallID;
  const llvm::Value *iCallTarget;
  unsigned long iCallTargetID;
} supaCFG;

// [OS-CFI]Function* can map a set of Instruction*
typedef std::map<const llvm::Value *, unsigned long> ValToIDMap;
typedef std::map<const llvm::Value *, unsigned long>::iterator ValToIDMapIt;
typedef std::map<llvm::Instruction *, unsigned long> InstToIDMap;
typedef std::map<llvm::Instruction *, unsigned long>::iterator InstToIDMapIt;
typedef std::map<llvm::Function *, std::set<llvm::Instruction *>>
    FuncToInstSetMap;
typedef std::map<llvm::Function *, std::set<llvm::Instruction *>>::iterator
    FuncToInstSetMapIt;

// [OS-CFI] Function* can map a set of BasicBlock*
typedef std::map<llvm::Function *, std::set<llvm::BasicBlock *>> FuncToBBSetMap;
typedef std::map<llvm::Function *, std::set<llvm::BasicBlock *>>::iterator
    FuncToBBSetMapIt;

//icall_nodeid, icallID
typedef std::map<unsigned long, unsigned long> Icall;

//Vcall_To_Vatble
//typedef std::map<unsigned long, llvm::GlobalVariable> Vcall_To_Vatble;

//global_nodeid,globalvalue
typedef std::map<unsigned long, const llvm::GlobalValue *> Global;
typedef std::map<unsigned long, const llvm::GlobalValue *>::iterator GlobalIt;
typedef std::multimap<Icall, Global> IcallToGlobaltarget;
typedef std::multimap<Icall, Global>::iterator IcallToGlobaltargetIt;
//icall_num,OriginID
typedef std::multimap<unsigned long, unsigned long> IcallToOrigin;
//origin_target_nodeid,target
typedef std::map<unsigned long, const Value *> TargetSet;
typedef std::multimap<IcallToOrigin, TargetSet> OriginTargetSet;
typedef std::multimap<IcallToOrigin, TargetSet>::iterator OriginTargetSetIt;

// [OS-CFI] BasicBlock* can map to an unsigned long
typedef std::map<llvm::BasicBlock *, unsigned long> BBToIDMap;
typedef std::map<llvm::BasicBlock *, unsigned long>::iterator BBToIDMapIt;

// [OS-CFI] typedef container for CFGs
typedef std::vector<supaCFG *> SUPACFGList;
typedef std::vector<supaCFG *>::iterator SUPACFGListIt;
typedef std::vector<cCFG *> CCFGList;
typedef std::vector<cCFG *>::iterator CCFGListIt;
typedef std::vector<oCFG *> OCFGList;
typedef std::vector<oCFG *>::iterator OCFGListIt;
typedef std::vector<atCFG *> ATCFGList;
typedef std::vector<atCFG *>::iterator ATCFGListIt;
// [OS-CFI] typdef address taken function set
typedef std::set<llvm::Function *> FuncSet;
typedef std::set<llvm::Function *>::iterator FuncSetIt;

typedef std::set<unsigned long > TargetIDSet;
typedef std::pair<unsigned long, unsigned long > OriginPair;
typedef std::map<OriginPair, TargetIDSet > OriginToTargetIDMap;
typedef std::map<OriginPair, TargetIDSet >::iterator OriginToTargetIDMapIt;
typedef std::set<llvm::Value *> Process_GepInst;
typedef std::set<llvm::Value *>::reverse_iterator Process_GepInstReverseIt;
typedef std::set<llvm::Value *>::iterator Process_GepInstIt;
typedef std::map<llvm::Value *, unsigned long > GlobalStructSize;
typedef std::map<llvm::Value *, unsigned long >::iterator GlobalStructSizeIt;

class DDAPass : public llvm::ModulePass {
private:
  SUPACFGList supaCFGList; // [OS-CFI] List of SUPA CF-CFG Entry
  CCFGList cCFGList;       // [OS-CFI] List of CS-CFG Entry
  OCFGList oCFGList;       // [OS-CFI] List of OS-CFG Entry
  ATCFGList atCFGList;     // [OS-CFI] List of ADDRTY-CFG Entry
  FuncSet setAddrFunc;     // [OS-CFI] Set of ADDR Functions
  FuncSet setFunc;

  FuncToInstSetMap mapFnCSite; // [OS-CFI] ToDo
  InstToIDMap mapInstID;       // [OS-CFI] ToDo
  BBToIDMap mapBBID;           // [OS-CFI] ToDo
  FuncToBBSetMap mapFnBB;      // [OS-CFI] ToDo
  ValToIDMap mapValID;         // [OS-CFI] ToDo
  GlobalStructSize globalstructsize;
public:
  /// Pass ID
  static char ID;
  typedef SCCDetection<SVFG *> SVFGSCC;
  typedef std::set<const SVFGEdge *> SVFGEdgeSet;
  typedef std::vector<PointerAnalysis *> PTAVector;

  DDAPass() : llvm::ModulePass(ID), _pta(NULL), _client(NULL) {}
  ~DDAPass();

  virtual inline void getAnalysisUsage(llvm::AnalysisUsage &au) const {
    // declare your dependencies here.
    /// do not intend to change the IR in this pass,
    au.setPreservesAll();
  }

  virtual inline void *getAdjustedAnalysisPointer(llvm::AnalysisID id) {
    return this;
  }

  /// Interface expose to users of our pointer analysis, given Location infos
  virtual inline llvm::AliasResult alias(const llvm::MemoryLocation &LocA,
                                         const llvm::MemoryLocation &LocB) {
    return alias(LocA.Ptr, LocB.Ptr);
  }

  /// Interface expose to users of our pointer analysis, given Value infos
  virtual llvm::AliasResult alias(const llvm::Value *V1, const llvm::Value *V2);

  /// We start from here
  virtual bool runOnModule(SVFModule module);

  /// We start from here
  virtual bool runOnModule(llvm::Module &module) { return runOnModule(module); }

  /// Select a client
  virtual void selectClient(SVFModule module);

  /// Pass name
  virtual inline llvm::StringRef getPassName() const { return "DDAPass"; }

private:
  unsigned long getHashID(const llvm::Instruction *); // [OS-CFI] return unique
                                                      // id for an instruction
  void computeCFG(SVFModule);  // [OS-CFI] fill out CFG containers
  void my_computeCFG(SVFModule);
  bool IsIcallInOCFG(unsigned long);
    bool IsIcallInatCFG(unsigned long);
  void dumpSUPACFG(); // [OS-CFI] print SUPA CI-CFG
  void dumpoCFG();    // [OS-CFI] print OS-CFG
  void dumpcCFG();    // [OS-CFI] print CS-CFG
  void dumpatCFG();   // [OS-CFI] print ADDRTY-CFG
  void dumpGlobal();
  void labelForCSite(const llvm::Instruction *, unsigned long); // [OS-CFI] ToDo
  void createLabelForCS();                                      // [OS-CFI] ToDo
  void createLabelForValue(SVFModule);                          // [OS-CFI] ToDo
  bool isTypeMatch(const llvm::Instruction *,
                   const llvm::Value *); // [OS-CFI] test the type match between
                                         // sink and source
  bool fillEmptyPointsToSet(
      const llvm::Instruction *); // [OS-CFI] use address-taken type match cfg
                                  // for empty points-to set

  void FindValInGlobalStruct(const Value *Func, Value *global_var, std::vector<unsigned long > &vec, Process_GepInst *Process_Gep);
  unsigned long getunsigned(Value *Gep_Offset);
  /// Print queries' pts
  void printQueryPTS();
  /// Create pointer analysis according to specified kind and analyze the
  /// module.
  void runPointerAnalysis(SVFModule module, u32_t kind);
  /// Initialize queries for DDA
  void answerQueries(PointerAnalysis *pta);
  /// Context insensitive Edge for DDA
  void initCxtInsensitiveEdges(PointerAnalysis *pta, const SVFG *svfg,
                               const SVFGSCC *svfgSCC,
                               SVFGEdgeSet &insensitveEdges);
  /// Return TRUE if this edge is inside a SVFG SCC, i.e., src node and dst node
  /// are in the same SCC on the SVFG.
  bool edgeInSVFGSCC(const SVFGSCC *svfgSCC, const SVFGEdge *edge);
  /// Return TRUE if this edge is inside a SVFG SCC, i.e., src node and dst node
  /// are in the same SCC on the SVFG.
  bool edgeInCallGraphSCC(PointerAnalysis *pta, const SVFGEdge *edge);

  void collectCxtInsenEdgeForRecur(PointerAnalysis *pta, const SVFG *svfg,
                                   SVFGEdgeSet &insensitveEdges);
  void collectCxtInsenEdgeForVFCycle(PointerAnalysis *pta, const SVFG *svfg,
                                     const SVFGSCC *svfgSCC,
                                     SVFGEdgeSet &insensitveEdges);

  bool findFuncInVtable(const llvm::GlobalVariable* vtable, std::string Func_name);

  bool IsNotRepeat(atCFG *oItem);
  int64_t get_struct_mem_offset(Value *getelementptr_inst);
  bool IsNotRepeat(oCFG *oItem);     //[ECCut]
  PointerAnalysis *_pta; ///<  pointer analysis to be executed.
  DDAClient *_client;    ///<  DDA client used
};

#endif /* WPA_H_ */
