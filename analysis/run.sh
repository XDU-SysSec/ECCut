CC=$LLVM_PATH/build/bin/clang
CXX=$LLVM_PATH/build/bin/clang++
DIS=$LLVM_PATH/build/bin/llvm-dis
OPT=$LLVM_PATH/build/bin/opt
OSCFG=../svf-src-eccut/build/bin/oscfg
PYSCRIPT=./dumpData.py
LLC=$LLVM_PATH/build/bin/llc
CFG=$LLVM_PATH/build/lib/LLVMInstCFG.so
Hello=$LLVM_PATH/build/lib/LLVMHello.so
Monitor=$LLVM_PATH/build/lib/LLVMMonitor.so
DIS=$LLVM_PATH/build/bin/llvm-dis
Optimization=$LLVM_PATH/build/lib/LLVMOptimizeCode.so


echo "Enter project name (Target binary name): "
read progName

echo "Monitor pass"
$OPT -load $Monitor -monitor "$progName"".bc" > "$progName"".monitor.bc" 

echo "Hello pass"
time $OPT -load $Hello -hello "$progName"".monitor.bc" > "$progName"".hello.bc"

echo "Static points-to analysis CFG generation ..."
time $OSCFG -svfmain -cxt -query=funptr -maxcxt=3 -flowbg=1000 -cxtbg=10000 -cpts -print-query-pts "$progName"".hello.bc" > "outs.txt" 2> "stats.bin"

echo "link cfi.bc"
CC -c -emit-llvm cfi.c -o cfi.bc
llvm-link cfi.bc "$progName"".hello.oscfg.bc" > "$progName"".hello.oscfg.link.bc"

echo "Generating the binary ..."
$LLC -filetype=obj "$progName"".hello.oscfg.bc"
$CXX -mmpx -pthread -O0  "$progName"".hello.oscfg.o" -o "$progName""_dump"

echo "Dumping CFG table and run python script ..."
objdump -s -j cfg_label_tracker "$progName""_dump" > dump_table.bin
python $PYSCRIPT "$progName""_dump"
cp dump_table.bin dump_table.back

echo "Optimization phase ..."
$OPT -load $CFG -llvm-inst-cfg  "$progName"".hello.oscfg.bc" > "$progName"".hello.oscfg.cfg.bc"
#$DIS "$progName"".monitor.oscfg.opt.bc"

echo "Optimization to update_mpx_table"
$OPT -load $Optimization -optimizecode "$progName"".hello.oscfg.cfg.bc" > "$progName"".hello.oscfg.opt.bc"

echo "Generating the secure binary with optimization ..."
$LLC -filetype=obj "$progName"".hello.oscfg.opt.bc"
$CXX -mmpx -pthread -O0 "$progName"".hello.oscfg.opt.o" -o "$progName""_exec"

