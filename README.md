# ECCut
Boosting Practical Control-Flow Integrity with Complete Field Sensitivity and Origin Awareness
The new techniques significantly improve the security of CFI by reducing the largest and average EC sizes. By optimizing the instrumenting code and the verification process, our system incurs only a small overhead.

## Project Structure
* clang: ECCut/clang/ItaniumCXXABI.cpp: Instrumentation for virtual call reference monitor.
* llvm: ECCut source code for static analysis and others
  * ECCut/pass/Monitor: Instrumentation for indirect call reference monitor
  * ECCut/pass/Hello: Static Analysis and instrumentation for origin-awareness
  * ECCut/pass/LookOrigin: Supporting tools for SVF
  * ECCut/pass/instCFG: Insert CFG into bc file
* svf-src: We build this based on OS-CFI's svf-src 
  * lib/DDA Point-to analysis and employment Complete field sensitivity for ECCut

## Overall Process
* Step 1: Install required binary
* Step 2: Download and build binutils
* Step 3: Build the LLVM compiler
* Step 4: Build svf-src

## Installation Guideline
1. Install required binary

```text
sudo apt install cmake g++ gcc python bash git python3-pip radare2
pip3 install r2pipe
sudo apt-get install linux-headers-$(uname -r) csh gawk automake libtool bison flex libncurses5-dev
# Check 'makeinfo -v'. If 'makeinfo' does not exist
sudo apt-get install apt-file texinfo texi2html
sudo apt-file update
sudo apt-file search makeinfo
```
2. Download and build binutils
```text
cd ~
git clone --depth 1 git://sourceware.org/git/binutils-gdb.git binutils
mkdir build
cd build
../binutils/configure --enable-gold --enable-plugins --disable-werror
```
3. Build the LLVM compiler
```text
cd /your_path_to_llvm-src
mkdir build
cd build
cmake -DLLVM_BINUTILS_INCDIR="~/binutils/include" -G "Unix Makefiles" -DLLVM_ENABLE_PROJECTS="clang;lld;clang-tools-extra;polly;openmp;compiler-rt" -DCMAKE_BUILD_TYPE=Release ../llvm
make -j4
```

4. Build svf-src
```text
cd ECCut/svf-src-eccut
#replace svf-src-eccut/include/MemoryModel/PointerAnalysis.h:19 your_path_to_llvm-src
#replace svf-src-eccut/lib/CMakeLists.txt:58 your_path_to_llvm-src
#replace svf-src-eccut/tools/OSCFG/oscfg.cpp:17 your_path_to_llvm-src
#replace svf-src-eccut/lib/DDA/ContextDDA.cpp:11 your_path_to_llvm-src
export LLVM_SRC=your_path_to_llvm-src/llvm
export LLVM_OBJ=your_path_to_llvm-src/build
export LLVM_DIR=your_path_to_llvm-src/build
export PATH=$LLVM_DIR/bin:$PATH

mkdir build
cd build
cmake -D ../
make -j4
```





