; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt < %s -basicaa -slp-vectorizer -S -mcpu=corei7-avx | FileCheck %s
; RUN: opt < %s -basicaa -slp-vectorizer -slp-max-reg-size=128 -S -mcpu=corei7-avx | FileCheck %s --check-prefix=V128

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define void @foo(double* %x) {
; CHECK-LABEL: @foo(
; CHECK-NEXT:    [[TMP1:%.*]] = getelementptr inbounds double, double* [[X:%.*]], i64 1
; CHECK-NEXT:    [[TMP2:%.*]] = getelementptr inbounds double, double* [[X]], i64 2
; CHECK-NEXT:    [[TMP3:%.*]] = getelementptr inbounds double, double* [[X]], i64 3
; CHECK-NEXT:    [[TMP4:%.*]] = bitcast double* [[X]] to <4 x double>*
; CHECK-NEXT:    [[TMP5:%.*]] = load <4 x double>, <4 x double>* [[TMP4]], align 8
; CHECK-NEXT:    [[TMP6:%.*]] = fadd <4 x double> [[TMP5]], [[TMP5]]
; CHECK-NEXT:    [[TMP7:%.*]] = fadd <4 x double> [[TMP6]], [[TMP5]]
; CHECK-NEXT:    [[TMP8:%.*]] = bitcast double* [[X]] to <4 x double>*
; CHECK-NEXT:    store <4 x double> [[TMP7]], <4 x double>* [[TMP8]], align 8
; CHECK-NEXT:    ret void
;
; V128-LABEL: @foo(
; V128-NEXT:    [[TMP1:%.*]] = getelementptr inbounds double, double* [[X:%.*]], i64 1
; V128-NEXT:    [[TMP2:%.*]] = bitcast double* [[X]] to <2 x double>*
; V128-NEXT:    [[TMP3:%.*]] = load <2 x double>, <2 x double>* [[TMP2]], align 8
; V128-NEXT:    [[TMP4:%.*]] = fadd <2 x double> [[TMP3]], [[TMP3]]
; V128-NEXT:    [[TMP5:%.*]] = fadd <2 x double> [[TMP4]], [[TMP3]]
; V128-NEXT:    [[TMP6:%.*]] = bitcast double* [[X]] to <2 x double>*
; V128-NEXT:    store <2 x double> [[TMP5]], <2 x double>* [[TMP6]], align 8
; V128-NEXT:    [[TMP7:%.*]] = getelementptr inbounds double, double* [[X]], i64 2
; V128-NEXT:    [[TMP8:%.*]] = getelementptr inbounds double, double* [[X]], i64 3
; V128-NEXT:    [[TMP9:%.*]] = bitcast double* [[TMP7]] to <2 x double>*
; V128-NEXT:    [[TMP10:%.*]] = load <2 x double>, <2 x double>* [[TMP9]], align 8
; V128-NEXT:    [[TMP11:%.*]] = fadd <2 x double> [[TMP10]], [[TMP10]]
; V128-NEXT:    [[TMP12:%.*]] = fadd <2 x double> [[TMP11]], [[TMP10]]
; V128-NEXT:    [[TMP13:%.*]] = bitcast double* [[TMP7]] to <2 x double>*
; V128-NEXT:    store <2 x double> [[TMP12]], <2 x double>* [[TMP13]], align 8
; V128-NEXT:    ret void
;
  %1 = load double, double* %x, align 8
  %2 = fadd double %1, %1
  %3 = fadd double %2, %1
  store double %3, double* %x, align 8
  %4 = getelementptr inbounds double, double* %x, i64 1
  %5 = load double, double* %4, align 8
  %6 = fadd double %5, %5
  %7 = fadd double %6, %5
  store double %7, double* %4, align 8
  %8 = getelementptr inbounds double, double* %x, i64 2
  %9 = load double, double* %8, align 8
  %10 = fadd double %9, %9
  %11 = fadd double %10, %9
  store double %11, double* %8, align 8
  %12 = getelementptr inbounds double, double* %x, i64 3
  %13 = load double, double* %12, align 8
  %14 = fadd double %13, %13
  %15 = fadd double %14, %13
  store double %15, double* %12, align 8
  ret void
}
