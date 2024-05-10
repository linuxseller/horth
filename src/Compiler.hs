module Compiler where

import Data.Memory
import Data.Parser
import Parser

header = unlines [
  ".text                "
 ,"# print             "
 ,"print:              "
 ,"  popq %rcx         "
 ,"  popq %rax         "
 ,"  pushq %rcx        "
 ,"  mov %rsp, %rbp    "
 ,"  dec %rsp          "
 ,"  movb $0x0A, (%rsp)"
 ,"  jmp .p_loop_cond  "
 ,"  .p_loop:          "
 ,"    mov $10, %rbx   "
 ,"    div %rbx        "
 ,"    add $0x30, %dl  "
 ,"    dec %rsp        "
 ,"    mov %dl, (%rsp) "
 ,"  .p_loop_cond:     "
 ,"    test %rax, %rax "
 ,"    jnz .p_loop     "
 ,"  mov $1,   %rax    "
 ,"  mov $1,   %rdi    "
 ,"  mov %rsp, %rsi    "
 ,"  mov %rbp, %rdx    "
 ,"  sub %rsp, %rdx    "
 ,"  syscall           "
 ,"  mov %rbp, %rsp    "
 ,"  ret               "
 ,"#print              "
 ,".globl _start       "
 ,"_start:\n"]

footer = unlines [
  "movl $1, %eax"
 ,"popq  %rbx"
 ,"int  $0x80\n"]

compCommand :: AST -> String
compCommand AstAdd = unlines [
  "# add         "
 ,"popq %rax     "
 ,"popq %rbx     "
 ,"add %rbx, %rax"
 ,"push %rax\n"]

compCommand AstSub = unlines [
  "# sub         "
 ,"popq %rax      "
 ,"popq %rbx      "
 ,"sub %rbx, %rax "
 ,"push %rax\n"]

compCommand AstMul = unlines [
  "# mul          "
 ,"popq %rax       "
 ,"popq %rbx       "
 ,"imul %rbx, %rax "
 ,"push %rax\n"]

compCommand AstInc = unlines [
  "# inc        "
 ,"popq  %rax    "
 ,"inc   %rax    "
 ,"pushq %rax\n"]

compCommand AstDec = unlines [
  "# dec        "
 ,"popq  %rax    "
 ,"dec   %rax    "
 ,"pushq %rax\n"]

compCommand AstSwap = unlines [
  "# swap       "
  ,"popq  %rax   "
 ,"popq  %rbx    "
 ,"pushq %rax    "
 ,"pushq %rbx\n"]

compCommand AstDup =unlines [
  "# dup       "
  ,"popq  %rax  "
  ,"pushq %rax  "
  ,"pushq %rax\n"]

compCommand (AstPush (AstNum val)) = unlines [
  "# push                   "
  ,"pushq $" <> show val <> "\n"]

compCommand AstPop = unlines [
  "# pop"
  ,"popq %rax,\n"]

compCommand AstPrint = "call print\n"
compCommand _ = ""

splitOn :: Eq a => a -> [a] -> [a] -> ([a], [a])
splitOn _ _ [] = ([],[])
splitOn elem pref (x:xs)
  | elem == x = (pref, xs)
  | otherwise = splitOn elem (pref++[x]) xs

compileLoop :: [AST] -> String
compileLoop ast = unlines {
  ".loop:\n"
 ,(compileH ast)
 ,".check_loop:"
 ,"test %rax, %rax"
 ,"jnz .loop\n"
]

compileH :: [AST] -> String
compileH [] = ""
compileH [x] = compCommand x
compileH (AstWhile:xs) = compileLoop [] ++ compileH xs
compileH (x:xs) = compCommand x ++ compileH xs

compile commands = header ++ compileH commands ++ footer
