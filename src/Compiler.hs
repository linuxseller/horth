module Compiler where

import Data.Memory
import Data.Parser
import Parser

header =
  ".text                \n\
  \# print              \n\
  \print:               \n\
  \  popq %rcx          \n\
  \  popq %rax          \n\
  \  pushq %rcx         \n\
  \  mov %rsp, %rbp     \n\
  \  dec %rsp           \n\
  \  movb $0x0A, (%rsp) \n\
  \  jmp .p_loop_cond   \n\
  \  .p_loop:           \n\
  \    mov $10, %rbx    \n\
  \    div %rbx         \n\
  \    add $0x30, %dl   \n\
  \    dec %rsp         \n\
  \    mov %dl, (%rsp)  \n\
  \  .p_loop_cond:      \n\
  \    test %rax, %rax  \n\
  \    jnz .p_loop      \n\
  \  mov $1,   %rax     \n\
  \  mov $1,   %rdi     \n\
  \  mov %rsp, %rsi     \n\
  \  mov %rbp, %rdx     \n\
  \  sub %rsp, %rdx     \n\
  \  syscall            \n\
  \  mov %rbp, %rsp     \n\
  \  ret                \n\
  \#print               \n\
  \.globl _start        \n\
  \_start:\n"

footer =
  "movl $1, %eax\n\
  \popq  %rbx   \n\
  \int  $0x80   \n"

compCommand :: AST -> String
compCommand AstAdd =
  "# add         \n\
 \popq %rax      \n\
 \popq %rbx      \n\
 \add %rbx, %rax \n\
 \push %rax\n"

compCommand AstSub =
  "# sub         \n\
 \popq %rax      \n\
 \popq %rbx      \n\
 \sub %rbx, %rax \n\
 \push %rax\n"

compCommand AstMul =
  "# mul          \n\
 \popq %rax       \n\
 \popq %rbx       \n\
 \imul %rbx, %rax \n\
 \push %rax\n"

compCommand AstInc =
  "# inc        \n\
 \popq  %rax    \n\
 \inc   %rax    \n\
 \pushq %rax\n"

compCommand AstDec =
  "# dec        \n\
 \popq  %rax    \n\
 \dec   %rax    \n\
 \pushq %rax\n"

compCommand AstSwap =
  "# swap       \n\
  \popq  %rax   \n\
 \popq  %rbx    \n\
 \pushq %rax    \n\
 \pushq %rbx\n"

compCommand AstDup =
  "# dup       \n\
  \popq  %rax  \n\
  \pushq %rax  \n\
  \pushq %rax\n"

compCommand (AstPush (AstNum val)) =
  "# push                   \n\
  \pushq $" <> show val <> "\n"

compCommand AstPop =
  "# pop    \n\
  \popq %rax\n"

compCommand AstPrint = "call print\n"
compCommand _ = ""

splitOn :: Eq a => a -> [a] -> [a] -> ([a], [a])
splitOn _ _ [] = ([],[])
splitOn elem pref (x:xs)
  | elem == x = (pref, xs)
  | otherwise = splitOn elem (pref++[x]) xs

compileLoop :: [AST] -> String
compileLoop ast =
  ".loop:\n" ++ (compileH ast) ++ ".check_loop:\n\
 \test %rax, %rax\n\
 \jnz .loop\n"

compileH :: [AST] -> String
compileH [] = ""
compileH [x] = compCommand x
compileH (AstWhile:xs) = compileLoop [] ++ compileH xs
compileH (x:xs) = compCommand x ++ compileH xs

compile commands = header ++ compileH commands ++ footer
