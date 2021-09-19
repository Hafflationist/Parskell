module Parskell.Synthesis.Register where

data FullRegister
    = Rax
    | Rbx
    | Rcx
    | Rdx
    | Rsi
    | Rdi
    | Rsp
    | Rbp
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    deriving Eq
instance Show FullRegister where
    show Rax = "rax"
    show Rbx = "rbx"
    show Rcx = "rcx"
    show Rdx = "rdx"
    show Rsi = "rsi"
    show Rdi = "rdi"
    show Rsp = "rsp"
    show Rbp = "rbp"
    show R8 = "r8"
    show R9 = "r9"
    show R10 = "r10"
    show R11 = "r11"
    show R12 = "r12"
    show R13 = "r13"
    show R14 = "r14"
    show R15 = "r15"
    