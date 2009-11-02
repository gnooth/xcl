#include "lisp.hpp"
#include "Frame.hpp"
#include "UnwindProtect.hpp"

void UnwindProtect::run_cleanup_code()
{
//   printf("entering run_cleanup_code()\n");
//   fflush(stdout);
  __asm__ __volatile__("push %%rbp\n\t"
                       "push %%r12\n\t"
                       "movq %0,%%rbp\n\t"
                       "call *%1\n\t"
                       "pop %%r12\n\t"
                       "pop %%rbp\n\t"
                         : // no output registers
                         : "r"(_rbp), "r"(_code) // input
                         : "rax","rbx","rcx","rdx",
                       "rsi","rdi","r8","r9",
                       "r13","r14","r15",
                       "memory" // clobber list
                       );
//   printf("leaving run_cleanup_code()\n");
//   fflush(stdout);
}

