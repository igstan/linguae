#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>
#include <libkern/OSCacheControl.h>

//
// Grab disassembly using:
//
// ```bash
// cat <<CODE | gcc -xc -c -o- - | objdump --disassemble --no-leading-addr -
// int add() {
//   return 42;
// }
// CODE
// ```
//
static uint8_t code[] = {
  0x40, 0x05, 0x80, 0x52, // mov w0, #42
  0xc0, 0x03, 0x5f, 0xd6, // ret
};

int main(int argc, const char * argv[]) {
  // 1. Find out how much memory we need to allocate.
  const int size = sizeof(code);

  // 2. Allocate the needed memory and mark it executable.
  int map_flags = MAP_ANON | MAP_PRIVATE | MAP_JIT;
  void* mem = mmap(NULL, size, PROT_READ | PROT_WRITE | PROT_EXEC, map_flags, -1, 0);
  if (mem == MAP_FAILED || mem == NULL) {
    printf("Failed to allocate executable memory\n");
    return 1;
  }

  // 3. Enable WRITE access. This also disables EXEC access.
  pthread_jit_write_protect_np(false);

  // 4. Write the instructions.
  memcpy(mem, code, size);

  // 5. Disable WRITE access, which re-enables EXEC access.
  pthread_jit_write_protect_np(true);

  // 6. Make sure the instruction cache is coherent with the data cache.
  sys_icache_invalidate(&mem, size);

  // 7. Cast the memory region to a function pointer
  int (*jitted_function)(void) = (int (*)(void))mem;

  // 8. Call the function
  printf("JIT-ed code returned: %d\n", jitted_function());

  return 0;
}
