# Define compiler, linker, and assembler
CXX = g++
AS = as
CC = gcc

# Define flags for C++ and C compilation
CXXFLAGS = -std=c++17 -O2 -Wall
CFLAGS = -O2 -Wall

# Define the C++ compiler source and target
CPP_COMPILER_SRC = compiler.cpp
CPP_COMPILER_BIN = .build/compiler

# Define the assembly output file
ASM_OUTPUT = .build/main.s

# Define the C source file and object file
C_SRC = print_int.c
C_OBJ = .build/print_int.o

# Define the final binary
TARGET = program

# Create the .build directory
$(shell mkdir -p .build)

all: $(TARGET)

$(TARGET): $(ASM_OUTPUT) $(C_OBJ)
	$(CC) -o $@ .build/main.o $(C_OBJ)

$(C_OBJ): $(C_SRC)
	$(CC) $(CFLAGS) -c $< -o $@

$(ASM_OUTPUT): $(CPP_COMPILER_BIN)
	./$(CPP_COMPILER_BIN) > $@
	$(AS) -o .build/main.o $(ASM_OUTPUT)

$(CPP_COMPILER_BIN): $(CPP_COMPILER_SRC)
	$(CXX) $(CXXFLAGS) $< -o $@

clean:
	rm -rf .build $(TARGET)

.PHONY: all clean
