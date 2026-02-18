# Makefile for Adder Compiler
# Replace 'elf64' with 'macho64' on macOS

# Pattern rule to compile .snek files to .s assembly files
test/%.s: test/%.snek src/main.rs
	cargo run -- $< test/$*.s

# Pattern rule to assemble .s files and link into executables
# Change -f elf64 to -f macho64 on macOS
test/%.run: test/%.s runtime/start.rs
	nasm -f elf64 test/$*.s -o runtime/our_code.o
	ar rcs runtime/libour_code.a runtime/our_code.o
	rustc -L runtime/ runtime/start.rs -o test/$*.run

# Clean build artifacts
clean:
	cargo clean
	rm -f test/*.s test/*.run runtime/*.o runtime/*.a

# Run all tests
test: test/37.run test/add.run test/add1.run test/negate.run test/negate_nested.run test/complex.run test/sub1.run test/mix.run test/zero.run test/big.run test/num.run test/example.run
	@echo "Running tests..."
	@./test/37.run
	@./test/add.run
	@./test/add1.run
	@./test/negate.run
	@./test/negate_nested.run
	@./test/complex.run
	@./test/sub1.run
	@./test/mix.run
	@./test/zero.run
	@./test/big.run
	@./test/num.run
	@./test/example.run

.PHONY: clean test
