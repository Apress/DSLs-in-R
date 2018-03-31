src_Makefiles=$(wildcard */Makefile)
built_files=$(src_Makefiles:/Makefile=/README.md)

all: $(built_files)

$(built_files): %/README.md : %/README.Rmd
	@echo building $(dir $@)
	make -C $(dir $@)

clean:
	-rm */*.md
