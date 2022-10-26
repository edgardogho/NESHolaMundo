objs := HolaMundo.o
out := HolaMundo.nes

all: $(out)

clean:
	rm -f $(objs) $(out)

.PHONY: all clean

# Assemble

%.o: %.s
	ca65 $< -o $@ -l HolaMundo.lst

HolaMundo.o: HolaMundo.s

# Link

HolaMundo.nes: link.x $(objs)
	ld65 -C link.x $(objs) -o $@
