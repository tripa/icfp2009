all: Bin1.hs Bin2.hs Bin3.hs

Bin%.hs: bin%.asm
	perl optimizer.pl $<
