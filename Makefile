all: Bin1.hs Bin2.hs Bin3.hs

Bin%.hs: bin%.asm
	perl optimizer.pl $<

clean:
	rm -f *~ *.hi *.ho *.o hohmann meetgreet
