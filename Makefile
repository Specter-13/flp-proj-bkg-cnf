all: build move

build: 
	cd src; \
	ghc --make Main -Wall -o flp21-fun \
	
	
move:
	mv src/flp21-fun flp21-fun 

testing: build
	cd scripts; \
	./tester.sh

clean:
	rm test/*.out flp21-fun 
	cd src; \
	rm *.o *.hi Main

zip:
	zip -r flp-fun-xspavo00.zip src test scripts doc Makefile 

