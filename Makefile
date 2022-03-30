all: build move

build: 
	cd src; \
	ghc --make Main -Wall -o flp21-fun \
	
	
move:
	mv src/flp21-fun flp21-fun 

testing:
	cd scripts; \
	./tester.sh

clean:
	rm test/*.out
	rm flp21-fun


