all: jar runner
	cp $(shell find target/ -name fpinscala.jar) .

runner:
	@printf "#!/bin/sh\njava -jar fpinscala.jar \$$@\n" > fpinscala
	chmod +x fpinscala

jar:
	sbt assembly

clean:
	rm -fr target/ project/project/ project/target/
	rm -fr fpinscala.jar fpinscala
