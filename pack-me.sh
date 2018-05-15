#!/bin/bash

# Packs files to a format specified by the course supervisor

rm igor_matuszewski.zip 2>/dev/null
rm -rdf igor_matuszewski

mkdir igor_matuszewski 2>/dev/null

cp -R app/ igor_matuszewski/
cp -R src/ igor_matuszewski/
cp -R syntax/ igor_matuszewski/

cp Setup.hs igor_matuszewski/

cp stack.yaml igor_matuszewski/
cp stretch-lang.cabal igor_matuszewski/
cp package.yaml igor_matuszewski/

cp README_pl.md igor_matuszewski/README
cp -R good/ igor_matuszewski/
cp -R bad/ igor_matuszewski/

# Generate a Makefile to be used on a `students` university machine
rm Makefile 2>/dev/null
echo "#!/bin/bash" >> Makefile
echo "CABAL=/home/students/inf/PUBLIC/MRJP/ghc-8.2.2/bin/cabal" >> Makefile
echo "build:" >> Makefile
echo -e "\t\$(CABAL) build" >> Makefile
echo -e "\tcp dist/build/stretchi/stretchi ./interpreter" >> Makefile
echo "" >> Makefile
echo "clean:" >> Makefile
echo -e "\trm ./interpreter" >> Makefile
cp Makefile igor_matuszewski/

zip -r igor_matuszewski.zip igor_matuszewski

rm -rd igor_matuszewski
rm Makefile
