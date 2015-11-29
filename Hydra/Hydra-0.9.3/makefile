#-----------------------------------------------------------------------
# Hydra: Functional Digital Circuit Description Language
# Package HydraLib
# John O'Donnell

#-----------------------------------------------------------------------
# Fixed locations that need to be edited manually

# Directory where snapshots will be saved
SnapshotDestination := ./archive

# make doc will print the location of index of the html documentation.
# To view the documentation in browser, put this string in front of the
# location of the index file
#   If using cygwin on Windows:  "file:///C:/cygwin"
#   If using Windows command window:   "file:///C:/Users/yourusername"

#-----------------------------------------------------------------------
# Calculated locations, don't need to edit

PackagePath := $(shell pwd)
PackageName := $(shell basename $(PackagePath))

#-----------------------------------------------------------------------

.PHONY : build
build :
	cabal install
	cabal haddock
	make doc

#-----------------------------------------------------------------------
.PHONY : doc
doc :
	mkdir -p doc/html
	pandoc --standalone --table-of-contents \
	   --template=doc/src/topTemplate \
	   -o doc/html/index.html doc/src/index.txt
	cp doc/src/style.css doc/html
	echo "Location of documentation: "
	echo "$(PackagePath)/doc/html/index.html"

#-----------------------------------------------------------------------
# make clean -- delete temporary and object files

.PHONY : clean
clean :
# First use cabal to delete its temporary files
	runhaskell Setup clean
# Delete object and backup files
	find . \( -name '*~' -o -name '*.hi' -o \
	          -name '*.bak' -o -name '*.o' \) \
	       -delete
# Delete documentation generated from src
	rm -rf doc/html

#-----------------------------------------------------------------------
# make snapshot -- snapshot a tarball in $snapshotDestination

DateTimeStamp := $(shell date +%Y-%m-%d-at-%H-%M)
SnapshotTarName := $(PackageName)-on-$(DateTimeStamp).tgz

.PHONY : snapshot
snapshot :
	echo PackagePath = $(PackagePath)
	echo PackageName = $(PackageName)
	echo DateTimeStamp = $(DateTimeStamp)
	echo snapshotTarName = $(SnapshotTarName)
	cd .. ; \
	tar -czf $(SnapshotTarName) $(PackageName) ; \
	mv $(SnapshotTarName) $(SnapshotDestination)

.PHONY : release
release :
	make clean
	cd .. ; \
	tar -czf $(PackageName).tgz  $(PackageName) ; \
	zip -o -r $(PackageName).zip $(PackageName) ;
