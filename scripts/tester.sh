#!/bin/bash

inputFiles="test/*.in"
refFiles="test/ref/*.ref"
# outputDirectory= "../test/output";

cd ..
#build project
make

passed=0
failed=0
#output
for filename in $inputFiles 
do
    refFile="test/ref/$(basename $filename .in).ref"
    outName="out"
    outputFilePath="test/$(basename $filename .in).out"
    
    echo "Testing $filename ..."
    ./flp21-fun -2 $filename > $outputFilePath
    
    diff $outputFilePath $refFile

    if [ $? == '0' ] 
    then
        passed=$(($passed+1))
    else
        failed=$(($failed+1))
    fi

done
echo "-----------------------------------------"
echo "Passed tests: $passed"
echo "Failed tests: $failed"
echo "-----------------------------------------"