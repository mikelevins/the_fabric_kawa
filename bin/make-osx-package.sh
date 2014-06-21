#!/bin/sh

PROJECT_DIR=/Users/mikel/Workshop/fabric
JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_05.jdk/Contents/Home

BUILD_DIR=/Users/mikel/Desktop/Fabric_build/macosx
PACKAGER=${JAVA_HOME}/bin/javafxpackager
PRODUCT=Fabric
JARFILE=Fabric.jar

${PACKAGER} -deploy -v -native -outdir ${BUILD_DIR} -outfile ${PRODUCT} -srcdir ${PROJECT_DIR} -srcfiles ${JARFILE} -appclass Fabric -name "The Fabric" -title "The Fabric"

