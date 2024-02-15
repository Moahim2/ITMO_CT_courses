SET ROOT=%~dp0..\..\
SET IMPLEMENTOR_PATH=%ROOT%\java-advanced\java-solutions\info\kgeorgiy\ja\levitskii\implementor\
SET TEST_PATH=%ROOT%java-advanced-2023\modules\info.kgeorgiy.java.advanced.implementor\^
info\kgeorgiy\java\advanced\implementor\

MD jarTempDir
javac -d jarTempDir --class-path %ROOT%java-advanced-2023\lib\junit-4.11.jar;^
%ROOT%java-advanced-2023\artifacts\info.kgeorgiy.java.advanced.implementor.jar\ %IMPLEMENTOR_PATH%Implementor.java

jar.exe --create --file Implementor.jar --manifest MANIFEST.MF -C jarTempDir .

rd jarTempDir /s /q
