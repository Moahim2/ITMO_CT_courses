SET ROOT=%~dp0..\..\
SET IMPLEMENTOR_PATH=%ROOT%\java-advanced\java-solutions\info\kgeorgiy\ja\levitskii\implementor\
rem SET TEST_PATH=%ROOT%java-advanced-2023\modules\info.kgeorgiy.java.advanced.implementor\info\kgeorgiy\java\advanced\implementor\
rem javac -d TempDir %TEST_PATH%\JarImpler.java %TEST_PATH%\Impler.java %TEST_PATH%\ImplerException.java
rem javadoc --class-path TempDir\ %IMPLEMENTOR_PATH%Implementor.java -private -d %ROOT%\java-advanced\javadoc

rem rd TempDir /s /q

javadoc --class-path %ROOT%java-advanced-2023\lib\junit-4.11.jar;%ROOT%java-advanced-2023\artifacts\^
info.kgeorgiy.java.advanced.implementor.jar\ %IMPLEMENTOR_PATH%Implementor.java -private -d %ROOT%\java-advanced\javadoc

