set PROJECT_DRIVE=%1
set PROJECT_DIRECTORY=%2
set PROJECT_DIRECTORY_FWD=%3
set CVM_ScaleFactor=%4

%PROJECT_DRIVE%
set "SCEN_DIR=%PROJECT_DRIVE%%PROJECT_DIRECTORY%"
set "SCEN_DIR_FWD=%PROJECT_DRIVE%%PROJECT_DIRECTORY_FWD%"
set "CVM_DIR=%SCEN_DIR%\CVM"
cd %CVM_DIR%

set OLDPATH=%PATH%
PATH=C:\Program Files\TransCAD 6.0r2;C:\Program Files\Common Files\Microsoft Shared\Windows Live;C:\Program Files (x86)\Common Files\Microsoft Shared\Windows Live;C:\Windows\system32;C:\Windows;C:\Windows\System32\Wbem;c:\Program Files (x86)\ATI Technologies\ATI.ACE\Core-Static;C:\Program Files (x86)\NTRU Cryptosystems\NTRU TCG Software Stack\bin\;C:\Program Files\NTRU Cryptosystems\NTRU TCG Software Stack\bin\;C:\Program Files\Wave Systems Corp\Gemalto\Access Client\v5\;C:\Program Files\TransCAD60;C:\Program Files (x86)\Windows Live\Shared;C:\Windows\System32\WindowsPowerShell\v1.0\;C:\Windows\System32\WindowsPowerShell\v1.0\;c:\Anaconda2\

REM c:\python27\python.exe "SDCVM.py" -s 10.0
python %SCEN_DIR%\python\sdcvm.py -s %CVM_ScaleFactor% -p %SCEN_DIR%

REM for Aggie "c:\Program Files\Java\jre7\bin\java.exe" -Xmx24000m -Xmn16000M -Dlog4j.configuration=file:Inputs/log4j.xml -DSCENDIR=./ -cp Code/sandagcvm_r74.jar;Code/common-base_r3391.jar;Code/log4j-1.2.9.jar;"C:/Program Files/TransCAD 6.0r2/GISDK/Matrices/TranscadMatrix.jar" com.hbaspecto.activityTravel.cvm.GenerateCommercialTours "Inputs/cvm.properties"
REM   32 bit java 	         					logging configuration         directory		cvm code         matrixetccode             loggingcode                                        transcadjavaapi							name of the program		                  properties
"c:\Program Files\Java\jre7\bin\java.exe" -Xmx24000m -Xmn16000M -Dlog4j.configuration=file:Inputs/log4j.xml -DSCENDIR=%SCEN_DIR_FWD% -cp Code/sandagcvm_r74.jar;Code/common-base_r3391.jar;Code/log4j-1.2.9.jar;Code/TranscadMatrix.jar com.hbaspecto.activityTravel.cvm.GenerateCommercialTours "Inputs/cvm.properties"

REM c:\python27\python.exe "SDCVM postprocess OD.py"
python %SCEN_DIR%\python\sdcvm_summarize.py -p %SCEN_DIR%

set PATH=%OLDPATH%