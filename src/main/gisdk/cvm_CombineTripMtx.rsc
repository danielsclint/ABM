/*********************************************************************************
Combine CVM Trip Matrix with ABM Trip Matrix
macro "Combine CVM and ABM Matrices" 

input files:
   Trip_EA.mtx: Early AM auto trip matrix file
   Trip_AM.mtx: AM Peak  auto trip matrix file 
   Trip_MD.mtx: Midday   auto trip matrix file 
   Trip_PM.mtx: PM Peak  auto trip matrix file 
   Trip_EV.mtx: Evening  auto trip matrix file 

each file has 14 cores:

 Name     Description
 -------  ---------------------------------------
 SOV_GP   Drive Alone Non-Toll
 SOV_PAY  Drive Alone Toll
 SR2_GP   Shared-ride 2 Person Non-HOV Non-Toll
 SR2_HOV  Shared-ride 2 Person HOV Non-Toll
 SR2_PAY  Shared-ride 2 Person HOV Toll Eligible
 SR3_GP   Shared-ride 3+ Person Non-HOV Non-Toll
 SR2_HOV  Shared-ride 3+ Person HOV Non-Toll
 SR2_PAY  Shared-ride 3+ Person HOV Toll Eligible
 lhdn     Light heavy-duty Truck Non-Toll
 mhdn     Medium heavy-duty Truck Non-Toll
 hhdnv    Heavy heavy-duty Truck Non-Toll
 lhdt     Light heavy-duty Truck Toll
 mhdt     Medium heavy-duty Truck Toll
 hhdt     Heavy heavy-duty Truck Toll

output files:

   Trip_EA.mtx: Early AM auto trip matrix file
   Trip_AM.mtx: AM Peak  auto trip matrix file 
   Trip_MD.mtx: Midday   auto trip matrix file 
   Trip_PM.mtx: PM Peak  auto trip matrix file 
   Trip_EV.mtx: Evening  auto trip matrix file

each file has 22 cores:

 Name     Description
 -------  ---------------------------------------
 SOV_GP   Drive Alone Non-Toll
 SOV_PAY  Drive Alone Toll
 SR2_GP   Shared-ride 2 Person Non-HOV Non-Toll
 SR2_HOV  Shared-ride 2 Person HOV Non-Toll
 SR2_PAY  Shared-ride 2 Person HOV Toll Eligible
 SR3_GP   Shared-ride 3+ Person Non-HOV Non-Toll
 SR2_HOV  Shared-ride 3+ Person HOV Non-Toll
 SR2_PAY  Shared-ride 3+ Person HOV Toll Eligible
 lhdn     External Light heavy-duty Truck Non-Toll
 mhdn     External Medium heavy-duty Truck Non-Toll
 hhdnv    External Heavy heavy-duty Truck Non-Toll
 lhdt     External Light heavy-duty Truck Toll
 mhdt     External Medium heavy-duty Truck Toll
 hhdt     External Heavy heavy-duty Truck Toll
 CVM:LT   Internal CVM Light heavy-duty Truck Toll
 CVM:MT   Internal CVM Medium heavy-duty Truck Toll
 CVM:HT   Internal CVM Heavy heavy-duty Truck Toll
 CVM:LN   Internal CVM Light heavy-duty Truck Non-Toll
 CVM:MN   Internal CVM Medium heavy-duty Truck Non-Toll
 CVM:HN   Internal CVM Heavy heavy-duty Truck Non-Toll
 ExtLDN   External Light-Duty Non-Toll
 ExtLDT   External Light-Duty Toll

 RCU 2013-02-22
*************************************************************************************/

Macro "Test Combine CVM and ABM Matrices"
  Shared outputDir, path, mxzone, mxext, scenarioYear
  path="T:\\devel\\abmctm\\2012_aztec_13_2_5"
  outputDir="T:\\devel\\abmctm\\2012_aztec_13_2_5\\output"
  properties = "\\conf\\sandag_abm.properties"
  mxzone=4996
  mxext=12
  scenarioYear="2012"
  RunMacro("Combine CVM and ABM Matrices",properties,1)
EndMacro

Macro "Combine CVM and ABM Matrices" (properties,iteration)
  
  Shared outputDir, path, mxzone, mxext, scenarioYear

  periods = {"_EA", "_AM", "_MD", "_PM", "_EV"}
  VehType = {"LT", "MT", "HT", "LN", "MN", "HN"}
  ExtVehType = {"LHD", "MHD", "HHD"}

  // Read CVM Scale Factor
  cvmScaleFactorStr= RunMacro("read properties",properties,"cvm.scaleFactor","S")
  // Lists iteration with delimiter ","
  cvmScaleFactorArr = ParseString(cvmScaleFactorStr, ",")
  cvmScaleFactor = StringToReal(cvmScaleFactorArr[iteration])

  CVM = OpenMatrix(outputDir + "\\CVM_Trips.mtx", )
  CVM_mcs = CreateMatrixCurrencies(CVM, , , )

  ExtTrk = OpenMatrix(path + "\\input_truck\\ExtTruckTrips_" + scenarioYear + ".mtx", )
  ExtTrk_mcs = CreateMatrixCurrencies(ExtTrk, , , )

  // Loop through time periods
  for i = 1 to periods.length do

    // Add Cores to Trip.mtx files and loop through vehicle types
    ABM = OpenMatrix(outputDir + "\\Trip"+periods[i]+".mtx", )
    for j = 1 to VehType.length do
      corename="CVM"+":"+VehType[j]
      AddMatrixCore(ABM, corename)
    end

    //Create EI, IE, EE from HDTM trip cores
    //create an array of internal zone ids
//    iizones=mxzone-mxext
//    dim zones[iizones]
//    for k = 1 to zones.length do
//       zones[k]=i2s(k+12)
//    end
    //create an array of zone ids
    dim zones[mxzone]
    for k = 1 to zones.length do
       zones[k]=i2s(k)
    end

    //create matrix currencies
    ABM_mcs = CreateMatrixCurrencies(ABM, , , )

    //set the internal-internal values to 0
    FillMatrix(ABM_mcs.lhdt, zones, zones, {"Copy", 0.0},)
    FillMatrix(ABM_mcs.mhdt, zones, zones, {"Copy", 0.0},)
    FillMatrix(ABM_mcs.hhdt, zones, zones, {"Copy", 0.0},)
//    FillMatrix(ABM_mcs.lhdn, zones, zones, {"Copy", 0.0},)
//    FillMatrix(ABM_mcs.mhdn, zones, zones, {"Copy", 0.0},)
//    FillMatrix(ABM_mcs.hhdn, zones, zones, {"Copy", 0.0},)

    // 
    ABM_mcs.("lhdn") :=  ExtTrk_mcs.(ExtVehType[1]+periods[i])
    ABM_mcs.("mhdn") :=  ExtTrk_mcs.(ExtVehType[2]+periods[i])
    ABM_mcs.("hhdn") :=  ExtTrk_mcs.(ExtVehType[3]+periods[i])

    // Merge and transform cores from CVM_Trips.mtx to LHD, MHD, HHD format with Toll/Non-toll, scale trips, and add external trucks
//    ABM_mcs.("CVM:LN") :=  (CVM_mcs.("CVM"+periods[i]+":LNT") / cvmScaleFactor) + ABM_mcs.lhdn
//    ABM_mcs.("CVM:MN") :=  (CVM_mcs.("CVM"+periods[i]+":INT") / cvmScaleFactor) + (CVM_mcs.("CVM"+periods[i]+":MNT") / cvmScaleFactor) + ABM_mcs.mhdn
//    ABM_mcs.("CVM:HN") :=  (CVM_mcs.("CVM"+periods[i]+":HNT") / cvmScaleFactor) + ABM_mcs.hhdn
//    ABM_mcs.("CVM:LT") :=  (CVM_mcs.("CVM"+periods[i]+":LT") / cvmScaleFactor) + ABM_mcs.lhdt
//    ABM_mcs.("CVM:MT") :=  (CVM_mcs.("CVM"+periods[i]+":IT") / cvmScaleFactor) + (CVM_mcs.("CVM"+periods[i]+":MT") / cvmScaleFactor) + ABM_mcs.mhdt
//    ABM_mcs.("CVM:HT") :=  (CVM_mcs.("CVM"+periods[i]+":HT") / cvmScaleFactor) + ABM_mcs.hhdt

    // Transform CVM_Trips.mtx to LHD, MHD, HHD format with Toll/Non-toll, scale trips, keep external trucks separate
    ABM_mcs.("CVM:LN") :=  (CVM_mcs.("CVM"+periods[i]+":LNT") / cvmScaleFactor)
    ABM_mcs.("CVM:MN") :=  (CVM_mcs.("CVM"+periods[i]+":INT") / cvmScaleFactor) + (CVM_mcs.("CVM"+periods[i]+":MNT") / cvmScaleFactor)
    ABM_mcs.("CVM:HN") :=  (CVM_mcs.("CVM"+periods[i]+":HNT") / cvmScaleFactor)
    ABM_mcs.("CVM:LT") :=  (CVM_mcs.("CVM"+periods[i]+":LT") / cvmScaleFactor)
    ABM_mcs.("CVM:MT") :=  (CVM_mcs.("CVM"+periods[i]+":IT") / cvmScaleFactor) + (CVM_mcs.("CVM"+periods[i]+":MT") / cvmScaleFactor)
    ABM_mcs.("CVM:HT") :=  (CVM_mcs.("CVM"+periods[i]+":HT") / cvmScaleFactor)

    //drop external truck matrix
//    DropMatrixCore(ABM, "lhdt")
//    DropMatrixCore(ABM, "mhdt")
//    DropMatrixCore(ABM, "hhdt")
//    DropMatrixCore(ABM, "lhdn")
//    DropMatrixCore(ABM, "mhdn")
//    DropMatrixCore(ABM, "hhdn")

  end
  RunMacro("close all" )
EndMacro