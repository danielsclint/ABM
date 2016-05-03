Macro "Test CVM Ext LD"
  Shared outputDir, path, mxzone, mxext
  path="T:\\devel\\CVM\\sr13\\2012_calib5"
  outputDir="T:\\devel\\CVM\\sr13\\2012_calib5\\output_cvm"
  mxzone=4996
  mxext=12
  RunMacro("CVM Ext LD")
EndMacro

Macro "CVM Ext LD"
  
  Shared outputDir, path, mxzone, mxext

  periods = {"_EA","_AM","_MD","_PM","_EV"}
  VehType = {"ExtLDN","ExtLDT"}

  ExtLDN_TOD_Factor = {0.12, 0.12, 0.12, 0.12, 0.12}
  ExtLDT_TOD_Factor = {0.12, 0.12, 0.12, 0.12, 0.12}

  // Loop through time periods
  for i = 1 to periods.length do

    //Copy trip_xx.mtx to backup
    di = GetDirectoryInfo(outputDir + "\\Trip"+periods[i]+"_bu.mtx", "File")
    if di.length > 0 then do
      RunMacro("SDcopyfile",{outputDir + "\\Trip"+periods[i]+".mtx",outputDir + "\\Trip"+periods[i]+"_bu.mtx"})
    end

    // Add Cores to Trip.mtx files and loop through vehicle types
    ABM = OpenMatrix(outputDir + "\\Trip"+periods[i]+".mtx", )
    for j = 1 to VehType.length do
      corename=VehType[j]
      AddMatrixCore(ABM, corename)
    end

    //create an array of internal zone ids
    iizones=mxzone-mxext
    dim zones[iizones]
    for k = 1 to zones.length do
       zones[k]=i2s(k+12)
    end

    //create matrix currencies
    ABM_mcs = CreateMatrixCurrencies(ABM, , , )

    //create external light duty cores
    ABM_mcs.ExtLDN := ABM_mcs.SOV_GP * ExtLDN_TOD_Factor[i]
    ABM_mcs.ExtLDT := ABM_mcs.SOV_PAY * ExtLDT_TOD_Factor[i]

    //set the internal-internal values to 0
    FillMatrix(ABM_mcs.ExtLDN, zones, zones, {"Copy", 0.0},)
    FillMatrix(ABM_mcs.ExtLDT, zones, zones, {"Copy", 0.0},)

    // Remove trips from person trip cores
    ABM_mcs.SOV_GP :=  ABM_mcs.SOV_GP - ABM_mcs.ExtLDN
    ABM_mcs.SOV_PAY :=  ABM_mcs.SOV_PAY - ABM_mcs.ExtLDT

  end
  RunMacro("close all" )
EndMacro