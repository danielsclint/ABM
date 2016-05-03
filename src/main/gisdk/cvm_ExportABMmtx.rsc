//The 5 text files that the CVM Python code needs are:
// "Light_Mid": "impldt_MD_du.TXT"
// "Medium_Mid": "impmhdt_MD_DU.TXT"
// "Heavy_Mid": "imphhdt_MD_DU.TXT"
// "Time_Mid": "impldt_MD_Time.TXT"
// "Dist_Mid": "impldt_MD_Dist.TXT"

Macro "Export_ABM_Skims" //Use for skim Matrix with DU core
  shared outputDir

  matrices={"impmhdt_MD", "imphhdt_MD", "impldt_MD", "impldt_MD", "impldt_MD"}
  core2export={"DU_MD","DU_MD","DU_MD","*STM_MD (Skim)","Length (Skim)"}
  corenameout={"DU","DU","DU","Time","Dist"}

  for i=1 to matrices.length do
    filenameout=matrices[i]+"_"+corenameout[i]
    RunMacro("Export Matrix",outputDir,matrices[i],core2export[i],filenameout)
  end
  return(1)
EndMacro

Macro "Export Matrix" (path,filename,core2export,filenameout)
  filenamedel=filename+core2export+".txt"
  RunMacro("FileCheckDelete",path,filenamedel) 
  filenamedel=filename+core2export+".dcc"
  RunMacro("FileCheckDelete",path,filenamedel)
  filenamedel=filename+core2export+".csx"
  RunMacro("FileCheckDelete",path,filenamedel)
  RunMacro("Export Matrix to CSV",path,filename+".mtx",core2export,filenameout)  //check to see if you need to force double precision
EndMacro