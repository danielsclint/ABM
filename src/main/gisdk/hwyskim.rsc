/***********************************************
Hwy skim all

This macro calls macro "Update highway network", which updates highway network with times
from last highway assignment, and then skims highway network by calling macro "hwy skim" for
the following modes:

dant  Drive-alone non-toll
dat   Drive-alone toll
s2nh  Shared-2 non-toll HOV
s2th  Shared-2 toll HOV
s3nh  Shared-3 non-toll HOV
s3th  Shared-3 toll HOV
truck Truck

***********************************************/
Macro "Hwy skim all" 

   shared path, inputDir, outputDir, mxzone

   ok=RunMacro("Update highway network",)
   if !ok then goto quit
      
   ok=RunMacro("hwy skim",{"dant"}) 
   if !ok then goto quit

   ok=RunMacro("hwy skim",{"dat"}) 
   if !ok then goto quit

   ok=RunMacro("hwy skim",{"s2nh"}) 
   if !ok then goto quit

   ok=RunMacro("hwy skim",{"s2th"}) 
   if !ok then goto quit

   ok=RunMacro("hwy skim",{"s3nh"})
   if !ok then goto quit

   ok=RunMacro("hwy skim",{"s3th"}) 
   if !ok then goto quit

   ok=RunMacro("hwy skim",{"hhdn"}) 
   if !ok then goto quit

   ok=RunMacro("hwy skim",{"hhdt"}) 
   if !ok then goto quit

   ok=RunMacro("hwy skim",{"truck"}) 
   if !ok then goto quit


   return(1)
   
   quit:
      return(0)
  
EndMacro

/********************************************************************************

Update highway network

Updates highway line layer and network with fields from latest assignment flow tables.  

The following fields are updated on the line layer:

Field       Description
-------     ---------------
STM         SOV time
HTM         HOV time
SCST        SOV generalized cost
H2CST       Shared-2 generalized cost
H3CST       Shared-3 generalized cost

Each field is xxField_yy where:

 xx is AB or BA indicating direction
 yy is period, as follows:
   EA: Early AM
   AM: AM peak
   MD: Midday
   PM: PM peak
   EV: Evening
   
Inputs:
   input\hwy.dbd           Highway line layer
   input\hwy.net           Highway network
   output\hwyload_yy.bin   Loaded flow table from assignment, one per period (yy)

Outputs:
   output\hwy.dbd          Updated highway line layer
   output\hwy.net          Updated highway network

********************************************************************************/
Macro "Update highway network"

   shared path, inputDir, outputDir
   
/* WSU commented out; not used in script
   VOT={67,67,67,67,67,67,67,67,67,68,89,67,68,89}
*/

   // input files
   db_file = outputDir + "\\hwy.dbd"
   net_file = outputDir + "\\hwy.net" 

   {node_lyr, link_lyr} = RunMacro("TCB Add DB Layers", db_file)
   db_node_lyr = db_file + "|" + node_lyr

   periods = {"_EA", "_AM", "_MD", "_PM", "_EV"}
   da_vot=67.00 // $0.67 cents per minute VOT ($40.2 per hour)
   s2_vot=67.00
   s3_vot=67.00
   lhd_VOT=50.00
   mhd_VOT=51.00
   hhd_VOT=72.00
 
   da_vot=da_vot*60/100   //Convert to dollars per hour VOT so don't have to change gen cost function below
   s2_vot=s2_vot*60/100
   s3_vot=s3_vot*60/100
   
   //Recompute generalized cost using MSA cost in flow table,
   for i = 1 to periods.length do
    
      flowTable = outputDir+"\\hwyload"+periods[i]+".bin"
        
      // The Dataview Set is a joined view of the link layer and the flow table, based on link ID
      Opts.Input.[Dataview Set] = {{db_file+"|"+link_lyr, flowTable, {"ID"}, {"ID1"}},"sovtime"+periods[i] }   
      Opts.Global.Fields = {"ABSTM"+periods[i],"BASTM"+periods[i]}                                  // the field to fill
      Opts.Global.Method = "Formula"                                                                  // the fill method
      Opts.Global.Parameter = {"AB_MSA_Cost"  ,
                               "BA_MSA_Cost"  }   
      ret_value = RunMacro("TCB Run Operation", "Fill Dataview", Opts, &Ret)
      if !ret_value then goto quit
         
       // The Dataview Set is a joined view of the link layer and the flow table, based on link ID
      Opts.Input.[Dataview Set] = {{db_file+"|"+link_lyr, flowTable, {"ID"}, {"ID1"}},"hovtime"+periods[i] }   
      Opts.Global.Fields = {"ABHTM"+periods[i],"BAHTM"+periods[i]}                                // the field to fill
      Opts.Global.Method = "Formula"                                                                  // the fill method
      Opts.Global.Parameter = {"AB_MSA_Cost"  ,
                               "BA_MSA_Cost"  }   
      ret_value = RunMacro("TCB Run Operation", "Fill Dataview", Opts, &Ret)
      if !ret_value then goto quit
      
      // The Dataview Set is a joined view of the link layer and the flow table, based on link ID
      Opts.Input.[Dataview Set] = {{db_file+"|"+link_lyr, flowTable, {"ID"}, {"ID1"}},"dajoin"+periods[i] }   
      Opts.Global.Fields = {"ABSCST"+periods[i],"BASCST"+periods[i]}                                  // the field to fill
      Opts.Global.Method = "Formula"                                                                  // the fill method
      Opts.Global.Parameter = {"AB_MSA_Cost + ((ITOLL3"+periods[i]+"/100)/"+String(da_vot)+"*60)"  ,
                               "BA_MSA_Cost + ((ITOLL3"+periods[i]+"/100)/"+String(da_vot)+"*60)"  }   
      ret_value = RunMacro("TCB Run Operation", "Fill Dataview", Opts, &Ret)
      if !ret_value then goto quit
         
       // The Dataview Set is a joined view of the link layer and the flow table, based on link ID
      Opts.Input.[Dataview Set] = {{db_file+"|"+link_lyr, flowTable, {"ID"}, {"ID1"}},"s2join"+periods[i] }   
      Opts.Global.Fields = {"ABH2CST"+periods[i],"BAH2CST"+periods[i]}                                // the field to fill
      Opts.Global.Method = "Formula"                                                                  // the fill method
      Opts.Global.Parameter = {"if (IHOV=3 or IHOV=4) then (AB_MSA_Cost + ((ITOLL3"+periods[i]+"/100)/"+String(s2_vot)+"*60)) else AB_MSA_Cost"  ,
                               "if (IHOV=3 or IHOV=4) then (BA_MSA_Cost + ((ITOLL3"+periods[i]+"/100)/"+String(s2_vot)+"*60)) else BA_MSA_Cost"  }   
      ret_value = RunMacro("TCB Run Operation", "Fill Dataview", Opts, &Ret)
      if !ret_value then goto quit
      
       // The Dataview Set is a joined view of the link layer and the flow table, based on link ID
      Opts.Input.[Dataview Set] = {{db_file+"|"+link_lyr, flowTable, {"ID"}, {"ID1"}},"s3join"+periods[i] }   
      Opts.Global.Fields = {"ABH3CST"+periods[i],"BAH3CST"+periods[i]}                                // the field to fill
      Opts.Global.Method = "Formula"                                                                  // the fill method
      Opts.Global.Parameter = {"if IHOV=4 then (AB_MSA_Cost + ((ITOLL3"+periods[i]+"/100)/"+String(s3_vot)+"*60)) else AB_MSA_Cost"  ,
                               "if IHOV=4 then (BA_MSA_Cost + ((ITOLL3"+periods[i]+"/100)/"+String(s3_vot)+"*60)) else BA_MSA_Cost"  }   
      ret_value = RunMacro("TCB Run Operation", "Fill Dataview", Opts, &Ret)
      if !ret_value then goto quit
       
      Opts = null
      Opts.Input.Database = db_file
      Opts.Input.Network = net_file
      Opts.Input.[Link Set] = {db_file+"|"+link_lyr, link_lyr}
      Opts.Global.[Fields Indices] = "*STM"+periods[i]
      Opts.Global.Options.[Link Fields] = { {link_lyr+".ABSTM"+periods[i],link_lyr+".BASTM"+periods[i] } }
      Opts.Global.Options.Constants = {1}
      ret_value = RunMacro("TCB Run Operation",  "Update Network Field", Opts) 
      if !ret_value then goto quit
 

      Opts = null
      Opts.Input.Database = db_file
      Opts.Input.Network = net_file
      Opts.Input.[Link Set] = {db_file+"|"+link_lyr, link_lyr}
      Opts.Global.[Fields Indices] = "*HTM"+periods[i]
      Opts.Global.Options.[Link Fields] = { {link_lyr+".ABHTM"+periods[i],link_lyr+".BAHTM"+periods[i] } }
      Opts.Global.Options.Constants = {1}
      ret_value = RunMacro("TCB Run Operation",  "Update Network Field", Opts) 
      if !ret_value then goto quit
       
      Opts = null
      Opts.Input.Database = db_file
      Opts.Input.Network = net_file
      Opts.Input.[Link Set] = {db_file+"|"+link_lyr, link_lyr}
      Opts.Global.[Fields Indices] = "*SCST"+periods[i]
      Opts.Global.Options.[Link Fields] = { {link_lyr+".ABSCST"+periods[i],link_lyr+".BASCST"+periods[i] } }
      Opts.Global.Options.Constants = {1}
      ret_value = RunMacro("TCB Run Operation",  "Update Network Field", Opts) 
      if !ret_value then goto quit
 

      Opts = null
      Opts.Input.Database = db_file
      Opts.Input.Network = net_file
      Opts.Input.[Link Set] = {db_file+"|"+link_lyr, link_lyr}
      Opts.Global.[Fields Indices] = "*H2CST"+periods[i]
      Opts.Global.Options.[Link Fields] = { {link_lyr+".ABH2CST"+periods[i],link_lyr+".BAH2CST"+periods[i] } }
      Opts.Global.Options.Constants = {1}
      ret_value = RunMacro("TCB Run Operation",  "Update Network Field", Opts) 
      if !ret_value then goto quit
         

      Opts = null
      Opts.Input.Database = db_file
      Opts.Input.Network = net_file
      Opts.Input.[Link Set] = {db_file+"|"+link_lyr, link_lyr}
      Opts.Global.[Fields Indices] = "*H3CST"+periods[i]
      Opts.Global.Options.[Link Fields] = { {link_lyr+".ABH3CST"+periods[i],link_lyr+".BAH3CST"+periods[i] } }
      Opts.Global.Options.Constants = {1}
      ret_value = RunMacro("TCB Run Operation",  "Update Network Field", Opts) 
      if !ret_value then goto quit                
       
       
    end   
   
 
   ok=1
   runmacro("close all")
   quit:   

   return(ok)   
   
EndMacro
   
/*

hwy skim

Skim highway network for the following modes (passed as argument)


Mode     Description                  Cost attribute
----     ---------------------        --------------
truck    Truck                        SCST
lhdn     Light-heavy-duty non-toll    LHDCST
mhdn     Medium-heavy-duty non-toll   MHDCST
hhdn     Heavy-heavy-duty non-toll    HHDCST
lhdt     Light-heavy-duty toll        LHDCST
mhdt     Medium-heavy-duty toll       MHDCST
hhdt     Heavy-heavy-duty toll        HHDCST
dant     Drive-alone non-toll         SCST
dat      Drive-alone toll             SCST
s2nh     Shared-2 non-toll HOV        H2CST
s2th     Shared-2 toll non-HOV        H2CST
s3nh     Shared-3 non-toll HOV        H3CST
s3th     Shared-3 toll HOV            H3CST

Note:  dant skims also apply to shared-2 non-toll, non-HOV and shared 3+ non-toll, non-HOV

v1.0  jef 3/30/2012

*/
Macro "hwy skim" (arr)

   shared path, inputDir, outputDir, mxzone
   mode=arr[1]
   dim skimbyset1[3],skimbyset2[3]

/*WSU commented out; not used in script.
   VOT={50,50,50,50,50,50,50,50,50,51,72,50,51,72}
*/

   // input files
   db_file = outputDir + "\\hwy.dbd"
   net_file = outputDir + "\\hwy.net" 

   {node_lyr, link_lyr} = RunMacro("TCB Add DB Layers", db_file)
   db_node_lyr = db_file + "|" + node_lyr
   net = ReadNetwork(net_file)

   periods = {"_EA", "_AM", "_MD", "_PM", "_EV"}

   for i = 1 to periods.length do


      if mode = "truck" then do

         CostFld =  "*SCST"+periods[i]                               // minimizing cost field
         SkimVar1 = "*STM" +periods[i]                               // first skim varaible (in addition to LENGTH)
         SkimVar2 = null
         SkimVar3 = null
         skimbyset1 = null                                           // second skim varaible (in addition to LENGTH)
         skimbyset2 = null                                           // third skim variable
         
         excl_qry =  "!((ihov=1|ihov=4|ifc>7)&(ITRUCK=1|ITRUCK>4))"  // query for exclusion link set

         set = "TrkToll"+periods[i]
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where (ihov=4 and (ITRUCK=1|ITRUCK>4))",)
         if n > 0 then skimbyset1={vw_set, {"itoll"+periods[i],"itoll2"+periods[i]}}
         
         skimmat = "imptrk"+periods[i]+".mtx"                        // output skim matrices

      end

      else if mode = "ldn" then do

         CostFld =  "*LDCST"+periods[i]                             // minimizing cost field
         SkimVar1 = "*STM"+periods[i]                              // first skim varaible (in addition to LENGTH)
         SkimVar2 = null
         SkimVar3 = null
         skimbyset1 = null
         skimbyset2 = null
   
         excl_qry = "!(ihov=1&ITRUCK<5)"                           // query for exclusion link set
   
         skimmat =  "impldn"+periods[i]+".mtx"                     // output skim matrices
      end
      else if mode = "ldt" then do
         
         CostFld = "*LDCST"+periods[i]                                // minimizing cost field
         SkimVar1 = "*STM"+periods[i]                                // first skim varaible (in addition to LENGTH)
         SkimVar2 = "ITOLL2"+periods[i]
         SkimVar3 = null
         skimbyset1 = null
         skimbyset2 = null

         excl_qry = "!(((ihov=1|ihov=4|((ihov=2|ihov=3)&(itoll"+periods[i]+">0&abln"+periods[i]+"<9)))|ifc>7)&ITRUCK<5)"
         set =  mode
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several","Select * where !(((ihov=1|ihov=4|((ihov=2|ihov=3)&(itoll"+periods[i]+">0&abln"+periods[i]+"<9)))|ifc>7)&ITRUCK<5)",)
         if n = 0 then excl_qry=null   //reset value if no selection records
   
         // skimbyset1 = length on toll lanes
         set = "ldtdst"+periods[i]
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where ((ihov=4|((ihov=2|ihov=3)&(itoll"+periods[i]+">0&abln"+periods[i]+"<9)))&ITRUCK<5)",)
         if n > 0 then skimbyset1={vw_set, {"Length"}}
      
         skimmat = "impldt"+periods[i]+".mtx"
      end
      else if mode = "lhdn" then do                                  // light duty truck non-toll 
                                                                                    
         CostFld =  "*LHDCST"+periods[i]                               // minimizing cost field 
         SkimVar1 = "*STM" +periods[i]                               // first skim varaible (in addition to LENGTH)
         SkimVar2 = null
         SkimVar3 = null
         skimbyset1 = null                                                           
         skimbyset2 = null                                                           
         
         excl_qry =  "!((ihov=1|ifc>7)&(ITRUCK<4|ITRUCK=7))"             // query for lhd non-toll exclusion link set
         
         skimmat = "imp"+mode+periods[i]+".mtx"                      // output skim matrices        
      end    
      else if mode = "mhdn" then do                                  // medium duty truck non-toll 
                                                                                    
         CostFld =  "*MHDCST"+periods[i]                               // minimizing cost field 
         SkimVar1 = "*STM" +periods[i]                               // first skim varaible (in addition to LENGTH)
         SkimVar2 = null
         SkimVar3 = null
         skimbyset1 = null                                                           
         skimbyset2 = null                                                           
         
         excl_qry =  "!((ihov=1|ifc>7)&(ITRUCK<3|ITRUCK>5))"             // query for mhd non-toll exclusion link set
         
         skimmat = "imp"+mode+periods[i]+".mtx"                      // output skim matrices        
      end   
      else if mode = "hhdn" then do                                  // heavy duty truck non-toll 
                                                                                    
         CostFld =  "*HHDCST"+periods[i]                               // minimizing cost field 
         SkimVar1 = "*STM" +periods[i]                               // first skim varaible (in addition to LENGTH)
         SkimVar2 = null
         SkimVar3 = null
         skimbyset1 = null                                                           
         skimbyset2 = null                                                           
         
         excl_qry =  "!((ihov=1|ifc>7)&(ITRUCK=1|ITRUCK>4))"             // query for hhd non-toll exclusion link set
         
         skimmat = "imp"+mode+periods[i]+".mtx"                      // output skim matrices        
      end 
      else if mode = "lhdt" then do                                  // light duty truck toll
                                                                                  
         CostFld =  "*LHDCST"+periods[i]                               // minimizing cost field 
         SkimVar1 = "*STM" +periods[i]                               // first skim varaible (in addition to LENGTH)
         SkimVar2 = "ITOLL2" + periods[i] 
         SkimVar3 = null                                                                                       
         skimbyset1 = null                                           // second skim varaible
         skimbyset2 = null                                           // third skim variable
                  
         excl_qry = "!(((ihov=1|ihov=4|((ihov=2|ihov=3)&(itoll"+periods[i]+">0&abln"+periods[i]+"<9)))|ifc>7) & (ITRUCK<4|ITRUCK=7))" // query for lhd toll exclusion link set
        
         set =  mode
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where !(((ihov=1|ihov=4|((ihov=2|ihov=3)&(itoll"+periods[i]+">0&abln"+periods[i]+"<9)))|ifc>7)&(ITRUCK<4|ITRUCK=7))",)
         if n = 0 then excl_qry=null                                                    // reset value if no selection records 

         // skimbyset1 = length on toll lanes
         set = "tolldst"+periods[i]
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where ((ihov=4|((ihov=2|ihov=3)&(itoll"+periods[i]+">0&abln"+periods[i]+"<9)))&(ITRUCK<4|ITRUCK=7))",)   // for all links
         if n > 0 then skimbyset1={vw_set, {"Length"}}
  
         skimmat = "imp"+mode+periods[i]+".mtx"                    // output skim matrices        
      end  
      else if mode = "mhdt" then do                                  // medium duty truck toll
                                                                                  
         CostFld =  "*MHDCST"+periods[i]                               // minimizing cost field 
         SkimVar1 = "*STM" +periods[i]                               // first skim varaible (in addition to LENGTH)
         SkimVar2 = "ITOLL2" + periods[i] 
         SkimVar3 = null                                                                                       
         skimbyset1 = null                                           // second skim varaible
         skimbyset2 = null                                           // third skim variable
                
         excl_qry = "!(((ihov=1|ihov=4|((ihov=2|ihov=3)&(itoll"+periods[i]+">0&abln"+periods[i]+"<9)))|ifc>7)&(ITRUCK<3|ITRUCK>5))" // query for mhd toll exclusion link set
          
         set =  mode
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where !(((ihov=1|ihov=4|((ihov=2|ihov=3)&(itoll"+periods[i]+">0&abln"+periods[i]+"<9)))|ifc>7)&(ITRUCK<3|ITRUCK>5))",)
         if n = 0 then excl_qry=null                                                    // reset value if no selection records 
         
         // skimbyset1 = length on toll lanes
         set = "tolldst"+periods[i]
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where ((ihov=4|((ihov=2|ihov=3)&(itoll"+periods[i]+">0&abln"+periods[i]+"<9)))&(ITRUCK<3|ITRUCK>5))",)   // for all links
         if n > 0 then skimbyset1={vw_set, {"Length"}}
  
         skimmat = "imp"+mode+periods[i]+".mtx"                    // output skim matrices        
      end  

      else if mode = "hhdt" then do                                  // heavy duty truck toll
                                                                                  
         CostFld =  "*HHDCST"+periods[i]                               // minimizing cost field 
         SkimVar1 = "*STM" +periods[i]                               // first skim varaible (in addition to LENGTH)
         SkimVar2 = "ITOLL2" + periods[i]
         SkimVar3 = null                                                                     
         skimbyset1 = null                                           // second skim varaible
         skimbyset2 = null                                           // third skim variable
   
         excl_qry = "!(((ihov=1|ihov=4|((ihov=2|ihov=3)&(itoll"+periods[i]+">0&abln"+periods[i]+"<9)))|ifc>7)&(ITRUCK=1|ITRUCK>4))" // query for hhd toll exclusion link set
          
         set =  mode
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where !(((ihov=1|ihov=4|((ihov=2|ihov=3)&(itoll"+periods[i]+">0&abln"+periods[i]+"<9)))|ifc>7)&(ITRUCK=1|ITRUCK>4))",)
         if n = 0 then excl_qry=null                                                    // reset value if no selection records 

         // skimbyset1 = length on toll lanes
         set = "tolldst"+periods[i]
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where ((ihov=4|((ihov=2|ihov=3)&(itoll"+periods[i]+">0&abln"+periods[i]+"<9)))&(ITRUCK=1|ITRUCK>4))",)   // for all links
         if n > 0 then skimbyset1={vw_set, {"Length"}}

         skimmat = "imp"+mode+periods[i]+".mtx"                    // output skim matrices        
      end  
                                                                           
      else if mode = "dant" then do

         CostFld =  "*SCST"+periods[i]                             // minimizing cost field
         SkimVar1 = "*STM"+periods[i]                              // first skim varaible (in addition to LENGTH)
         SkimVar2 = null
         SkimVar3 = null
         skimbyset1 = null
         skimbyset2 = null
   
         excl_qry = "!(ihov=1&ITRUCK<5)"                           // query for exclusion link set
   
         skimmat =  "impdan"+periods[i]+".mtx"                     // output skim matrices
      end
      else if mode = "dat" then do
         
         CostFld = "*SCST"+periods[i]                                // minimizing cost field
         SkimVar1 = "*STM"+periods[i]                                // first skim varaible (in addition to LENGTH)
         SkimVar2 = "ITOLL"+periods[i]
         SkimVar3 = "ITOLL2"+periods[i]
         skimbyset1 = null
         skimbyset2 = null

         excl_qry = "!(((ihov=1|ihov=4|((ihov=2|ihov=3)&(itoll"+periods[i]+">0&abln"+periods[i]+"<9)))|ifc>7)&ITRUCK<5)"
         set = mode
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several","Select * where "+excl_qry,)
         if n = 0 then excl_qry=null   //reset value if no selection records
         
         // skimbyset1 = length on toll lanes
         set = "datdst"+periods[i]
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where ((ihov=4|((ihov=2|ihov=3)&(itoll"+periods[i]+">0&abln"+periods[i]+"<9)))&ITRUCK<5)",)
         if n > 0 then skimbyset1={vw_set, {"Length"}}
   
         skimmat = "impdat"+periods[i]+".mtx"
      end
      else if mode = "s2nh" then do
      
         CostFld =  "*H2CST"+periods[i]            // minimizing cost field
         SkimVar1 = "*HTM"+periods[i]
         SkimVar2 = null
         SkimVar3 = null
         skimbyset1 = null
         skimbyset2 = null
         
         excl_qry ="!((ihov=1|(ihov=2&abln"+periods[i]+" <9)|ifc>7)&ITRUCK<5)"//initialize the value
         set = mode
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several","Select * where "+excl_qry,)
         if n = 0 then excl_qry=null   //reset value if no selection records
        
         set = "s2hdst" + periods[i]
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where (abln"+periods[i]+"<9 and ihov=2 and ITRUCK<5)",)
         if n > 0 then skimbyset2={vw_set, {"Length"}}
         
         skimmat = "imps2nh"+periods[i]+".mtx"
      end
      else if mode = "s2th" then do
         
         CostFld = "*H2CST"+periods[i]            // minimizing cost field
         SkimVar1 ="*HTM"+periods[i]
         SkimVar2 = null
         SkimVar3 = null
         skimbyset1 = null
         skimbyset2 = null

         excl_qry = "!(((ihov=1|(ihov=2&abln"+periods[i]+"<9)|ihov=4|(ihov=3&itoll"+periods[i]+">0&abln"+periods[i]+"<9))|ifc>7)&ITRUCK<5)"
         set = mode
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several","Select * where " + excl_qry,)
         if n = 0 then excl_qry=null   //reset value if no selection records
         
         set = "s2tdst"+periods[i]
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where (((abln"+periods[i]+"<9 & ihov=2) | (ihov=4 | (ihov=3 & itoll"+periods[i]+" >0 & abln"+periods[i]+"< 9)))& ITRUCK<5)",)
         if n > 0 then skimbyset1={vw_set, {"Length"}}         
         
         set = "s2t"+periods[i]
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where ((ihov=4|(ihov=3 & itoll"+periods[i]+" >0 & abln"+periods[i]+" < 9)) & ITRUCK<5)",)
         if n > 0 then skimbyset2={vw_set, {"itoll"+periods[i]}}
         
         skimmat = "imps2th"+periods[i]+".mtx"
      
      end
      else if mode = "s3nh" then do
      
         CostFld =  "*H3CST"+periods[i]            // minimizing cost field
         SkimVar1 = "*HTM" +periods[i]
         SkimVar2 = null
         SkimVar3 = null
         skimbyset1 = null
         skimbyset2 = null

         excl_qry = "!((ihov=1|((ihov=2|ihov=3)&abln"+periods[i]+"<9)|ifc>7)& ITRUCK<5)"
         set =  mode
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where " + excl_qry,)
         if n = 0 then excl_qry=null
         
         set = "s3hdst"+periods[i]
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where (abln"+periods[i]+"<9 & (ihov=2 | ihov=3) & ITRUCK<5)",)
         if n > 0 then skimbyset2={vw_set, {"Length"}}
         
         skimmat = "imps3nh"+periods[i]+".mtx"
         
      end
      else if mode = "s3th" then do
 
         CostFld = "*H3CST" + periods[i]                         // minimizing cost field
         SkimVar1 = "*HTM" + periods[i]
         SkimVar2 = null
         SkimVar3 = null
         skimbyset1 = null
         skimbyset2 = null
      
         excl_qry = "(abln"+periods[i]+"=9 | ITRUCK >4)"     
         set =  mode + periods[i]
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where " + excl_qry,)
         if n = 0 then excl_qry=null
      
         set = "s3tdst" + periods[i]
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where (((abln"+periods[i]+"<9 and (ihov=2 or ihov=3)) or ihov=4)and ITRUCK <5)",)
         if n > 0 then skimbyset1={vw_set, {"Length"}}
         
         set = "s3t" + periods[i]
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where (ihov=4 and ITRUCK <5)",)
         if n > 0 then skimbyset2={vw_set, {"itoll"+periods[i]}}         
      
         skimmat = "imps3th"+periods[i]+".mtx"
      
      end
      
      //delete existing skim file
      ok=RunMacro("SDdeletefile",{outputDir+"\\"+skimmat}) 
      if !ok then goto quit
   
      //skim network
      set = "AllLinks"
      vw_set = link_lyr + "|" + set
      SetLayer(link_lyr)
      n = SelectAll(set) 
      NetOpts = null
      NetOpts.[Link ID] = link_lyr+".ID"
      NetOpts.[Type] = "Enable"
      ChangeLinkStatus(net,vw_set, NetOpts)               // first enable all links
   
      if excl_qry<>null then do
         set = "toll"
         vw_set = link_lyr + "|" + set
         SetLayer(link_lyr)
         n = SelectByQuery(set, "Several", "Select * where "+excl_qry,) 
        NetOpts = null
        NetOpts.[Link ID] = link_lyr+".ID"
        NetOpts.[Type] = "Disable"
        ChangeLinkStatus(net,vw_set, NetOpts)               // disable exclusion query
      end

      Opts = null
      Opts.Input.Network = net_file
      Opts.Input.[Origin Set]   = {db_node_lyr, node_lyr, "Centroids", "select * where ID <= " + i2s(mxzone)}
      Opts.Input.[Destination Set] = {db_node_lyr, node_lyr, "Centroids"}
      Opts.Input.[Via Set]     = {db_node_lyr, node_lyr}
      Opts.Field.Minimize     = CostFld
      Opts.Field.Nodes = node_lyr + ".ID"
      if SkimVar2=null and SkimVar3=null then do
        Opts.Field.[Skim Fields]={{"Length","All"},{SkimVar1,"All"}}
      end
      else if SkimVar3=null then do
        Opts.Field.[Skim Fields]={{"Length","All"},{SkimVar1,"All"},{SkimVar2,"All"}}
      end
      else do
        Opts.Field.[Skim Fields]={{"Length","All"},{SkimVar1,"All"},{SkimVar2,"All"},{SkimVar3,"All"}}      
      end
      if skimbyset1 <> null then  
         if skimbyset2 <> null then
            Opts.Field.[Skim by Set]={skimbyset1,skimbyset2}
         else
            Opts.Field.[Skim by Set]={skimbyset1}
      end
      else if skimbyset2 <> null then 
        Opts.Field.[Skim by Set]={skimbyset2}
        //end of previous if string
      if (mode = "lhdn" | mode = "mhdn" | mode = "hhdn" | mode = "lhdt" | mode = "mhdt" | mode = "hhdt") then 
        if (mode = "lhdn" | mode = "mhdn" | mode = "hhdn") then
         Opts.Output.[Output Matrix].Label = "impedance truck"                                 
        else if (mode = "lhdt" | mode = "mhdt" | mode = "hhdt") then
          Opts.Output.[Output Matrix].Label = "impedance truck toll"  
        else 
         Opts.Output.[Output Matrix].Label = "congested " + mode + " impedance"  
      Opts.Output.[Output Matrix].Compression = 0 //uncompressed, for version 4.8 plus    
      Opts.Output.[Output Matrix].[File Name] = outputDir + "\\"+skimmat
      
      RunMacro("HwycadLog",{"hwyskim.rsc: hwy skim","TCSPMAT: "+skimmat+"; "+CostFld})
      ok = RunMacro("TCB Run Procedure", 1, "TCSPMAT", Opts)
      if !ok then goto quit

      // STEP 2: Intrazonal added by Ziying Ouyang, June 3, 2009
      // mtxcore={"Length (Skim)"}+{SkimVar1[i]+" (Skim)"}+{SkimVar2[i]+" (Skim)"}+{SkimVar3[i]+" (Skim)"}
      mtxcore={"Length (Skim)"}+{SkimVar1+" (Skim)"}
      for j = 1 to mtxcore.length do
         Opts = null
         Opts.Global.Factor = 0.5
         Opts.Global.Neighbors = 3
         Opts.Global.Operation = 1
         Opts.Global.[Treat Missing] = 2
         Opts.Input.[Matrix Currency] = {outputDir + "\\"+skimmat,mtxcore[j], , }
         RunMacro("HwycadLog",{"hwyskim.rsc: hwy skim","Intrazonal: "+skimmat+"; "+mtxcore[j]})
         ok = RunMacro("TCB Run Procedure", j, "Intrazonal", Opts)
         if !ok then goto quit
      end
      
      // Set toll intrazonal to 0
      mtxcoresub={"ITOLL2" + periods[i]}
      m=OpenMatrix(outputDir + "\\"+skimmat,)
      mtxcore=GetMatrixCoreNames(m)
      mtxcore_indices = GetMatrixIndexNames(m)
      for j = 1 to mtxcoresub.length do
        for k = 1 to mtxcore.length do
          pos = Position(mtxcore[k], mtxcoresub[j])
          if pos > 0 then do
            mciToll=CreateMatrixCurrency(m, mtxcore[k], mtxcore_indices[1][1], mtxcore_indices[2][1], )
            x=1
            while x <= mxzone do
              strX = String(x)
              SetMatrixValue (mciToll, strX, strX, 0)
              x=x+1
            end
          end
        end
      end




      //Create CVM Disutility Skims
      if mode = "ldn" then do
        //Light Duty Non-Toll
        ok=RunMacro("DU Skim","ldn",skimmat,periods[i]) 
        if !ok then goto quit   
      end 
      else if mode = "lhdn" then do
        //Truck Medium Non-Toll
        ok=RunMacro("DU Skim","lhdn",skimmat,periods[i]) 
        if !ok then goto quit
      end
      else if mode = "mhdn" then do
        //Truck Medium Non-Toll
        ok=RunMacro("DU Skim","mhdn",skimmat,periods[i]) 
        if !ok then goto quit 
      end
      else if mode = "hhdn" then do
        //Truck Heavy Non-Toll
        ok=RunMacro("DU Skim","hhdn",skimmat,periods[i]) 
        if !ok then goto quit 
      end
      if mode = "ldt" then do
        //Light Duty Toll
        ok=RunMacro("DU Skim","ldt",skimmat,periods[i]) 
        if !ok then goto quit   
      end 
      else if mode = "lhdt" then do
        //Truck Medium Toll
        ok=RunMacro("DU Skim","lhdt",skimmat,periods[i]) 
        if !ok then goto quit
      end
      else if mode = "mhdt" then do
        //Truck Medium Toll
        ok=RunMacro("DU Skim","mhdt",skimmat,periods[i]) 
        if !ok then goto quit 
      end
      else if mode = "hhdt" then do
        //Truck Heavy Toll
        ok=RunMacro("DU Skim","hhdt",skimmat,periods[i]) 
        if !ok then goto quit 
      end   
    end
 
   ok=1
   runmacro("close all")
   quit:   

   return(ok)   
  
EndMacro


Macro "DU Skim" (mode,skimmat,period) //Disutility Skim
    shared outputDir

    //Cores to add together (time,length,toll)
    toll_core={4,2,3}
    nontoll_core={3,2}
    corenameDU="DU"+period

    //Disutility Coefficients for Light Vehicle, Medium Vehicle, Heavy Vehicles
    DUCoef={{-0.313,-0.138,-0.01},{-0.313,-0.492,-0.01},{-0.302,-0.580,-0.02}} //Toll coefficients are scaled to convert to cents, heavy truck tolls are *2 for SR125 toll difference for trucks
  
    //Create Medium and Heavy from Truck skim, Setup names for later
    if mode = "ldn" then do
      util=1
      core=nontoll_core
    end
    if mode = "ldt" then do
      util=1
      core=toll_core
    end
    if mode = "lhdn" then do
      util=2
      core=nontoll_core
    end
    if mode = "lhdt" then do
      util=2
      core=toll_core
    end
    if mode = "mhdn" then do
      util=2
      core=nontoll_core
    end
    if mode = "mhdt" then do
      util=2
      core=toll_core
    end
    if mode = "hhdn" then do
      util=3
      core=nontoll_core
    end
    if mode = "hhdt" then do
      util=3
      core=toll_core
    end

    //open matrix, add disultility core, get corenames, get matrix id's, and create matrix currencies
    m = OpenMatrix(outputDir + "\\"+skimmat, )
    AddMatrixCore(m, corenameDU)
    corename = GetMatrixCoreNames(m)
    curr_idx = GetMatrixIndex(m)
    mcTime = CreateMatrixCurrency(m, corename[core[1]], curr_idx[1], curr_idx[2], )
    mcLength = CreateMatrixCurrency(m, corename[core[2]], curr_idx[1], curr_idx[2], )
    if core.length = 3 then do //toll matrix
      mcToll = CreateMatrixCurrency(m, corename[core[3]], curr_idx[1], curr_idx[2], )
    end
    mcDU = CreateMatrixCurrency(m, corenameDU, curr_idx[1], curr_idx[2], )

    //calc disutility
    if core.length = 3 then do //toll matrix
      mcDU := DUCoef[util][1]*mcTime + DUCoef[util][2]*mcLength + DUCoef[util][3]*mcToll
    end
    else do
      mcDU := DUCoef[util][1]*mcTime + DUCoef[util][2]*mcLength     
    end
    return(1)

EndMacro

