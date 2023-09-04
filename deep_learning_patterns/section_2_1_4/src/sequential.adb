
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with Python;
with Shuffler;

with Structure; use Structure;

procedure Sequential is
   Program_Name : constant String := "Sequential ";

   Input_Data   : constant Real_Float_Vector (1 .. 13) :=
                    (1.0, 0.0, 0.5, -0.5, 2.3, -5.2, 10.9, -12.0,
                     4.5, 6.9, -0.1, 7.0, -8.0);
   Loss_Type    : Loss_Kind := Mean_Square_Error_Loss;
   theModel     : Model (Input_Data'Length);
   Output_Node  : Node (1);
   --     Classifier           : Python.Module;
begin
   Add_Node (theModel, 10, ReLu_Activation);
   Add_Node (theModel, 10, ReLu_Activation);
   Add_Node (theModel, 1);
   Make_Connections (theModel);
   Output_Node := Get_Output_Node (theModel);

      --     Python.Initialize;
      --     Classifier := Python.Import_File ("sequential");
      --
      --     Python.Call (Classifier, "plot", X);
      --
      --     Python.Finalize;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

exception
   when Error: Constraint_Error => Put_Line (Program_Name &
                                               "Constraint_Error");
      Put_Line (Exception_Information(Error));
   when Error: others => Put_Line (Program_Name & "exception");
      Put_Line (Exception_Information(Error));

end Sequential;
