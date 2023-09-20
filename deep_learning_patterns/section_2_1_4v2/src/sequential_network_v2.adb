
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with Python;
with Shuffler;

with Neural_Model; use Neural_Model;

--  Each neuron is a very simple function that considers a weighted sum of
--  incoming signals and then fires in a characteristic way by comparing the
--  value of that sum against some threshold.
procedure Sequential_Network_V2 is
   Program_Name : constant String := "Sequential_Network_V2 ";

   Num_Samples  : constant Positive := 1;
   Num_Features : constant Positive := 13;
   Input_Data   : constant Real_Float_Matrix (1 .. Num_Samples,
                                              1 .. Num_Features)
     := (others => (1.0, 0.0, 0.5, -0.5, 2.3, -5.2, 10.9, -12.0,
                    4.5, 6.9, -0.1, 7.0, -8.0));
   Labels       : constant Real_Float_Matrix (1 .. Num_Samples, 1 .. 1)
     := (others => (others => 0.0));
   theModel     : Sequential_Model (Num_Samples, Num_Features,
                                    Loss_Mean_Square_Error);
   --     Classifier     : Python.Module;
begin
   New_Line;
   Put_Line ("Program " & Program_Name);
   Add_First_Layer (theModel, Input_Data);
   Add_Layer (theModel, 10, ReLu_Activation);
   Add_Layer (theModel, 10, ReLu_Activation);
   Add_Layer (theModel, 1, Soft_Max_Activation);
   Add_Connections (theModel);
   Put_Line (Program_Name & "connections added");

   Compile (theModel);
   --     declare
   --  Output_Data : Real_Float_Vector := Get_Output_Value (theModel);
   --     begin
   --  Print_Float_Vector (Program_Name & "Output_Data", Output_Data);
   --     end;

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

end Sequential_Network_V2;
