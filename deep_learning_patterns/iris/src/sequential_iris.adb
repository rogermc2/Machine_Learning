
with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with Classifier_Loader;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
--  with Python;
--  with Shuffler;

with Neural_Model; use Neural_Model;

--  Each neuron is a very simple function that considers a weighted sum of
--  incoming signals and then compares the value of that sum against some threshold.
procedure Sequential_Iris is
   use Classifier_Loader;
   Program_Name : constant String := "Sequential_Iris ";

   Iris_Data       : constant ML_Types.Multi_Output_Data_Record :=
                       Load_Data ("src/iris.csv");
   Iris_Features   : constant ML_Types.Value_Data_Lists_2D :=
                       Iris_Data.Feature_Values;
   Class_Names     : ML_Types.Class_Names_List;
   Num_Samples     : constant Positive := Positive (Iris_Features.Length);
   Num_Features    : constant Positive := 13;
   Num_Classes     : constant Positive := 1;
   Num_Epochs      : constant Positive := 40;
   Learn_Rate      : constant Float := 0.1;
   Input_Data      : constant Real_Float_Matrix (1 .. Num_Samples,
                                                 1 .. Num_Features)
     := (others => (1.0, 0.0, 0.5, -0.5, 2.3, -5.2, 10.9, -12.0,
                    4.5, 6.9, -0.1, 7.0, -8.0));
   Labels          : constant Real_Float_Matrix (1 .. Num_Samples, 1 .. 1)
     := (others => (others => 0.5));
   theModel        : Sequential_Model (Num_Samples, Num_Features, Num_Classes,
                                       Loss_Mean_Square_Error);
   --     Classifier     : Python.Module;
begin
   New_Line;
   Put_Line ("Program " & Program_Name);
   Class_Names.Append (To_Unbounded_String ("Setosa"));
   Class_Names.Append (To_Unbounded_String ("Versicolour"));
   Class_Names.Append (To_Unbounded_String ("Virginica"));

   Assert (Num_Samples > 0, Program_Name &
             " called with empty Features vector.");
   Add_Labels (theModel, Labels);
   Add_First_Layer (theModel, Get_Row (Input_Data, 1));
   Add_Layer (theModel, 10, ReLu_Activation);
   Add_Layer (theModel, 10, ReLu_Activation);
   Add_Layer (theModel, 1, Identity_Activation);
   Add_Connections (theModel);

   Compile (theModel, Num_Epochs, Learn_Rate);
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

end Sequential_Iris;
