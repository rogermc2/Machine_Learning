
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;

procedure TF_Quick_Start is
   Program_Name   : constant String := "TF_Quick_Start ";
   Test_Size      : constant Positive := 20;
   Train_Size     : constant Positive := 1000;
   Num_Features   : constant Positive := 28 * 28;
   Num_Labels     : constant Positive := 10;
   X_Train        : Real_Float_Matrix (1 .. Train_Size, 1 .. Num_Features);
   Y_Train        : Integer_Matrix (1 .. Train_Size, 1 .. Num_Labels);
   X_Test         : Real_Float_Matrix (1 .. Test_Size, 1 .. Num_Features);
   Y_Test         : Integer_Matrix (1 .. Test_Size, 1 .. Num_Labels);
   TF_Module      : Python.Module;
begin
   --  on High Sierra, TF_Quick_Start must be run with GPS started from
   --  conda activate tf3.9
   Put_Line (Program_Name);

   Python.Initialize;
   TF_Module := Python.Import_File ("quick_start");
   Python.Call (TF_Module, "version");
   Python.Call (TF_Module, "load_data", X_Train, Y_Train, X_Test, Y_Test);

   Python.Close_Module (TF_Module);
   Python.Finalize;

   Print_Float_Matrix (Program_Name & "X_Train", X_Train, 1, 2, 204, 210);
   Print_Integer_Matrix (Program_Name & "Y_Train", Y_Train, 1, 2);
   Print_Float_Matrix (Program_Name & "X_Test", X_Test, 1, 2, 204, 210);
   Print_Integer_Matrix (Program_Name & "Y_Test", Y_Test, 1, 2);

   Put_Line ("----------------------------------------------");
   New_Line;

end TF_Quick_Start;
