
with Ada.Text_IO; use Ada.Text_IO;

--  with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;

procedure TF_Quick_Start is
   Routine_Name   : constant String := "TF_Quick_Start ";
--     Test_Size      : constant Positive := 20;
--     Train_Size     : constant Positive := 1000;
--     Num_Features   : constant Positive := 28 * 28;
--     Num_Labels     : constant Positive := 10;
--     X_Train        : Real_Float_Matrix (1 .. Train_Size, 1 .. Num_Features);
--     Y_Train        : Integer_Matrix (1 .. Train_Size, 1 .. Num_Labels);
--     X_Test         : Real_Float_Matrix (1 .. Test_Size, 1 .. Num_Features);
--     Y_Test         : Integer_Matrix (1 .. Train_Size, 1 .. Num_Labels);
   TF_Module      : Python.Module;
begin
   Put_Line (Routine_Name);

   Python.Initialize;
   TF_Module := Python.Import_File ("quick_start");
   Python.Call (TF_Module, "version");
   Python.Call (TF_Module, "load_data");
--     Python.Call (TF_Module, "load_data", X_Train, Y_Train, X_Test, Y_Test);

   Python.Close_Module (TF_Module);
   Python.Finalize;

   Put_Line ("----------------------------------------------");
   New_Line;

end TF_Quick_Start;