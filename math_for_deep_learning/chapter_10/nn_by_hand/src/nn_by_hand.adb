
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
--  with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python; use Python;

with NN_By_Hand_Support; use NN_By_Hand_Support;

procedure NN_By_Hand is

   Project_Name : constant String := "NN_By_Hand ";

   Data         : constant Dataset := Build_Dataset;
   Net          : Network_Package.Map;
   Tn           : Natural;
   Fp           : Natural;
   Fn           : Natural;
   Tp           : Natural;
   Pred         : ML_Types.Integer_List;
   Py_Module    : Module;
begin
   Evaluate (Net, Data.X_Test, Data.Y_Test, Tn, Fp, Fn, Tp, Pred);
   Put_Line (Project_Name & "Tn" & Integer'Image (Tn));
   Put_Line (Project_Name & "Fp" & Integer'Image (Fp));
   Put_Line (Project_Name & "Fn" & Integer'Image (Fn));
   Put_Line (Project_Name & "Tp" & Integer'Image (Tp));
   Print_Integer_List ("Pred", Pred);

   Python.Initialize;
   Py_Module := Import_File ("nn_by_hand");
--     Python.Call (Py_Module, "show_contours", X_Data, Y_Data, Z);

--     Plot_Points (Py_Module, -0.5, 2.3, 0.02);
--     Plot_Points (Py_Module, 2.3, 2.3, 0.01);

--     Python.Call (Py_Module, "show");
   New_Line;
   Python.Finalize;
   Put_Line (Project_Name & "done");

end NN_By_Hand;
