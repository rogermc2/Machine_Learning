
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
--  with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
--  with Python; use Python;

with NN_By_Hand_Support; use NN_By_Hand_Support;

procedure NN_By_Hand is

--     Project_Name : constant String := "NN_By_Hand ";
   Epochs       : constant Positive := 1000;
   Eta          : constant Float := 0.1;
   Data         : constant Dataset := Build_Dataset;
   Net          : Network_Package.Map := Neural_Net;
   Tn_0         : Natural;
   Fp_0         : Natural;
   Fn_0         : Natural;
   Tp_0         : Natural;
   Accuracy_0   : Float;
   Pred_0       : ML_Types.Integer_List;
   Tn           : Natural;
   Fp           : Natural;
   Fn           : Natural;
   Tp           : Natural;
   Accuracy     : Float;
   Pred         : ML_Types.Integer_List;
--     Py_Module    : Module;
begin
   Print_Integer_Array ("Data.Data.Y_Test", Data.Y_Test);
   Evaluate (Net, Data.X_Test, Data.Y_Test, Tn_0, Fp_0, Fn_0, Tp_0,
             Accuracy_0, Pred_0);
   Put_Line ("Pre-training results:");
   Put_Line ("Tn" & Integer'Image (Tn_0) & "    Fp" & Integer'Image (Fp_0));
   Put_Line ("Fn" & Integer'Image (Fn_0) & "    Tp" & Integer'Image (Tp_0));
   Put_Line ("Accuracy " & Float'Image (Accuracy_0));
   Print_Integer_List ("Pred", Pred_0);
   New_Line;

   Gradient_Descent (Net, Data.X_Train, Data.Y_Train, Epochs, Eta);
   Evaluate (Net, Data.X_Test, Data.Y_Test, Tn, Fp, Fn, Tp, Accuracy, Pred);
   Put_Line ("Gradient descent results:");
   Put_Line ("Tn" & Integer'Image (Tn) & "    Fp" & Integer'Image (Fp));
   Put_Line ("Fn" & Integer'Image (Fn) & "    Tp" & Integer'Image (Tp));
   Put_Line ("Accuracy " & Float'Image (Accuracy));
   Print_Integer_List ("Pred", Pred);
   New_Line;

--     Python.Initialize;
--     Py_Module := Import_File ("nn_by_hand");
--     Python.Call (Py_Module, "show_contours", X_Data, Y_Data, Z);

--     Plot_Points (Py_Module, -0.5, 2.3, 0.02);
--     Plot_Points (Py_Module, 2.3, 2.3, 0.01);

--     Python.Call (Py_Module, "show");
   New_Line;
--     Python.Finalize;
   Put_Line ("done");

end NN_By_Hand;
