
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
with ML_Types;

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
begin
   Evaluate (Net, Data.X_Test, Data.Y_Test, Tn_0, Fp_0, Fn_0, Tp_0,
             Accuracy_0, Pred_0);
   Put_Line ("Pre-training results:");
   Put_Line ("T0" & Integer'Image (Tn_0) & "    F1" & Integer'Image (Fp_0));
   Put_Line ("F0" & Integer'Image (Fn_0) & "    T1" & Integer'Image (Tp_0));
   Print_Integer_List ("Pred", Pred_0);
   Put_Line ("Accuracy " & Float'Image (Accuracy_0));
   New_Line;

   Gradient_Descent (Net, Data.X_Train, Data.Y_Train, Epochs, Eta);
   Evaluate (Net, Data.X_Test, Data.Y_Test, Tn, Fp, Fn, Tp, Accuracy, Pred);
   Put_Line ("Gradient descent results:");
   Put_Line ("T0" & Integer'Image (Tn) & "    F1" & Integer'Image (Fp));
   Put_Line ("F0" & Integer'Image (Fn) & "    T1" & Integer'Image (Tp));
   Print_Integer_List ("Pred", Pred);
   Put_Line ("Accuracy " & Float'Image (Accuracy));
   New_Line;

end NN_By_Hand;
