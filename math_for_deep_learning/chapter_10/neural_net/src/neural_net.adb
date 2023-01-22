
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
with Classifier_Utilities;
with CSV_Data_Loader;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Neural_Processes; use Neural_Processes;
--  with Python; use Python;

with Network; use Network;
--  1. Feed input data into the neural network.
--  2. The data flows from layer to layer until the output is reached.
--  3. Calculate the error which is a scalar.
--  4. Finally, adjust each parameter (weight or bias) by subtracting
--     the derivative of the error with respect to the parameter.
--  5. Iterate through this process.

procedure Neural_Net is
   use Classifier_Utilities;
   use CSV_Data_Loader;
   use Real_Float_Arrays;
   Project_Name     : constant String := "Neural_Net ";
   Data_Directory   : constant String := "../../datasets/";
   Train_Size       : constant Positive := 10000;
   Test_Size        : constant Positive := 1000;
   Data             : constant Base_Split_State :=
                        Get_Split_State (Data_Directory & "mnist_784",
                                         Digits_Data, Train_Size, Test_Size,
                                         Reload => True);

--     X_Train_Image    : Real_Float_Vector (Data.Train_X'Range (2));
--     Minibatches    : constant Positive := 40000;
   Minibatches      : constant Positive := 4000;
   Learning_Rate    : constant Float := 1.0;
   Net              : Network_Data;
   Predictions      : Real_Float_List_2D;  --  out
   Confusion        : Integer_Matrix (0 .. 9, 0 .. 9) :=
                        (others => (others => 0));
   CM_Col           : Natural;
--     Py_Module        : Module;
begin
   Put_Line (Project_Name);

   Print_Matrix_Dimensions (Project_Name & "X_Train" , Data.Train_X);
   Print_Matrix_Dimensions (Project_Name & "Y_Train", Data.Train_Y);
   Print_Matrix_Dimensions (Project_Name & "X_Test", Data.Test_X);
   Put_Line (Project_Name & "Y_Test length:" &
               Integer'Image (Data.Test_Y'Length));

--     for index in X_Train_Image'Range loop
--        X_Train_Image (index) := Data.Train_X (2, index);
--     end loop;

--     Python.Initialize;
--     Py_Module := Import_File ("neural_net");
--     Python.Call (Py_Module, "show_image", X_Train_Image);
--     Python.Finalize;

   Add_Fully_Connected_Layer (Net.Layers, Layer_Range (Data.Num_Features), 100);
   Add_Activation_Layer (Net.Layers, 100);
   Add_Fully_Connected_Layer (Net.Layers, 100, 50);
   Add_Activation_Layer (Net.Layers, 50);
   Add_Fully_Connected_Layer (Net.Layers, 50, 10);
   Add_Activation_Layer (Net.Layers, 10);
   Net.Verbose := True;

   Fit (Net, Data.Train_X, Data.Train_Y, Minibatches, Learning_Rate);

   --  Build the confusion matrix using the test set predictions
   Predictions := Predict (Net, Data.Test_X);  --  out

   --  Y_Test values range is the digits 0 .. 9
   for index in Data.Test_Y'Range loop
      CM_Col := Arg_Max (Predictions (index)) - 1;
      Confusion (Integer (Data.Test_Y (index)), CM_Col) :=
        Confusion (Integer (Data.Test_Y (index)), CM_Col) + 1 ;
   end loop;

   Print_Integer_Matrix ("Confusion matrix", Confusion);
   Put_Line ("Accuracy: " & Float'Image (Float (Sum_Diagonal (Confusion)) /
               Float (Sum (Confusion))));
   New_Line;

end Neural_Net;
