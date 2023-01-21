
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
with Classifier_Utilities;
with Load_Dataset;
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
   use Real_Float_Arrays;
   Project_Name     : constant String := "Neural_Net ";
--     Data_Directory   : constant String := "../../datasets/";
   Data_Directory   : constant String :=
                        "../../../neural_learning/datasets/";
   Num_Feature_Cols : constant Positive := 14 * 14;
   Features         : constant Real_Float_Matrix := Load_Dataset.Load_Features
     (Data_Directory & "mnist_784.csv", Num_Feature_Cols);
   Labels           : constant Real_Float_Matrix :=
                        To_Real_Float_Matrix (Load_Dataset.Load_Labels
     (Data_Directory & "mnist_784.csv"));
--     Train_Data       : constant Load_Dataset.Digits_Data_Record :=
--                          Load_Data_Set (Data_Directory & "mnist_784.csv",
--                                        Num_Feature_Cols, 20000);
--     Test_Data        : constant Load_Dataset.Digits_Data_Record :=
--                          Load_Data_Set (Data_Directory & "mnist_784.csv",
--                                        Num_Feature_Cols, 10000);
   X_Train          : constant Real_Float_Matrix := Features / 255.0;
   X_Test           : constant Real_Float_Matrix := Features / 255.0;
   Y_Train          : constant Real_Float_Matrix := Labels;
   Y_Test           : Real_Float_Vector (Labels'Range);
--     X_Train          : constant Real_Float_Matrix :=
--                          Load_Data (Data_Directory & "x_train.csv",
--                                     Num_Feature_Cols) / 255.0;
--     X_Test           : constant Real_Float_Matrix :=
--                          Load_Data (Data_Directory & "x_test.csv",
--                                     Num_Feature_Cols) / 255.0;
--     Y_Train          : constant Real_Float_Matrix :=
--                          Load_Data (Data_Directory & "y_train.csv", 10);
--     Y_Test           : constant Real_Float_Vector :=
--                          Load_Data (Data_Directory & "y_test.csv");
   --     X_Train_Image  : Real_Float_Vector (X_Train'Range (2));
   --     Minibatches    : constant Positive := 40000;
   Minibatches      : constant Positive := 4;
   Learning_Rate    : constant Float := 1.0;
   Net              : Network_Data;
   Predictions      : Real_Float_List_2D;  --  out
   Confusion        : Integer_Matrix (0 .. 9, 0 .. 9) :=
                        (others => (others => 0));
   CM_Col           : Natural;
   --     Py_Module      : Module;
begin
   Put_Line (Project_Name);
   for index in Y_Test'Range loop
      Y_Test (index) := Labels (index, 1);
   end loop;
   Print_Matrix_Dimensions (Project_Name & "X_Train" , X_Train);
   Print_Matrix_Dimensions (Project_Name & "Y_Train", Y_Train);
   Print_Matrix_Dimensions (Project_Name & "X_Test", X_Test);
   Put_Line (Project_Name & "Y_Test length:" &  Integer'Image (Y_Test'Length));
   --     Print_Float_Matrix (Project_Name & "Y_Train", Y_Train, 1, 3);

   --     for index in X_Train_Image'Range loop
   --        X_Train_Image (index) := X_Train (X_Train'Last, index);
   --     end loop;

   --     Python.Initialize;
   --     Py_Module := Import_File ("neural_net");
   --     Python.Call (Py_Module, "show_image", X_Train_Image);
   --     Python.Finalize;

   Add_Fully_Connected_Layer (Net.Layers, Layer_Range (Num_Feature_Cols), 100);
   Add_Activation_Layer (Net.Layers, 100);
   Add_Fully_Connected_Layer (Net.Layers, 100, 50);
   Add_Activation_Layer (Net.Layers, 50);
   Add_Fully_Connected_Layer (Net.Layers, 50, 10);
   Add_Activation_Layer (Net.Layers, 10);
   Net.Verbose := True;

   Fit (Net, X_Train, Y_Train, Minibatches, Learning_Rate);

   --  Build the confusion matrix using the test set predictions
   Predictions := Predict (Net, X_Test);  --  out
   --     Put_Line ("Predictions size:" & Integer'Image (Integer (Predictions.Length))
   --               & " x" & Integer'Image (Integer (Predictions (1).Length)));
   --     Print_Float_Vector ("Y_Test 3", Y_Test, 3, 3);
   Print_Real_Float_List_2D ("Predictions 1 .. 3", Predictions, 1, 3);

   --  Y_Test values range is the digits 0 .. 9
   for index in Y_Test'First .. Y_Test'First + 5 loop
      CM_Col := Arg_Max (Predictions (index)) - 1;
      Put_Line (Project_Name & "Row, CM_Col, True value:" &
                  Integer'Image (index) & Integer'Image (CM_Col) &
                  Integer'Image (Integer (Y_Test (index))));
      Confusion (Integer (Y_Test (index)), CM_Col) :=
        Confusion (Integer (Y_Test (index)), CM_Col) + 1 ;
   end loop;
   --
   --     Print_Integer_Matrix ("Confusion matrix", Confusion);
   --     Put_Line ("Confusion diagonal sum:" &
   --                 Integer'Image (Sum_Diagonal (Confusion)));
   --     Put_Line ("Confusion sum:" & Integer'Image (Sum (Confusion)));
   --     Put_Line ("Accuracy: " & Float'Image (Float (Sum_Diagonal (Confusion)) /
   --                 Float (Sum (Confusion))));

   Put_Line (Project_Name & "done.");
   New_Line;

end Neural_Net;
