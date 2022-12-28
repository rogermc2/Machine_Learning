
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
with Classifier_Utilities;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with ML_Types;
with Neural_Processes; use Neural_Processes;
--  with Python; use Python;

with Network; use Network;

procedure Neural_Net is
   use Classifier_Utilities;
   Project_Name : constant String := "Neural_Net ";
   X_Train       : constant Real_Float_Matrix :=
                     Load_Data ("../../datasets/x_train.csv", 14) / 255.0;
   X_Test        : constant Real_Float_Matrix :=
                     Load_Data ("../../datasets/x_test.csv", 14) / 255.0;
   Y_Train       : constant Real_Float_Matrix :=
                     Load_Data ("../../datasets/y_train.csv", 10);
   Y_Test        : constant Real_Float_Vector :=
                     Load_Data ("../../datasets/y_test.csv");
   Minibatches   : constant Positive := 40000;
   Learning_Rate : constant Float := 1.0;
   Net           : Network_Data;
   Predictions   : Real_Matrix_List;
   Confusion     : Integer_Matrix (1 .. 10, 1 .. 10) :=
   (others => (others => 0));
   --     Py_Module   : Module;
begin
   Print_Matrix_Dimensions (Project_Name & "X_Train" , X_Train);
   Print_Matrix_Dimensions (Project_Name & "Y_Train", Y_Train);
   Print_Matrix_Dimensions (Project_Name & "X_Test", X_Test);
   Put_Line (Project_Name & "Y_Test length:" &  Integer'Image (Y_Test'Length));
   Add_Fully_Connected_Layer (Net.Layers, 14 * 14, 100);
   Add_Activation_Layer (Net.Layers);
   Add_Fully_Connected_Layer (Net.Layers, 100, 50);
   Add_Activation_Layer (Net.Layers);
   Add_Fully_Connected_Layer (Net.Layers, 50, 10);
   Add_Activation_Layer (Net.Layers);

   Fit (Net, X_Train, Y_Train, Minibatches, Learning_Rate);

   --  Build the confusion matrix using the test set predictions
--     Predictions := Predict (Net, X_Test);
--     for index in Y_Test'Range loop
--        Confusion (Integer (Y_Test (index)), Arg_Max (Predictions (index))) :=
--        Confusion (Integer (Y_Test (index)), Arg_Max (Predictions (index))) + 1 ;
--     end loop;
   --     Python.Initialize;
   --     Py_Module := Import_File ("neural_net");
   --     Python.Call (Py_Module, "load");
   --     New_Line;
   --     Python.Finalize;

   New_Line;

end Neural_Net;
