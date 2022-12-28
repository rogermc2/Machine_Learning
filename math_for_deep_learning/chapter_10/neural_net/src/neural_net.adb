
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with ML_Types;
with Neural_Processes; use Neural_Processes;
--  with Python; use Python;

with Network; use Network;

procedure Neural_Net is
   --     Project_Name : constant String := "Neural_Net ";
   X_Train  : constant Real_Float_Matrix :=
                Load_Data ("../../datasets/x_train.csv", 14) / 255.0;
   X_Test   : constant Real_Float_Matrix :=
                Load_Data ("../../datasets/x_test.csv", 14) / 255.0;
   Y_Train  : constant Real_Float_Matrix :=
                Load_Data ("../../datasets/y_train.csv", 1);
   Y_Test   : constant Real_Float_Vector :=
                Load_Data ("../../datasets/y_test.csv");
   Layer   : Layer_Data;
   Net     : Network_List;
   --     Py_Module   : Module;
begin
   Add_Fully_Connected_Layer (Net, Layer);
   --     Python.Initialize;
   --     Py_Module := Import_File ("neural_net");
   --     Python.Call (Py_Module, "load");
   --     New_Line;
   --     Python.Finalize;

   New_Line;

end Neural_Net;
