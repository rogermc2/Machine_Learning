
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with ML_Types;
--  with Neural_Utilities;
--  with Python; use Python;

with Network; use Network;

procedure Neural_Net is
   X_Train  : constant Real_Float_Matrix :=
                Load_Data ("../../datasets/x_train.csv", 14) / 255.0;
   X_Test   : constant Real_Float_Matrix :=
                Load_Data ("../../datasets/x_test.csv", 14) / 255.0;
   Y_Train  : constant Real_Float_Matrix :=
                Load_Data ("../../datasets/y_train.csv", 1);
   Y_Test   : constant Real_Float_Vector :=
                Load_Data ("../../datasets/y_test.csv");
   --     Project_Name : constant String := "Neural_Net ";

   --     Py_Module   : Module;
begin
   --     Python.Initialize;
   --     Py_Module := Import_File ("neural_net");
   --     Python.Call (Py_Module, "load");
   --     New_Line;
   --     Python.Finalize;


   New_Line;

end Neural_Net;
