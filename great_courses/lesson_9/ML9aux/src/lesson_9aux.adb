
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;
with Python_API;
with Python_CLF;
with Support_9AUX; use Support_9AUX;

procedure Lesson_9AUX is
   Routine_Name     : constant String := "Lesson Lesson_9AUX ";
   Train_Data       : constant Data_Record :=
                        Get_Data ("../../data/diabetes.csv");
   Test_Data        : constant Data_Record :=
                        Get_Data ("../../data/diabetes.csv");
--     Train_Size       : constant Positive := Train_Data.Features'Length;
--     Test_Size        : constant Positive := Test_Data.Features'Length;
   Classifier       : Python.Module;
   Estimator        : Python_API.PyObject;
   Train_Error_List : Real_Float_List;
   Test_Error_List  : Real_Float_List;
   Leaves           : ML_Types.Integer_List;
begin
   New_Line;
   Python.Initialize;

   Classifier := Python.Import_File ("lesson_9aux");

--     for nodes in 2 .. 30 loop
   for nodes in 2 .. 3 loop
      Estimator := Python.Call (Classifier, "init_tree", nodes);

      Put_Line (Routine_Name & "fitting nodes: " & Integer'Image (nodes));
      Python_CLF.Call (Classifier, "fit", Estimator, Train_Data.Features,
                       Train_Data.Labels);

      declare
         Train_Predictions : constant Real_Float_Vector :=
                               Python_CLF.Call (Classifier, "predict",
                                                Estimator, Train_Data.Features);
         Test_Predictions  : constant Real_Float_Vector :=
                               Python_CLF.Call (Classifier, "predict",
                                                Estimator, Test_Data.Features);
         Train_Error    : constant Float :=
                               Error (Train_Predictions, Train_Data.Labels);
         Test_Error     : constant Float :=
                               Error (Test_Predictions,  Test_Data.Labels);
      begin
         Leaves.Append (nodes);
         Train_Error_List.Append (Train_Error);
         Test_Error_List.Append (Test_Error);
         Put_Line ("Train Error: " & Float'Image (Train_Error));
         Put_Line ("Test Error: " & Float'Image (Test_Error));
      end;

      Python_API.Py_DecRef (Estimator);
      New_Line;
   end loop;

   Python.Call (Classifier, "plot", Leaves, Train_Error_List,
                Test_Error_List);

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line (Routine_Name & "finished.");
   Put_Line ("----------------------------------------------");
   New_Line;

end Lesson_9AUX;
