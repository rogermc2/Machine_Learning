
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
with CSV_Data_Loader;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_API;
with Python_CLF;
with Support_9AUX_2; use Support_9AUX_2;

procedure Lesson_9AUX_2 is
   use CSV_Data_Loader;
   Routine_Name : constant String := "Lesson Lesson_9AUX_2 ";
   Train_Size   : constant Positive := 510;
   Test_Size    : constant Positive := 200;
   Data         : constant Base_Split_State := Get_Split_State
     ("../../data/diabetes.csv", Diabetes_Data, Train_Size,
      Test_Size, Shuffle => True, Reload => True);
   MS           : constant Integer_Array := Init_MS (Train_Size);
   Repeats      : constant Positive := 20;
   Max_Leaves   : constant Positive := 5;
   Classifier   : Python.Module;
   Estimator    : Python_API.PyObject;
   Mini_Total   : Float;
   Test_Total   : Float;
   Mini_Error   : Real_Float_List;
   Test_Error   : Real_Float_List;
begin
   New_Line;

   Python.Initialize;
   Classifier := Python.Import_File ("lesson_9aux_2");

   for ms_index in MS'Range loop
      Mini_Total := 0.0;
      Test_Total := 0.0;
      for index in 1 .. Repeats loop
         declare
            Mini : constant Data_Record := Mini_Data (Data, MS (ms_index));
         begin
            Estimator := Python.Call (Classifier, "init_tree", Max_Leaves);
            Python_CLF.Call (Classifier, "fit", Estimator, Mini.Features,
                             Mini.Labels);
            declare
               Train_Predictions : constant Real_Float_Vector :=
                                     Python_CLF.Call (Classifier, "predict",
                                                      Estimator, Mini.Features);
               Test_Predictions  : constant Real_Float_Vector :=
                                     Python_CLF.Call (Classifier, "predict",
                                                      Estimator, Data.Test_X);
            begin
               Mini_Total := Mini_Total +
                 Error (Train_Predictions, Mini.Labels);
               Test_Total := Test_Total +
                 Error (Test_Predictions, Data.Test_Y);
            end;

            Python_API.Py_DecRef (Estimator);
         end;

      end loop;
      Mini_Error.Append (Mini_Total / Float (Repeats));
      Test_Error.Append (Test_Total / Float (Repeats));
   end loop;

   Print_Integer_Array ("MS", MS);
   Python.Call (Classifier, "plot", MS, Mini_Error, Test_Error);

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line (Routine_Name & "finished.");
   Put_Line ("----------------------------------------------");
   New_Line;

end Lesson_9AUX_2;
