
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_CLF;
with Python_API;

with Support_8Aux; use Support_8Aux;

procedure Lesson_8Aux is
   Project_Name     : constant String := "Lesson 8Aux ";
   Num_Samples      : constant Positive := 10500;
   Test_Size        : constant Positive := 10000;
   Train_Size       : constant Positive := Num_Samples - Test_Size;
   Max_Leaf_Nodes   : constant Positive := 7;
   Scale            : constant Positive := 7;
   All_Data         : Real_Float_Matrix (1 .. Num_Samples, 1 .. 2);
   Labs             : Boolean_Array (1 .. Num_Samples);
   Data             : Real_Float_Matrix (1 .. Train_Size, 1 .. 2);
   Scaled_Data      : Real_Float_Matrix (Data'Range, Data'Range (2));
   Test_Data        : Real_Float_Matrix (1 .. Test_Size, 1 .. 2);
   Train_Labs       : Boolean_Array (1 .. Train_Size);
   Test_Labs        : Boolean_Array (1 .. Test_Size);
   Train_Pred       : Boolean_Array (Data'Range);
   Test_Pred        : Boolean_Array (Test_Data'Range);
   Classifier       : Python.Module;
   Decision_Tree    : Python_API.PyObject;
   Neighbours       : Python_API.PyObject;
   Comfy            : Real_Vector_List;
   Uncomfy          : Real_Vector_List;
begin
   for row in All_Data'Range loop
      All_Data (row, 1) := 65.0 + 12.0 * abs (Maths.Random_Float);
      All_Data (row, 2) := 15.0 + 75.0 * abs (Maths.Random_Float);
   end loop;

   for row in Labs'Range loop
      Labs (row) := Comfort (All_Data (row, 1), All_Data (row, 2));
   end loop;

   for row in All_Data'Range loop
      if Labs (row) then
         Comfy.Append ((All_Data  (row, 1), All_Data  (row, 2)));
      else
         Uncomfy.Append ((All_Data  (row, 1), All_Data  (row, 2)));
      end if;
   end loop;

   Train_Test_Split
     (X          => All_Data,   Y         => Labs,
      Train_Size => Train_Size, Test_Size => Test_Size,
      Train_X    => Data,       Train_Y   => Train_Labs,
      Test_X     => Test_Data,  Test_Y    => Test_Labs);

   Python.Initialize;
   Classifier := Python.Import_File ("lesson_8aux");

   Python.Call (Classifier, "xy_plot", Comfy, Uncomfy);
   Decision_Tree := Python.Call (Classifier, "init_DecisionTreeClassifier",
                                 Max_Leaf_Nodes);

   Python_CLF.Call (Classifier, "fit", Decision_Tree, Data, Train_Labs);

   Train_Pred := Python_CLF.Call (Classifier, "predict",
                                  Decision_Tree, Data);
   Test_Pred := Python_CLF.Call (Classifier, "predict",
                                 Decision_Tree, Test_Data);
   declare
      Predictions : constant Unbounded_String_Array :=
                      Get_Predictions (Test_Pred, Test_Labs);
   begin
      Put_Line ("Decision Tree Train accuracy: " &
                  Float'Image (Accuracy (Train_Pred, Labs)));
      Put_Line ("Decision Tree Test accuracy: " &
                  Float'Image (Accuracy (Test_Pred, Test_Labs)));
      Python.Call (Classifier, "plot_predictions", "Decision Tree Model", Test_Data, Predictions);
   end;

   Python_CLF.Call (Classifier, "print_tree", Decision_Tree);

   Python_API.Py_DecRef (Decision_Tree);

   Neighbours:= Python.Call (Classifier, "init_NeighborsClassifier", 1);
   Python_CLF.Call (Classifier, "fit", Neighbours, Data, Train_Labs);
   Train_Pred := Python_CLF.Call (Classifier, "predict",
                                  Neighbours, Data);
   Test_Pred := Python_CLF.Call (Classifier, "predict",
                                 Neighbours, Test_Data);
   declare
      Predictions : constant Unbounded_String_Array :=
                      Get_Predictions (Test_Pred, Test_Labs);
   begin
      Put_Line ("Neighbours Train accuracy: " &
                  Float'Image (Accuracy (Train_Pred, Labs)));
      Put_Line ("Neighbours Test accuracy: " &
                  Float'Image (Accuracy (Test_Pred, Test_Labs)));
      Python.Call (Classifier, "plot_predictions", "K-Nearest Neighbours",
                   Test_Data, Predictions);
   end;
   Python_API.Py_DecRef (Neighbours);

   Scaled_Data := Scale_Data (Data, Scale);
   Neighbours:= Python.Call (Classifier, "init_NeighborsClassifier", 1);
   Python_CLF.Call (Classifier, "fit", Neighbours, Scaled_Data, Train_Labs);
   Train_Pred := Python_CLF.Call (Classifier, "predict", Neighbours, Scaled_Data);
--     Test_Pred := Python_CLF.Call (Classifier, "predict",
--                                   Neighbours, Test_Data);
   declare
      Predictions : constant Unbounded_String_Array :=
                      Get_Predictions (Train_Pred, Train_Labs);
   begin
      Put_Line ("Neighbours Scaled Train accuracy: " &
                  Float'Image (Accuracy (Train_Pred, Labs)));
--        Put_Line ("Neighbours Test accuracy: " &
--                    Float'Image (Accuracy (Test_Pred, Test_Labs)));
      Python.Call (Classifier, "plot_predictions", "Scaled K-Nearest Neighbours",
                   Scaled_Data, Predictions);
   end;

   Python_API.Py_DecRef (Neighbours);
   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_8Aux;
