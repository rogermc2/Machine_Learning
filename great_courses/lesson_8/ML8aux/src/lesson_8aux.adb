
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
   Test_Size        : constant Positive := 1000;
   Train_Size       : constant Positive := Num_Samples - Test_Size;
   Max_Leaf_Nodes   : constant Positive := 7;
   All_Data         : Real_Float_Matrix (1 .. Num_Samples, 1 .. 2);
   Labs             : Boolean_Array (1 .. Num_Samples);
   Data             : Real_Float_Matrix (1 .. Train_Size, 1 .. 2);
   Test_Data        : Real_Float_Matrix (1 .. Test_Size, 1 .. 2);
   Train_Labs       : Boolean_Array (1 .. Train_Size);
   Test_Labs        : Boolean_Array (1 .. Test_Size);
   Classifier       : Python.Module;
   Decision_Tree    : Python_API.PyObject;
   --     Accuracy         : Real_Float_List;
   --     Accuracy_2D      : Real_Float_List_2D;
begin
   for row in All_Data'Range loop
      All_Data (row, 1) := 65.0 + 12.0 * abs (Maths.Random_Float);
      All_Data (row, 2) := 15.0 + 75.0 * abs (Maths.Random_Float);
   end loop;

   for row in Labs'Range loop
      Labs (row) := Comfort (All_Data (row, 1), All_Data (row, 2));
   end loop;

   Train_Test_Split
     (X          => All_Data,   Y         => Labs,
      Train_Size => Train_Size, Test_Size => Test_Size,
      Train_X    => Data,       Train_Y   => Train_Labs,
      Test_X     => Test_Data,  Test_Y    => Test_Labs);

   Python.Initialize;
   Classifier := Python.Import_File ("lesson_8aux");

   Python.Call (Classifier, "plot", Data, Train_Labs);
   Decision_Tree := Python.Call (Classifier, "init_DecisionTreeClassifier",
                                 Max_Leaf_Nodes);

   Python_CLF.Call (Classifier, "fit", Decision_Tree, Data, Train_Labs);
   declare
      Predictions : constant Boolean_Array := Python_CLF.Call
        (Classifier, "predict", Decision_Tree, Data);
   begin
      Put_Line ("Accuracy: " &
                  Float'Image (Float  (Correct (Predictions, Labs)) /
                    Float  (Train_Size)));
   end;

   Python_API.Py_DecRef (Decision_Tree);

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_8Aux;
