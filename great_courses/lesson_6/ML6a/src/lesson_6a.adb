
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
--  with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python; use Python;
with Python_API;
with Python_CLF;

with Support_6A; use Support_6A;

procedure Lesson_6A is
   Project_Name           : constant String := "Lesson_6A ";
   Vocab_File_Name        : constant String := "../../data/vocab.txt";
   Train_File_Name        : constant String := "../../data/spam-train.csv";
   Test_File_Name         : constant String := "../../data/spam-test.csv";
   Classifier             : Module;
   Word_Dict              : constant Vocablary_Dictionary_Map :=
                              Read_Vocabulary (Vocab_File_Name);
   Train_Data             : constant Data_Record :=
                              Get_Data (Train_File_Name, Word_Dict);
   Test_Data              : constant Data_Record :=
                              Get_Data (Test_File_Name, Word_Dict);
   Max_Leaf_Nodes         : constant Positive := 6;
   --     Accuracy               : Float;
   CLF                    : Python_API.PyObject;
begin
   Python.Initialize;
   Classifier := Import_File ("lesson_6a");
   CLF := Python_CLF.Call (Classifier, "init_classifer", Max_Leaf_Nodes);
   --  Train the model.
   Python_CLF.Call (Classifier, "fit", CLF, Train_Data.Features,
                    Train_Data.Labels);
--     Python_CLF.Call (Classifier, "predict",  Test_Data.Features);
   --     Put_Line (Project_Name & "Accuracy: " & Float'Image (Accuracy));

   Python.Finalize;

   Put_Line (Project_Name & "finished.");

end Lesson_6A;
