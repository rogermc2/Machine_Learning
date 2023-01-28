
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
--  with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python; use Python;

--  with Maths;

--  with ML; use ML;
with Support_6A; use Support_6A;

procedure Lesson_6A is

--     type Integer3_Array is array (Integer range 1 .. 3) of Integer;

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
--     Weights                : Real_Float_Vector (1 .. 4);
begin

   Python.Initialize;
   Classifier := Import_File ("lesson_6a");
   Python.Call (Classifier, "classify", Train_Data.Features,
                Train_Data.Labels);

   --  Train the model.

   Python.Finalize;

   Put_Line (Project_Name & "finished.");

end Lesson_6A;
