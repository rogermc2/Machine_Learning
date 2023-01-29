
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
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
   Words                  : constant ML_Types.Unbounded_List :=
                              Word_List (Word_Dict);
   Train_Data             : constant Data_Record :=
                              Get_Data (Train_File_Name, Word_Dict);
   Test_Data              : constant Data_Record :=
                              Get_Data (Test_File_Name, Word_Dict);
   Max_Leaf_Nodes         : constant Positive := 6;
   CLF                    : Python_API.PyObject;

   procedure Do_Predictions is
      Predictions : constant Integer_Array := Python_CLF.Call
        (Classifier, "predict", CLF, Test_Data.Features);
      Correct     : Natural := 0;
      Accuracy    : Float;
   begin
--        Put_Line (Project_Name & "Predictions lenght: " &
--                 Integer'Image (Predictions'Length));
--        Put_Line (Project_Name & "Test_Data.Labels lenght: " &
--                 Integer'Image (Integer (Test_Data.Labels.Length)));
--        Print_Integer_Array (Project_Name & "Prediction: ", Predictions, 1, 30);
      for index in Predictions'Range loop
         if Predictions (index) = Test_Data.Labels (index) then
            Correct := Correct + 1;
         end if;
      end loop;

      Accuracy := Float (Correct) / Float (Predictions'Length);
      Put_Line (Project_Name & "Accuracy: " & Float'Image (Accuracy));

   end Do_Predictions;

begin
--     Print_Unbound_List (Project_Name & "Words", Words);
   Python.Initialize;
   Classifier := Import_File ("lesson_6a");
   CLF := Python_CLF.Call (Classifier, "init_classifer", Max_Leaf_Nodes);
   --  Train the model.
   Python_CLF.Call (Classifier, "fit", CLF, Train_Data.Features,
                    Train_Data.Labels);
   Do_Predictions;
   Python_CLF.Call (Classifier, "show_tree", CLF, Words);

   Python.Finalize;

   Put_Line (Project_Name & "finished.");

end Lesson_6A;
