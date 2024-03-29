
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Neural_Utilities;
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
   Word_Dict              : constant Dictionary_List :=
                              Read_Vocabulary (Vocab_File_Name);
   Words                  : constant ML_Types.Indef_String_List :=
                              Word_List (Word_Dict);
   Train_Data             : constant Data_Record :=
                              Get_Data (Train_File_Name, Word_Dict);
   Test_Data              : constant Data_Record :=
                              Get_Data (Test_File_Name, Word_Dict);
   CLF                    : Python_API.PyObject;

   procedure Do_Predictions is
      Predictions : constant Integer_Array := Python_CLF.Call
        (Classifier, "predict", CLF, Test_Data.Features);
      Correct     : Natural := 0;
      Accuracy    : Float;
   begin
      for index in Predictions'Range loop
         if Predictions (index) = Test_Data.Labels (index) then
            Correct := Correct + 1;
         end if;
      end loop;

      Accuracy := Float (Correct) / Float (Predictions'Length);
      Put_Line ("Accuracy: " & Float'Image (Accuracy));

   end Do_Predictions;

   procedure Run_Tree (Max_Leaf_Nodes : Positive := 6) is
   begin
      CLF := Python_CLF.Call (Classifier, "init_classifer", Max_Leaf_Nodes);
      --  Train the model.
      Python_CLF.Call (Classifier, "fit", CLF, Train_Data.Features,
                       Train_Data.Labels);
      Put ("Leaves:" & Integer'Image (Max_Leaf_Nodes) & " ");
      Do_Predictions;
   end Run_Tree;

   --  -------------------------------------------------------------------------

   Sentence_1   : constant ML_Types.Indef_String_List :=
                    Neural_Utilities.Split_String_On_Spaces
                      ("yo come over carlos will be here soon");
   Sentence_2   : constant ML_Types.Indef_String_List :=
                    Neural_Utilities.Split_String_On_Spaces
                      ("congratulations thanks to a good friend u have won");
begin
   Python.Initialize;
   Classifier := Import_File ("lesson_6a");
   Run_Tree;
   Python_CLF.Call (Classifier, "show_tree", CLF, Words);
   Python_API.Py_DecRef (CLF);

   for leaves in 1 .. 15 loop
      Run_Tree (2 * leaves);
      Python_API.Py_DecRef (CLF);
   end loop;

   CLF := Python_CLF.Call (Classifier, "multinomial_fit",
                           Train_Data.Features, Train_Data.Labels);

   Put_Line ("Naive Bayes predictions:");
   Do_Predictions;

   Print_Bayes_Data (Classifier, CLF, Word_Dict, Sentence_1);
   Print_Bayes_Data (Classifier, CLF, Word_Dict, Sentence_2);

   Python_CLF.Call
     (Classifier, "print_confusion", CLF, Test_Data.Features,
      Test_Data.Labels);

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_6A;
