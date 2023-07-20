
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
--  with Neural_Utilities;
with Python; use Python;
with Python_API;
with Python_CLF;

with Support_22A; use Support_22A;

--  A clinical trial in the 1980s called the Infant Health and Development
--  Program or IHDP looked at the cognitive capacity of premature infants
--  and the impact of treatments on their development.
--  The IHDP dataset contains instances from their randomized study and
--  records properties of the children and their caregivers.
--  An important question the data can address is: does treatment from a
--  specialized therapist result in better outcomes than treatment from other
--  care givers?
procedure Lesson_22A is
--     subtype String_10 is String (1 .. 10);
--     type String10_Array is array (1 .. 5) of String_10;

   Project_Name           : constant String := "Lesson_22A ";
   Data_File_Name         : constant String := "../../data/ihdp_npci_1.csv";
   Classifier             : Module;
   Col                    : ML_Types.Indef_String_List;
--     Col                  : constant String10_Array :=
--                                ("treatment ", "y_factual ", "y_cfactual",
--                                 "mu0       ", "mu1       ");

   CLF                    : Python_API.PyObject_Ptr;

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

--     Sentence_1   : constant ML_Types.Indef_String_List :=
--                      Neural_Utilities.Split_String_On_Spaces
--                        ("yo come over carlos will be here soon");
--     Sentence_2   : constant ML_Types.Indef_String_List :=
--                      Neural_Utilities.Split_String_On_Spaces
--                        ("congratulations thanks to a good friend u have won");
begin
   Python.Initialize;
   Classifier := Import_File ("lesson_22a");
   Run_Tree;
   Python_CLF.Call (Classifier, "show_tree", CLF, Words);
   Python_API.Py_DecRef (CLF);

   for leaves in 1 .. 15 loop
      Run_Tree (2 * leaves);
      Python_API.Py_DecRef (CLF);
   end loop;

--     CLF := Python_CLF.Call (Classifier, "multinomial_fit",
--                             Train_Data.Features, Train_Data.Labels);

   Put_Line ("Naive Bayes predictions:");
   Do_Predictions;

--     Print_Bayes_Data (Classifier, CLF, Word_Dict, Sentence_1);
--     Print_Bayes_Data (Classifier, CLF, Word_Dict, Sentence_2);
--
--     Python_CLF.Call
--       (Classifier, "print_confusion", CLF, Test_Data.Features,
--        Test_Data.Labels);

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_22A;
