
with Ada.Text_IO; use Ada.Text_IO;

--  with Base;
--  with Base_Neural;
with CSV_Data_Loader;
--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Neural_Utilities;
with Python; use Python;
with Python_API;
with Python_CLF;

procedure Lesson_6Q is
   use CSV_Data_Loader;
   Project_Name   : constant String := "Lesson 6Q ";
   Data_File_Name : constant String :=
     "../../../neural_learning/datasets/mnist_784";
   Classifier     : Module;
   Train_Size     : constant Positive := 5000;
   Test_Size      : constant Positive := 1000;
   Data           : constant Base_Split_State :=
     Get_Split_State (Data_File_Name, Digits_Data, Train_Size,
                                      Test_Size, Y_Categorized => False, Reload => True);
   Train_X        : constant Real_Float_Matrix := Data.Train_X;
   Train_Y        : constant Integer_Matrix := Data.Train_Y;
   Test_X         : constant Real_Float_Matrix := Data.Test_X;
   Test_Y         : constant Integer_Matrix := Data.Test_Y;
   CLF            : Python_API.PyObject;

   procedure Do_Predictions is
      Predictions : constant Integer_Array := Python_CLF.Call
        (Classifier, "predict", CLF, Train_X);
      Correct     : Natural := 0;
      Accuracy    : Float;
   begin
      for index in Predictions'Range loop
         if Predictions (index) = Test_Y (index) then
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
      Python_CLF.Call (Classifier, "fit", CLF, Train_X, Train_Y);
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
   --     Python_CLF.Call (Classifier, "show_tree", CLF, Words);
   Python_API.Py_DecRef (CLF);

   --     for leaves in 1 .. 15 loop
   --        Run_Tree (2 * leaves);
   --        Python_API.Py_DecRef (CLF);
   --     end loop;

   CLF := Python_CLF.Call (Classifier, "multinomial_fit", Train_X, Train_Y);

   Do_Predictions;

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_6Q;
