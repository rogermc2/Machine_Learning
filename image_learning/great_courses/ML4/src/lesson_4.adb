
with Ada.Containers;
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with IL_Types; use IL_Types;

with Base_Decision_Tree;
with Criterion;
with Data_Splitter;
with Decision_Tree_Classification;
--  with Graphviz_Exporter; with Load_ARFF_Data;
with Openml_Ada;
with Plotting;
with Printing;
with Tree;
with Utilities;
with Weights;

with Support_4;

procedure Lesson_4 is
   use Ada.Containers;
   use Support_4;
   use Decision_Tree_Classification;
   Routine_Name   : constant String := "Lesson_4 ";
   Dataset_Name   : constant String := "mnist_784";
   Return_X_Y     : constant Boolean := True;
   Min_Split       : constant String := "2";
   Test_Size       : constant Positive := 1000;
   Train_Size      : constant Positive := 5000;
   Bunch           : Openml_Ada.Bunch_Data;
   X               : Float_List_2D;  --  rows of columns of values
   Y               : Integer_List;
--     X_Indices       : Integer_List;
--     Y_Indices       : Integer_List;
   Num_Samples     : Positive;
   Train_X         : Float_List_2D;
   Train_Y         : Integer_List;
   Test_X          : Float_List_2D;
   Test_Y          : Integer_List;
   aClassifier     : Base_Decision_Tree.Classifier
      (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
   No_Weights      : Weights.Weight_List := Float_Package.Empty_Vector;
   Prediction_List : Integer_List;
   Prediction      : Integer;
   Correct         : Natural := 0;
   --     Exporter      : Graphviz_Exporter.DOT_Tree_Exporter;

begin
   Put_Line (Routine_Name);
   if not Get_State (Dataset_Name, Train_X, Train_Y, Test_X, Test_Y, Bunch) then
      X := Bunch.Data;
      Y := Bunch.Target;
      Num_Samples := Positive (X.Length);

      Put_Line (Routine_Name & "Num_Samples" & Integer'Image (Num_Samples));
      Assert (X.Length > 0, Routine_Name & "X is empty.");

      Assert (Natural (Y.Length) = Num_Samples, Routine_Name &
                "Y length" & Count_Type'Image (Y.Length) &
                " is different to X length" & Natural'Image (Num_Samples));
      --        Printing.Print_Float_List ("Features row 16", X.Element (16));

      Put_Line (Routine_Name & "permuting");
      X := Utilities.Permute (X);
      Put_Line (Routine_Name & "X permuted");
      Utilities.Permute (Y);
      Put_Line (Routine_Name & "Y permuted");
--        Printing.Print_Float_List ("permuted features row 16", X.Element (16));
      Put_Line (Routine_Name & "splitting data");
      Data_Splitter.Train_Test_Split (X, Y, Test_Size, Train_Size,
                                      Test_X, Test_Y, Train_X, Train_Y);
      Put_Line ("Requested train size: " & Integer'Image (Train_Size));
      Put_Line ("Train data length: " & Count_Type'Image (Train_X.Length));
      X.Clear;
      Y.Clear;
      Save_State (Dataset_Name, Train_X, Train_Y, Test_X, Test_Y,
                  Bunch);
   end if;

   if not Get_Tree (Dataset_Name, aClassifier.Attributes.Decision_Tree) then
      if not Return_X_Y then
         Printing.Print_Strings ("Features", Bunch.Feature_Names);
      end if;
      Printing.Print_Float_List ("Train features row 16", Train_X.Element (16));
      Printing.Print_Float_List ("Test features row 16", Test_X.Element (16));

      Printing.Print_Float_List ("Train features row 417",
                                         Train_X.Element (417));

      Put_Line (Routine_Name & "Plotting");
      Plotting.Display_Image (Train_X.Element (4));
      Plotting.Display_Image (Test_X.Element (4));

      C_Init (aClassifier, Min_Split, Criterion.Gini_Criteria,
              Max_Leaf_Nodes => 170);

      --  Fit function adjusts weights according to data values so that better
      --  accuracy can be achieved
      Put_Line ("Classification_Fit");
      Classification_Fit (aClassifier, Train_X, Train_Y, No_Weights);
      Support_4.Save_Tree (Dataset_Name, aClassifier);
   end if;

   Put_Line ("----------------------------------------------");
   New_Line;

   Put_Line ("Train data length: " & Count_Type'Image (Train_X.Length));
   Put_Line ("Test data length: " & Count_Type'Image (Test_X.Length));
   Prediction_List := Base_Decision_Tree.Predict (aClassifier, Train_X);
   for index in Train_X.First_Index .. Train_X.Last_Index loop
--        Put_Line (Routine_Name & "Train_X index" & Integer'Image (index));
      Prediction := Prediction_List.Element (index);
      if Prediction = Train_Y.Element (index) then
         Correct := Correct + 1;
      end if;
   end loop;

   Put_Line ("Prediction: " &
               Float'Image (100.0 * Float (Correct) / Float (Train_X.Length)));
   New_Line;

   --     Graphviz_Exporter.C_Init
   --       (Exporter, aClassifier.Attributes.Decision_Tree);
   --     Graphviz_Exporter.Export_Graphviz
   --       (Exporter, aClassifier.Attributes.Decision_Tree, Feature_Names => Features,
   --        Output_File_Name => To_Unbounded_String ("diabetes.dot"));

end Lesson_4;
