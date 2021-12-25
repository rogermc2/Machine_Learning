
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Types;
with Classifier_Utilities;
with Criterion;
with Decision_Tree_Classification;
with Graphviz_Exporter;
with ML_Types;
with Printing;
with Tree;
with Weights;

package body Decision_Path_Tests is
   use ML_Types;

   --  -------------------------------------------------------------------------

   procedure Assert_Correct_Leaf_Indices
     (aClassifier    : Base_Decision_Tree.Classifier;
      X              : Value_Data_Lists_2D;
      Node_Indicator : Classifier_Types.Natural_Lists_2D) is
      Routine_Name   : constant String :=
                         "Decision_Path_Tests.Assert_Correct_Leaf_Indices ";
      Leaves         : constant Classifier_Types.Natural_List :=
                         Base_Decision_Tree.Apply (aClassifier, X);
      Node_List      : Classifier_Types.Natural_List;
      Enum_Leaves    : array (1 .. Integer (Leaves.Length), 1 .. 2) of Integer;
      Leaf_Indicator : array (1 .. Integer (Leaves.Length)) of Integer;
      Count          : Natural := 0;
   begin
      for index in 1 .. Integer (Leaves.Length) loop
         Enum_Leaves (index, 1) := index;
         Enum_Leaves (index, 2) := Leaves.Element (index);
      end loop;

      for index in 1 .. Integer (Enum_Leaves'Length) loop
         for index_2 in 1 .. Integer (Enum_Leaves'Length (2)) loop
            Node_List := Node_Indicator.Element (Enum_Leaves (index, index_2));
            Leaf_Indicator (index) := Node_List.Element (index_2);
         end loop;
      end loop;
      Printing.Print_Natural_List (Routine_Name & "Leaves", Leaves);

      Put_Line (Routine_Name & "Leaf_Indicator");
      for index in 1 .. Integer (Leaf_Indicator'Length) loop
         Put (Integer'Image (Leaf_Indicator(index)));
         Count := Count + 1;
         if Count > 10 then
            Count := 0;
            New_Line;
         end if;
      end loop;

   end Assert_Correct_Leaf_Indices;

   --  -------------------------------------------------------------------------
   --  Based on test_decision_path_hardcoded L1684
   procedure Test_Decision_Path_Hardcoded  is
      use Ada.Containers;
      use Classifier_Utilities;
      use Decision_Tree_Classification;
      --        use Printing;
      use Classifier_Types.Float_Package;
      Routine_Name      : constant String :=
                            "Decision_Path_Tests.Test_Decision_Path_Hardcoded ";
      Iris_Data         : constant Multi_Output_Data_Record :=
                            Load_Data ("src/iris.csv");
      X                 :  constant Value_Data_Lists_2D :=
                            Iris_Data.Feature_Values;
      Num_Samples       : constant Natural := Natural (X.Length);
      T                 : constant Classifier_Types.Multi_Value_Array
        (1 .. 2, 1 .. 3) := ((1, 1, 0), (1, 0, 1));
      Expected          : constant Value_Data_Lists_2D :=
                            To_Multi_Value_List (T);
      X2                : Value_Data_Lists_2D;
      theClassifier     : Base_Decision_Tree.Classifier
        (Tree.Float_Type, Tree.Float_Type, Tree.Float_Type);
      --  Iris_Target (Y) : num outputs x num samples
      Iris_Target       : Value_Data_Lists_2D;
      No_Weights        : Weights.Weight_List := Empty_Vector;
      Node_Indicator    : Classifier_Types.Natural_Lists_2D;
      Success           : Boolean;
   begin
      --  L1698
      C_Init (theClassifier, "2", Criterion.Gini_Criteria, Max_Depth => 1);
      Put_Line (Routine_Name);
      Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");

      --  Iris_Target is 2D list num outputs x num samples
      Iris_Target := Iris_Data.Label_Values;
      Assert (Positive (Iris_Target.Length) = Num_Samples, Routine_Name &
                " invalid Iris_Target vector");
      --  L1687
      Classification_Fit (theClassifier, X, Iris_Target, No_Weights);
      Printing.Print_Tree (Routine_Name & "Tree", theClassifier);
      Put_Line ("----------------------------------------------");
      New_Line;

      X2.Append (X.First_Element);
      X2.Append (X.Element (2));

      Node_Indicator := Base_Decision_Tree.Decision_Path (theClassifier, X2);
      Printing.Print_Natural_Lists_2D (Routine_Name & "Node_Indicator",
                                       Node_Indicator);
      Success := Positive (Node_Indicator.Length) = Num_Samples and
        Node_Indicator.Element (1).Length =
        theClassifier.Attributes.Decision_Tree.Nodes.Node_Count;
      if Success then
         Put_Line (Routine_Name & "test passed.");
      else
         Put_Line (Routine_Name & "test failed.");
         Put_Line (Routine_Name & "Node_Indicator size: " &
                     Count_Type'Image (Node_Indicator.Length) & " x" &
                     Count_Type'Image (Node_Indicator.Element (1).Length));
         New_Line;
      end if;

   end Test_Decision_Path_Hardcoded;

   --  -------------------------------------------------------------------------

   --  Based on test_decision_path L1692
   procedure Test_Decision_Path  is
      use Ada.Containers;
      use Classifier_Utilities;
      use Decision_Tree_Classification;
      --        use Printing;
      use Classifier_Types.Float_Package;
      Routine_Name      : constant String :=
                            "Decision_Path_Tests.Test_Decision_Path ";
      Exporter          : Graphviz_Exporter.DOT_Tree_Exporter;
      Iris_Data         : constant Multi_Output_Data_Record :=
                            Load_Data ("src/iris.csv");
      X                 :  constant Value_Data_Lists_2D :=
                            Iris_Data.Feature_Values;
      Num_Samples       : constant Natural := Natural (X.Length);
      theClassifier     : Base_Decision_Tree.Classifier
        (Tree.Float_Type, Tree.Float_Type, Tree.Float_Type);
      --  Iris_Target (Y) : num outputs x num samples
      Iris_Target       : Value_Data_Lists_2D;
      No_Weights        : Weights.Weight_List := Empty_Vector;
      Node_Indicator    : Classifier_Types.Natural_Lists_2D;
      Success           : Boolean;
   begin
      --  L1698
      C_Init (theClassifier, "2", Criterion.Gini_Criteria);
      Put_Line (Routine_Name);
      Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");

      --  Iris_Target is 2D list num outputs x num samples
      Iris_Target := Iris_Data.Label_Values;
      Assert (Positive (Iris_Target.Length) = Num_Samples, Routine_Name &
                " invalid Iris_Target vector");
      --  L1695
      Classification_Fit (theClassifier, X, Iris_Target, No_Weights);
      Printing.Print_Tree ("The Tree", theClassifier);
      Put_Line ("----------------------------------------------");
      New_Line;

      Node_Indicator := Base_Decision_Tree.Decision_Path (theClassifier, X);
      Printing.Print_Natural_Lists_2D (Routine_Name & "Node_Indicator",
                                       Node_Indicator);
      Success := Positive (Node_Indicator.Length) = Num_Samples and
        Node_Indicator.Element (1).Length =
        theClassifier.Attributes.Decision_Tree.Nodes.Node_Count;
      if Success then
         Put_Line (Routine_Name & "test passed.");
      else
         Put_Line (Routine_Name & "test failed.");
         Put_Line (Routine_Name & "Node_Indicator size: " &
                     Count_Type'Image (Node_Indicator.Length) & " x" &
                     Count_Type'Image (Node_Indicator.Element (1).Length));
         New_Line;
      end if;

      Graphviz_Exporter.C_Init
        (Exporter, theClassifier.Attributes.Decision_Tree);
      Graphviz_Exporter.Export_Graphviz
        (Exporter, theClassifier.Attributes.Decision_Tree,
         Output_File_Name => To_Unbounded_String ("decision_path.dot"));

      Assert_Correct_Leaf_Indices (theClassifier, X, Node_Indicator);

   end Test_Decision_Path;

   --  -------------------------------------------------------------------------

end Decision_Path_Tests;
