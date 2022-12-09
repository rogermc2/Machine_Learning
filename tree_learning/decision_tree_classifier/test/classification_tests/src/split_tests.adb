--  Based on scikit-learn/sklearn/tree/tests.test_tree.py

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
--  with Printing;
with Tree;
with Weights;

package body Split_Tests is
   use ML_Types;

   function Check_Min_Split (aClassifier : Base_Decision_Tree.Classifier;
                             Min_Split   : out Natural) return Boolean is
      use Tree;
      use Nodes_Package;
      Nodes             : constant Nodes_Package.Tree :=
                            aClassifier.Attributes.Decision_Tree.Nodes;
      Min_Samples_Split : constant Base_Decision_Tree.Split_Value_Record :=
                            aClassifier.Parameters.Min_Samples_Split;
      Min_Split_Rule    : Float;
      Num_Split         : Integer := Integer'Last;
      OK                : Boolean := True;

      procedure Check (Curs : Cursor) is
         use Base_Decision_Tree;
         Node : constant Tree_Node := Element (Curs);
      begin
         case Min_Samples_Split.Value_Kind is
            when Split_Float =>
               Min_Split_Rule := Min_Samples_Split.Float_Value;
            when Split_Integer =>
               Min_Split_Rule := Float (Min_Samples_Split.Integer_Value);
            when others => null;
         end case;
         if Curs /= Nodes.Root and then not Node.Leaf_Node  then
            OK := OK and Float (Node.Num_Node_Samples) >= Min_Split_Rule;
            if Node.Num_Node_Samples < Num_Split then
               Num_Split := Node.Num_Node_Samples;
            end if;
         end if;
      end Check;

   begin
      Iterate (Nodes, Check'Access);
      Min_Split := Num_Split;

      return OK;

   end Check_Min_Split;

   --  -------------------------------------------------------------------------
   --  L664
   procedure Test_Min_Samples_Split is
      use Ada.Containers;
      use Base_Decision_Tree;
      use Classifier_Utilities;
      use Decision_Tree_Classification;
--        use Printing;
      use Classifier_Types.Float_Package;
      Routine_Name              : constant String :=
                                    "Split_Tests.Test_Min_Samples_Split ";
      Iris_Data                 : constant Multi_Output_Data_Record :=
                                    Load_Data ("../../../datasets/iris.csv");
      --  L666
      X                         : constant Value_Data_Lists_2D :=
                                    Iris_Data.Feature_Values;
      Num_Samples               : constant Natural := Natural (X.Length);
      theClassifier             : Classifier (Tree.Float_Type, Tree.Float_Type,
                                              Tree.Float_Type);
      Exporter                  : Graphviz_Exporter.DOT_Tree_Exporter;
      --        Short         :  Value_Data_Lists_2D;
      --  Y: num outputs x num classes
      Y                         : Value_Data_Lists_2D;
      No_Weights                : Weights.Weight_List := Empty_Vector;
      Min_Split                 : Natural;
   begin
      Put_Line (Routine_Name & " Num_Samples" & Integer'Image (Num_Samples));
      Put_Line (Routine_Name);
      Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");

      --  L667 Y is 2D list num outputs x num classes
      Y := Iris_Data.Label_Values;
      Assert (Integer (Y.Length) = Num_Samples, Routine_Name &
                " invalid Y vector");

      --  L675 test for integer parameter
      --  Max_Leaf_Nodes is only used for Best First tree building
      Base_Decision_Tree.C_Init (theClassifier, "10", Criterion.Gini_Criteria);

      Classification_Fit (theClassifier, X, Y, No_Weights);
      Put_Line (Routine_Name & " Node_Count" & Count_Type'Image
                (theClassifier.Attributes.Decision_Tree.Nodes.Node_Count - 1));
--        Print_Tree ("The Tree", theClassifier);
      Put_Line ("----------------------------------------------");
      New_Line;

      Assert (Check_Min_Split (theClassifier, Min_Split),
              Routine_Name & "failed with Min_Split " &
                Integer'Image (Min_Split) & " less than 10");
      Put_Line (Routine_Name & " Min_Split integer test passed Min_Split: " &
                  Integer'Image (Min_Split));

      Graphviz_Exporter.C_Init
        (Exporter, theClassifier.Attributes.Decision_Tree);
      Graphviz_Exporter.Export_Graphviz
        (Exporter, theClassifier.Attributes.Decision_Tree,
         Output_File_Name => To_Unbounded_String ("integer_test.dot"));

      --  L684 test for float parameter
      Base_Decision_Tree.C_Init (theClassifier, "0.2", Criterion.Gini_Criteria);
      Classification_Fit (theClassifier, X, Y, No_Weights);

      Put_Line (Routine_Name & " Node_Count" & Count_Type'Image
                (theClassifier.Attributes.Decision_Tree.Nodes.Node_Count - 1));
--        Print_Tree ("The Tree", theClassifier);
      Put_Line ("----------------------------------------------");
      New_Line;

      Assert (Check_Min_Split (theClassifier, Min_Split),
              Routine_Name & "failed with Min_Split " &
                Integer'Image (Min_Split) & " less than 10");
      Put_Line (Routine_Name & " Min_Split float test passed Min_Split: " &
                  Integer'Image (Min_Split));

      Graphviz_Exporter.C_Init
        (Exporter, theClassifier.Attributes.Decision_Tree);
      Graphviz_Exporter.Export_Graphviz
        (Exporter, theClassifier.Attributes.Decision_Tree,
         Output_File_Name => To_Unbounded_String ("float_test.dot"));

   end Test_Min_Samples_Split;

   --  -------------------------------------------------------------------------

end Split_Tests;
