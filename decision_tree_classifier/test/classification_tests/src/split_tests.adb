--  Based on scikit-learn/sklearn/tree/tests.test_tree.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Types;
with Classifier_Utilities;
with Criterion;
with Decision_Tree_Classification;
with ML_Types;
with Node_Splitter;
with Printing;
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
      Min_Samples_Split : constant ML_Types.Value_Record :=
                            aClassifier.Parameters.Min_Samples_Split;
      --        Min_Integer_Split_Rule : Positive;
      Min_Split_Rule    : Float;
      Num_Split         : Integer := Integer'Last;
      OK                : Boolean := True;

      procedure Check (Curs : Cursor) is
         Node : constant Tree_Node := Element (Curs);
      begin
         case Min_Samples_Split.Value_Kind is
         when Float_Type =>
            Min_Split_Rule := Min_Samples_Split.Float_Value;
         when Integer_Type =>
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
      use Classifier_Utilities;
      use Decision_Tree_Classification;
      use Printing;
      use Classifier_Types.Float_Package;
      Routine_Name              : constant String := "Split_Tests.Test_Min_Samples_Split ";
      Iris_Data                 : constant Data_Record := Load_Data ("src/iris.csv");
      Criteria                  : Criterion.Criterion_Class;
      Splitter                  : Node_Splitter.Splitter_Class;
      theClassifier             : Base_Decision_Tree.Classifier
        (Tree.Float_Type, Tree.Float_Type, Tree.Float_Type);
      X                         :  Value_Data_Lists_2D;
      --        Short         :  Value_Data_Lists_2D;
      --  Y: num outputs x num classes
      Y                         : Value_Data_Lists_2D;
      No_Weights                : Weights.Weight_List := Empty_Vector;
      Num_Samples               : Natural;
      Min_Split_Float_Samples   : ML_Types.Value_Record (Float_Type);
      Min_Split_Integer_Samples : ML_Types.Value_Record (Integer_Type);
      Min_Split                 : Natural;
   begin
      --  L666
      X := Iris_Data.Feature_Values;
      Num_Samples := Natural (X.Length);
      --        for index in 1 .. 10 loop
      --           Short.Append (X.Element (index));
      --        end loop;
      --        Printing.Print_Value_Data_Lists_2D (Routine_Name & ", X_Short", Short);

      Put_Line (Routine_Name & " Num_Samples" & Integer'Image (Num_Samples));
      Put_Line (Routine_Name);
      Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");

      --  L667 Y is 2D list num outputs x num classes
      Y := To_Value_2D_List (Iris_Data.Label_Values);
      Assert (Integer (Y.Length) = Num_Samples, Routine_Name &
                " invalid Y vector");

      --        for index in 1 .. 100 loop
      --           Short.Append (Y.Element (index));
      --        end loop;
      --        Printing.Print_Value_Data_Lists_2D (Routine_Name & ", Y_Short", Short);

      --  L675 test for integer parameter
      --  Max_Leaf_Nodes is only used for Best First tree building
      Min_Split_Integer_Samples.Integer_Value := 10;
      C_Init (theClassifier, Criteria, Splitter, Min_Split_Integer_Samples);

      Classification_Fit (theClassifier, X, Y, No_Weights);
      Put_Line (Routine_Name & " Node_Count" & Count_Type'Image
                (theClassifier.Attributes.Decision_Tree.Nodes.Node_Count - 1));
      Print_Tree ("The Tree", theClassifier);
      Put_Line ("----------------------------------------------");
      New_Line;

      Assert (Check_Min_Split (theClassifier, Min_Split),
              Routine_Name & "failed with Min_Split " &
                Integer'Image (Min_Split) & " less than 10");
      Put_Line (Routine_Name & " Min_Split integer test status: " &
                  Integer'Image (Min_Split));

      --  L684 test for float parameter
      Min_Split_Float_Samples.Float_Value := 0.2;
      C_Init (theClassifier, Criteria, Splitter, Min_Split_Float_Samples);
      Classification_Fit (theClassifier, X, Y, No_Weights);

      Put_Line (Routine_Name & " Node_Count" & Count_Type'Image
                (theClassifier.Attributes.Decision_Tree.Nodes.Node_Count - 1));
      Print_Tree ("The Tree", theClassifier);
      Put_Line ("----------------------------------------------");
      New_Line;

      Assert (Check_Min_Split (theClassifier, Min_Split),
              Routine_Name & "failed with Min_Split " &
                Integer'Image (Min_Split) & " less than 10");
      Put_Line (Routine_Name & " Min_Split float test status: " &
                  Integer'Image (Min_Split));

   end Test_Min_Samples_Split;

   --  -------------------------------------------------------------------------

end Split_Tests;
