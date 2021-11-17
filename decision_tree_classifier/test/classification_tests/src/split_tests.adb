
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

   procedure Test_Min_Samples_Split is
      use Ada.Containers;
      use Classifier_Utilities;
      use Decision_Tree_Classification;
      use Printing;
      use Classifier_Types.Float_Package;
      Routine_Name  : constant String := "Split_Tests.Test_Min_Samples_Split";
      Iris_Data     : constant Data_Record := Load_Data ("src/iris.csv");
      Criteria      : Criterion.Criterion_Class;
      Splitter      : Node_Splitter.Splitter_Class;
      theClassifier : Base_Decision_Tree.Classifier
        (Tree.Float_Type, Tree.Float_Type, Tree.Float_Type);
      X             :  Value_Data_Lists_2D;
--        X_Short       :  Value_Data_Lists_2D;
      --  Y: num outputs x num classes
      Y             : Value_Data_Lists_2D;
      No_Weights    : Weights.Weight_List := Empty_Vector;
      Num_Samples   : Natural;
      Prediction    : ML_Types.Value_Data_Lists_2D;
   begin
      C_Init (theClassifier, Criteria, Splitter, Min_Split_Samples => 10,
              Max_Leaf_Nodes => 100);
      --  L1689
      X := Iris_Data.Feature_Values;
      Num_Samples := Natural (X.Length);
--        for index in 1 .. 10 loop
--           X_Short.Append (X.Element (index));
--        end loop;
--        Printing.Print_Value_Data_Lists_2D (Routine_Name & ", X_Short", X_Short);
      Put_Line (Routine_Name & ", Num_Samples" & Integer'Image (Num_Samples));
      Put_Line (Routine_Name);
      Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");

      --  Y is 2D list num outputs x num classes
      Y := To_Value_2D_List (Iris_Data.Label_Values);
      Assert (Integer (Y.Length) = Num_Samples, Routine_Name &
                " invalid Y vector");
      --  L1695
      Classification_Fit (theClassifier, X, Y, No_Weights);
      Put_Line (Routine_Name & ", Node_Count" & Count_Type'Image
                (theClassifier.Attributes.Decision_Tree.Nodes.Node_Count - 1));
      Print_Tree ("The Tree", theClassifier);
      Put_Line ("----------------------------------------------");
      New_Line;
      Put_Line (Routine_Name & ", Number of samples:" &
                  Integer'Image (Count_Samples (theClassifier)));
      Prediction := Base_Decision_Tree.Predict (theClassifier, X);
      Put_Line (Routine_Name & ", Prediction size" &
                  Count_Type'Image (Prediction.Length));
--        Print_Value_Data_Lists_2D
--          (Routine_Name & " Predictions", Prediction);

   end Test_Min_Samples_Split;

   --  -------------------------------------------------------------------------

end Split_Tests;
