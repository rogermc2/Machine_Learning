--  Based on Python 3.7 sklearn tree _classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)
with Utilities;

package body Decision_Tree_Classifer is

   procedure Build_Tree (Self : in out Classifier) is
      Criterion : Classifier_Criteria_Type := Self.Parameters.Criterion;
      Splitter : Splitter_Type := Self.Parameters.Splitter;
   begin
      if Self.Parameters.Max_Leaf_Nodes < 0 then
         null;
      else
         null;
      end if;
   end Build_Tree;

   procedure Check_Parameters is
   begin
      null;
   end Check_Parameters;

   --  Based on tree._classes.py BaseDecisionTree.Fit
   --  X :  a (n_samples, n_features) matrix of training samples
   --  Y :  a (n_samples, n_outputs) array of integer valued class labels
   --       for the training samples.
   --  Sample_Weight : array-like of shape (n_samples,), default=None
   function Fit (Self : in out Classifier;
--                   X    : Sample_Matrix;
--                   Y    : in out Integer_List;
                  XY_Data : ML_Types.Rows_Vector;
                  Sample_Weight : Float_Array;
                  Use_Weight   : Boolean := False;
                  Check_Input  : Boolean := True;
                  X_Idx_Sorted : State := None)
                 return Estimator.Estimator_Data is
      use Integer_Package;
      use Weight_Dictionary;
      use ML_Types;
      Num_Samples           : constant Positive := Positive (XY_Data.Length);
      --        Num_Samples           : Integer := X'length;
      aRow                  : Row_Data := XY_Data.Element (1);
      Num_Features          : constant Class_Range :=
                                Utilities.Number_Of_Features (XY_Data);
      Random_State          : Integer := Self.Parameters.Random_State;
      Expanded_Class_Weight : Float_List;
      theEstimator          : Estimator.Estimator_Data
        (Num_Samples, Positive (Num_Features));
      Y_Array               : Integer_Array (1 .. Integer (Num_Samples)) :=
                                Utilities.Label_Array (XY_Data);
      Y_Original            : Integer_Array_List;
      Classes_K             : Integer_List;
      Max_Leaf_Nodes        : Integer := -1;
   begin
      if Self.Parameters.CCP_Alpha < 0.0 then
         raise Value_Error with
         "Decision_Tree_Classifer.Fit CCP_Alpha must be greater than or equal to 0";
      end if;

      Self.Attributes.Num_Features := Num_Samples;
      Self.Attributes.Num_Outputs := Integer (Num_Samples);
      Self.Attributes.Classes.Clear;
      Self.Attributes.Num_Classes := 0;

   --  As Integer_List, indices are part of the returned list
      Classes_K := Unique (Y);
      Y.Clear;
      Y := Classes_K;

      if Self.Parameters.Class_Weight /= Empty_Map then
         Y_Original.Append (Y_Array);
         Expanded_Class_Weight := Compute_Sample_Weight (No_Weight, Y_Original);
      end if;
      if Self.Parameters.Max_Leaf_Nodes > 0 then
         Max_Leaf_Nodes := Self.Parameters.Max_Leaf_Nodes;
      end if;

      Check_Parameters;
      Build_Tree (Self);

      return theEstimator;
   end Fit;

   --  -------------------------------------------------------------------------

end Decision_Tree_Classifer;
