--  Based on Python 3.7 sklearn tree _classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)
with Utilities;

package body Decision_Tree_Classifer is

   --  -------------------------------------------------------------------------

   procedure Build_Tree (Self : in out Classifier) is
      Criterion : Classifier_Criteria_Type := Self.Parameters.Criterion;
      Splitter  : Splitter_Type := Self.Parameters.Splitter;
   begin
      if Self.Parameters.Max_Leaf_Nodes < 0 then
         null;
      else
         null;
      end if;
   end Build_Tree;

   --  -------------------------------------------------------------------------

   procedure Check_Parameters is
   begin
      null;
   end Check_Parameters;

   --  -------------------------------------------------------------------------
   --  The Fit function adjusts weights according to data values so that
   --  better accuracy can be achieved
   --  Based on tree._classes.py BaseDecisionTree.Fit
   --  X :  a (n_samples, n_features) matrix of training samples
   --  Y :  a (n_samples, n_outputs) array of integer valued class labels
   --       for the training samples.
   --  Sample_Weight : array-like of shape (n_samples,), default=None
   function Fit (Self          : in out Classifier;
                 --                   X    : Sample_Matrix;
                 --                   Y    : in out Integer_List;
                 X             : ML_Types.Features_Data_List;
                 Y             : in out ML_Types.Value_Data_List;
                 Sample_Weight : Float_Array;
                 Use_Weight    : Boolean := False;
                 Check_Input   : Boolean := True;
                 X_Idx_Sorted  : State := None)
                 return Estimator.Estimator_Data is
      use Integer_Package;
      use Weight_Dictionary;
      use ML_Types;
      Num_Samples           : constant Positive := Positive (X.Length);
      Num_Outputs           : constant Positive := Positive (Y.Length);
      Features              : Value_Data_List := X.First_Element;
      Num_Features          : constant Class_Range :=
                                Class_Range (Features.Length);
      Random_State          : Integer := Self.Parameters.Random_State;
      Expanded_Class_Weight : Float_List;
      theEstimator          : Estimator.Estimator_Data
        (Num_Samples, Positive (Num_Features));
      Y_Original            : Value_Data_List := Y;
      Classes               : Features_Data_List;
      Classes_K             : Value_Data_List;
      K_Index               : Positive;
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

      for k in 1 .. Num_Outputs loop
         Classes_K := Unique_Value (Y);
      end loop;
      Y.Clear;
      K_Index := Classes_K.First_Index;
      for index in Y'Range loop
            Y (index).Integer_Value := Classes_K.Element (K_Index);
            if index /= Y'Last then
                K_Index := K_Index + 1;
            end if;
      end loop;

      if Self.Parameters.Class_Weight /= Empty_Map then
         Y_Original.Append (Y);
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
