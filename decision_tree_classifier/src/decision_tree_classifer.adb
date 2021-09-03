--  Based on scikit-learn/sklearn/tree/_classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)

with Classifier_Types;
with Encode_Utils;

package body Decision_Tree_Classifer is

   --  -------------------------------------------------------------------------

   procedure Build_Tree (Self : in out Classifier) is
--        Criterion : Classifier_Criteria_Type := Self.Parameters.Criterion;
--        Splitter  : Splitter_Type := Self.Parameters.Splitter;
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
   function Fit (aClassifier   : in out Classifier;
                 --                   X    : Sample_Matrix;
                 --                   Y    : in out Integer_List;
                 X             : ML_Types.List_Of_Value_Data_Lists;
                 Y             : in out ML_Types.Value_Data_List;
                 Sample_Weight : Float_Array;
                 Use_Weight    : Boolean := False;
                 Check_Input   : Boolean := True;
                 X_Idx_Sorted  : State := None)
                 return Estimator.Estimator_Data is
      use Ada.Containers;
      use ML_Types;
      use Classifier_Types.Integer_Package;
      use Value_Data_Package;
      use Weights.Weight_Dictionary;
      Num_Samples           : constant Positive := Positive (X.Length);
      Num_Outputs           : constant Positive := Positive (Y.Length);
      Features              : Value_Data_List := X.First_Element;
      Num_Features          : constant Class_Range :=
                                Class_Range (Features.Length);
      Random_State          : Integer := aClassifier.Parameters.Random_State;
      Expanded_Class_Weight : Classifier_Types.Float_List;
      theEstimator          : Estimator.Estimator_Data
        (Num_Samples, Positive (Num_Features));
      Y_Original            : List_Of_Value_Data_Lists;
      Y_Encoded             : Value_Data_List;
--        Encode_Value          : Value_Record (Float_Type);
--        Classes               : Integer_List;
      Classes_K             : Value_Data_List;
      K_Length              : Natural := 0;
--        Y_Cursor              : Value_Data_Package.Cursor;
      K_Cursor              : Value_Data_Package.Cursor;
      Max_Leaf_Nodes        : Integer := -1;
   begin
      if aClassifier.Parameters.CCP_Alpha < 0.0 then
         raise Value_Error with
           "Decision_Tree_Classifer.Fit CCP_Alpha must be greater than or equal to 0";
      end if;

      aClassifier.Attributes.Num_Features := Num_Samples;
      aClassifier.Attributes.Num_Outputs := Integer (Num_Samples);
      aClassifier.Attributes.Classes.Clear;
      aClassifier.Attributes.Num_Classes.Clear;

      if Positive (Y.Length) /= Num_Samples then
         raise Value_Error with
           "Decision_Tree_Classifer.Fit Number of labels =" &
           Count_Type'Image (Y.Length) & " does not match number of samples ="
           & Integer'Image (Num_Samples);
      end if;

      for k in 1 .. Num_Outputs loop
         Classes_K := Encode_Utils.Unique (Y);
         Y_Encoded := Classes_K;
         K_Length := Natural (Classes_K.Length);
         aClassifier.Attributes.Classes.Append (Classes_K);
         aClassifier.Attributes.Num_Classes.Append (K_Length);
      end loop;
      Y.Clear;

      K_Cursor := Classes_K.First;
      while  Has_Element (K_Cursor) loop
            Y.Append (Element (K_Cursor));
            Next (K_Cursor);
      end loop;

      Y_Original.Clear;
      if aClassifier.Parameters.Class_Weight /= Empty_Map then
         Y_Original.Append (Y);
         Expanded_Class_Weight :=
           Weights.Compute_Sample_Weight (Weights.No_Weight, Y_Original);
      end if;
      if aClassifier.Parameters.Max_Leaf_Nodes > 0 then
         Max_Leaf_Nodes := aClassifier.Parameters.Max_Leaf_Nodes;
      end if;

      Check_Parameters;
      Build_Tree (aClassifier);

      return theEstimator;
   end Fit;

   --  -------------------------------------------------------------------------

end Decision_Tree_Classifer;
