--  Based on scikit-learn/sklearn/tree _tree.pyx class Tree

with Maths;

package body Tree is

   --     function Validate_X_Predict (Self         : Validation.Attribute_List;
   --                                   X           : Sample_Matrix;
   --                                   Check_Input : Boolean := True)
   --                                  return Sample_Matrix;

   --  -------------------------------------------------------------------------

   procedure Init (Self    : out Tree_Class; Num_Features : Positive;
                   Classes : ML_Types.Value_Data_List) is
   begin
      Self.Num_Features := Num_Features;
      Self.Classes.Clear;
      Self.Classes := Classes;
      Self.Attributes.Node_Count := 0;
      Self.Attributes.Max_Depth := 0;
      Self.Attributes.Capacity := 0;
   end Init;

   --  -------------------------------------------------------------------------
   --  For a classification model, the predicted class for each sample in X is
   --  returned. For a regression model, the predicted value based on X is
   --   returned.
   --     function Predict (Self : Validation.Attribute_List;
   function Predict (X : Sample_Matrix; Check_Input : Boolean := True)
                     return Probabilities_List is
      --        N_Samples       : constant Integer := X'Length;
      Probabilities   : Probabilities_List;
   begin
      --        Validation.Check_Is_Fitted (Self);
      --        X := Validate_X_Predict (Self, X, Check_Input);

      return Probabilities;
   end Predict;

   --  -------------------------------------------------------------------------

   --  Predict class log-probabilities of the input samples X.
   --     function Predict_Log_Probability (Self : Validation.Attribute_List;
   function Predict_Log_Probability (X    : Sample_Matrix) return Probabilities_List is
      use Maths.Float_Math_Functions;
      --        Prob   : constant Probability_Array := Predict_Probability (Self, X);
      --        Result : Probability_Array (1 .. Prob'Length) := (others => 0.0);
      --        Probabilities     : constant Probabilities_List := Predict_Probability (Self, X);
      Probabilities     : constant Probabilities_List := Predict_Probability (X);
      Log_Probabilities : Probabilities_List;
   begin
      for index in Probabilities.First_Index .. Probabilities.Last_Index loop
         Log_Probabilities.Append (Log (Probabilities.Element (index)));
      end loop;
      return Log_Probabilities;
   end Predict_Log_Probability;

   --  -------------------------------------------------------------------------
   --  Predict class probabilities of the input samples X.
   --  The predicted class probability is the fraction of samples of the same
   --   class in a leaf.
   --     function Predict_Probability (Self        : Validation.Attribute_List;
   function Predict_Probability (X           : Sample_Matrix;
                                 Check_Input : Boolean := True)
                                 return Probabilities_List is
      --        Result : Probability_Array (1 .. Integer (Self.Length)) := (others => 0.0);
      Probabilities   : Probabilities_List;
   begin
      --        Validation.Check_Is_Fitted (Self);
      --        X := Validate_X_Predict (Self, X, Check_Input)

      return Probabilities;
   end Predict_Probability;

   --  -------------------------------------------------------------------------

   procedure Resize (Self : in out Tree_Data; Capacity : Positive) is
   begin
      null;
   end Resize;

   --  -------------------------------------------------------------------------

   --     function Validate_X_Predict (Self         : Validation.Attribute_List;
   --                                   X           : Sample_Matrix;
   --                                   Check_Input : Boolean := True)
   --                                  return Sample_Matrix is
   --        Self_Length  : constant Integer := Integer (Self.Length);
   --        N_Features : constant Integer := X'Length (2);
   --     begin
   --        if N_Features /= Self_Length then
   --           raise Value_Error with
   --             "Number of features of the model must match the input." &
   --             " Model n_features is " & Integer'Image (Self_Length) &
   --             "and input n_features is " & Integer'Image (N_Features);
   --        end if;
   --        return X;
   --     end Validate_X_Predict;

   --  -------------------------------------------------------------------------

end Tree;
