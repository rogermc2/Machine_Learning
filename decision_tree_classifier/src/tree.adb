
with Maths;

package body Tree is

   function Validate_X_Predict (Self         : Validation.Attribute_List;
                                 X           : Sample_Matrix;
                                 Check_Input : Boolean := True)
                                return Sample_Matrix;

   --  -------------------------------------------------------------------------
   --  For a classification model, the predicted class for each sample in X is
   --  returned. For a regression model, the predicted value based on X is
   --   returned.
   function Predict (Self : Validation.Attribute_List;
                     X    : Sample_Matrix; Check_Input : Boolean := True)
                     return Probabilities_List is
      use Ada.Containers;
      N_Samples       : constant Integer := X'Length;
      Probabilities   : Probabilities_List;
   begin
--        Validation.Check_Is_Fitted (Self);
--        X := Validate_X_Predict (Self, X, Check_Input);
      if Self.Length = 1 then
         null;
      else
         null;
      end if;

      return Probabilities;
   end Predict;

   --  -------------------------------------------------------------------------

   --  Predict class log-probabilities of the input samples X.
   function Predict_Log_Probability (Self : Validation.Attribute_List;
                                     X    : Sample_Matrix) return Probabilities_List is
      use Maths.Float_Math_Functions;
--        Prob   : constant Probability_Array := Predict_Probability (Self, X);
--        Result : Probability_Array (1 .. Prob'Length) := (others => 0.0);
      Probabilities     : constant Probabilities_List := Predict_Probability (Self, X);
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
   function Predict_Probability (Self        : Validation.Attribute_List;
                                 X           : Sample_Matrix;
                                 Check_Input : Boolean := True)
                                 return Probabilities_List is
      use Ada.Containers;
--        Result : Probability_Array (1 .. Integer (Self.Length)) := (others => 0.0);
      Probabilities   : Probabilities_List;
   begin
--        Validation.Check_Is_Fitted (Self);
--        X := Validate_X_Predict (Self, X, Check_Input);
      if Self.Length = 1 then
         null;
      else
         null;
      end if;

      return Probabilities;
   end Predict_Probability;

   --  -------------------------------------------------------------------------

   function Validate_X_Predict (Self         : Validation.Attribute_List;
                                 X           : Sample_Matrix;
                                 Check_Input : Boolean := True)
                                return Sample_Matrix is
      Self_Length  : constant Integer := Integer (Self.Length);
      N_Features : constant Integer := X'Length (2);
   begin
      if N_Features /= Self_Length then
         raise Value_Error with
           "Number of features of the model must match the input." &
           " Model n_features is " & Integer'Image (Self_Length) &
           "and input n_features is " & Integer'Image (N_Features);
      end if;
      return X;
   end Validate_X_Predict;

   --  -------------------------------------------------------------------------

end Tree;
