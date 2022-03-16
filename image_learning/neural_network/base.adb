--  Based on scikit-learn/sklearn/neural_network/_base.py

with Maths;
with Utilities;

package body Base is

   --  -------------------------------------------------------------------------

   function Binary_Log_Loss (Y_True : Integer_List; Y_Prob : Float_List)
                             return Float is
      use Maths.Float_Math_Functions;
      use Float_Package;
      Y_P      : Float_List;
      Y_T      : Float_List;
      YT_Float : Float_List;
      YT_Int   : Integer;
      X_Log_Y1 : Float_List;
      X_Log_Y2 : Float_List;
      Sum1     : Float := 0.0;
      Sum2     : Float := 0.0;
   begin
      Check_Lengths ("Base.Log_Loss", Y_True, Y_Prob);
      for index in Y_Prob.First_Index .. Y_Prob.Last_Index loop
         Y_P.Append (1.0 - Y_Prob (index));
         YT_Int := Y_True (index);
         Y_T.Append (Float (1 - YT_Int));
         YT_Float.Append (Float (YT_Int));
      end loop;

      --  xlogy = x*log(y) so that the result is 0 if x = 0
      for index in Y_Prob.First_Index .. Y_Prob.Last_Index loop
         X_Log_Y1.Append (YT_Float (index) * Log (Y_Prob (index)));
         X_Log_Y2.Append (Y_T (index) * Log (Y_P (index)));
      end loop;

      for index in X_Log_Y1.First_Index .. X_Log_Y1.Last_Index loop
         Sum1 := Sum1 + X_Log_Y1 (index);
         Sum2 := Sum2 + X_Log_Y2 (index);
      end loop;

      return - (Sum1 + Sum2) / Float (Y_Prob.Length);

   end Binary_Log_Loss;

   --  -------------------------------------------------------------------------
   --  L498  abstract _estimate_log_prob;
   --  L513 _estimate_log_prob_resp computes the log-probabilities per
   --   component for each sample.
   function Estimate_Log_Prob (X : IL_Types.Float_List_2D) return Float is
      Result : Float;
   begin

      return Utilities.Log_Sum_Exponent (X);

   end Estimate_Log_Prob;

   --  -------------------------------------------------------------------------

   --  L513 _estimate_log_prob_resp computes the log-probabilities per
   --   component for each sample.
   function Estimate_Log_Prob_Resp (X : IL_Types.Float_List_2D) return Float is
      Result : Float;
   begin

      return Result;

   end Estimate_Log_Prob_Resp;

   --  -------------------------------------------------------------------------

   function Estimate_Weighted_Log_Prob (X : IL_Types.Float_List_2D)
                                        return Float is
      Result : Float;
   begin

      return Utilities.Log_Sum_Exponent (X);

   end Estimate_Weighted_Log_Prob;

   --  -------------------------------------------------------------------------

   function Identity (Activation : Float_List) return Float_List is
   begin
      return Activation;
   end Identity;

   --  -------------------------------------------------------------------------

   procedure Identity_Derivative (Z : Float_List; Del : in out Float_List) is
   begin
      null;
   end Identity_Derivative;

   --  -------------------------------------------------------------------------

   function Logistic (Activation : Float_List) return Float_List is
      use Maths.Float_Math_Functions;
      Result : Float_List;
   begin
      for index in Activation.First_Index .. Activation.Last_Index loop
         Result.Append (1.0 / (1.0 + Exp (Activation.Element (index))));
      end loop;

      return Result;
   end Logistic;

   --  -------------------------------------------------------------------------

   procedure Logistic_Derivative (Z : Float_List; Del : in out Float_List) is
      List_Z : Float_List;
   begin
      Del := Del * Z;
      --  List_Z = 1 - Z
      for index in Z.First_Index .. Z.Last_Index loop
         List_Z (index) := 1.0 - Z (index);
      end loop;
      Del := Del * List_Z;

   end Logistic_Derivative;

   --  -------------------------------------------------------------------------

   function Logistic_Sigmoid (X : Float) return Float is
      use Maths.Float_Math_Functions;
   begin
      return 1.0 / (1.0 + Exp (X));
   end Logistic_Sigmoid;

   --  -------------------------------------------------------------------------
   --  Log Loss is the negative average of the log of corrected predicted
   --  probabilities for each instance.
   function Log_Loss (Y_True : Integer_List; Y_Prob : Float_List)
                      return Float is
      use Maths.Float_Math_Functions;
      use Float_Package;
      Y_P      : Float_List;
      Y_T      : Float_List;
      YT_Float : Float_List;
      YT_Int   : Integer;
      X_Log_Y  : Float_List;
      Result   : Float := 0.0;
   begin
      Check_Lengths ("Base.Log_Loss", Y_True, Y_Prob);
      for index in Y_Prob.First_Index .. Y_Prob.Last_Index loop
         Y_P.Append (1.0 - Y_Prob (index));
         YT_Int := Y_True (index);
         Y_T.Append (Float (1 - YT_Int));
         YT_Float.Append (Float (YT_Int));
      end loop;

      Y_P := Y_P & Y_Prob;
      Y_T := Y_T & YT_Float;

      --  xlogy = x*log(y) so that the result is 0 if x = 0
      for index in Y_Prob.First_Index .. Y_Prob.Last_Index loop
         X_Log_Y.Append (-YT_Float (index) * Log (Y_Prob (index)));
      end loop;

      for index in X_Log_Y.First_Index .. X_Log_Y.Last_Index loop
         Result := Result + X_Log_Y (index);
      end loop;

      return Result / Float (X_Log_Y.Length);

   end Log_Loss;

   --  -------------------------------------------------------------------------

   function Relu (Activation : Float_List) return Float_List is
      Result : Float_List;
   begin
      for index in Activation.First_Index .. Activation.Last_Index loop
         Result.Append (Float'Max (0.0, Activation.Element (index)));
      end loop;

      return Result;
   end Relu;

   --  -------------------------------------------------------------------------

   procedure Relu_Derivative (Z : Float_List; Del : in out Float_List) is
   begin
         for index in Z.First_Index .. Z.Last_Index loop
            if Z (index) = 0.0 then
               Del (index) := 0.0;
            end if;
         end loop;

   end Relu_Derivative;

   --  -------------------------------------------------------------------------
   --  scikit-learn/sklearn/mixture/_base.py/score
   --  L356
   function Score (X    : IL_Types.Float_List_2D;
                   Y    : IL_Types.Integer_List) return Float is
   begin
      pragma Unreferenced (Y);
      return Score_Samples (X);

   end Score;

   --  -------------------------------------------------------------------------
   --  scikit-learn/sklearn/mixture/_base.py/score_samples
   --  L337
   function Score_Samples (X : IL_Types.Float_List_2D) return Float is
      Result : Float;
   begin

      return Estimate_Weighted_Log_Prob (X);

   end Score_Samples;

   --  -------------------------------------------------------------------------

   function Softmax (Activation : Float_List) return Float_List is
      use Maths.Float_Math_Functions;
      Exp_Sum : Float := 0.0;
      Result  : Float_List;
   begin
      for index in Activation.First_Index .. Activation.Last_Index loop
         Exp_Sum := Exp_Sum + Exp (Activation.Element (index));
      end loop;
      for index in Activation.First_Index .. Activation.Last_Index loop
         Result.Append  (Exp (Activation.Element (index) / Exp_Sum));
      end loop;

      return Result;
   end Softmax;

   --  -------------------------------------------------------------------------

   function Squared_Error (Y_True : Integer_List; Y_Pred : Float_List)
                           return Float is
      YT_Int   : Integer;
      YT_Float : Float_List;
   begin
      Check_Lengths ("Base.Squared_Error", Y_True, Y_Pred);
      for index in Y_Pred.First_Index .. Y_Pred.Last_Index loop
         YT_Int := Y_True (index);
         YT_Float.Append (Float (YT_Int));
      end loop;

      return Utilities.Mean (YT_Float - Y_Pred) / 2.0;

   end Squared_Error;

   --  -------------------------------------------------------------------------

   function Tanh (Activation : Float_List) return Float_List is
      use Maths.Float_Math_Functions;
      Result : Float_List;
   begin
      for index in Activation.First_Index .. Activation.Last_Index loop
         Result.Append (Tanh (Activation.Element (index)));
      end loop;

      return Result;
   end Tanh;

   --  -------------------------------------------------------------------------

   procedure Tanh_Derivative (Z : Float_List; Del : in out Float_List) is
      List_Z : Float_List;
   begin
      --  List_Z = 1 - Z ** 2
      for index in Z.First_Index .. Z.Last_Index loop
         List_Z (index) := 1.0 - Z (index) ** 2;
      end loop;

      Del := Del * List_Z;

   end Tanh_Derivative;

   --  -------------------------------------------------------------------------

end Base;
