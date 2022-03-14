--  Based on scikit-learn/sklearn/neural_network/_base.py

with Maths;
with Utilities;

package body Base is

   function Binary_Log_Loss (Y_True, Y_Prob : Float_List) return Float is
      use Maths.Float_Math_Functions;
      use Float_Package;
      Y_P      : Float_List;
      Y_T      : Float_List;
      X_Log_Y1 : Float_List;
      X_Log_Y2 : Float_List;
      Sum1     : Float := 0.0;
      Sum2     : Float := 0.0;
   begin
      Check_Lengths ("Base.Log_Loss", Y_True, Y_Prob);
      for index in Y_Prob.First_Index .. Y_Prob.Last_Index loop
         Y_P.Append (1.0 - Y_Prob (index));
         Y_T.Append (1.0 - Y_True (index));
      end loop;

      --  xlogy = x*log(y) so that the result is 0 if x = 0
      for index in Y_Prob.First_Index .. Y_Prob.Last_Index loop
         X_Log_Y1.Append (Y_True (index) * Log (Y_Prob (index)));
         X_Log_Y2.Append (Y_T (index) * Log (Y_P (index)));
      end loop;

      for index in X_Log_Y1.First_Index .. X_Log_Y1.Last_Index loop
         Sum1 := Sum1 + X_Log_Y1 (index);
         Sum2 := Sum2 + X_Log_Y2 (index);
      end loop;

      return - (Sum1 + Sum2) / Float (Y_Prob.Length);

   end Binary_Log_Loss;

   --  -------------------------------------------------------------------------

   function Identity (Activation : Float_List) return Float_List is
   begin
      return Activation;
   end Identity;

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

   function Logistic_Sigmoid (X : Float) return Float is
      use Maths.Float_Math_Functions;
   begin
      return 1.0 / (1.0 + Exp (X));
   end Logistic_Sigmoid;

   --  -------------------------------------------------------------------------
   --  Log Loss is the negative average of the log of corrected predicted
   --  probabilities for each instance.
   function Log_Loss (Y_True, Y_Prob : Float_List) return Float is
      use Maths.Float_Math_Functions;
      use Float_Package;
      Y_P     : Float_List;
      Y_T     : Float_List;
      X_Log_Y : Float_List;
      Result  : Float := 0.0;
   begin
      Check_Lengths ("Base.Log_Loss", Y_True, Y_Prob);
      for index in Y_Prob.First_Index .. Y_Prob.Last_Index loop
         Y_P.Append (1.0 - Y_Prob (index));
         Y_T.Append (1.0 - Y_True (index));
      end loop;
      Y_P := Y_P & Y_Prob;
      Y_T := Y_T & Y_True;

      --  xlogy = x*log(y) so that the result is 0 if x = 0
      for index in Y_Prob.First_Index .. Y_Prob.Last_Index loop
         X_Log_Y.Append (-Y_True (index) * Log (Y_Prob (index)));
      end loop;

      for index in X_Log_Y.First_Index .. X_Log_Y.Last_Index loop
         Result := Result + X_Log_Y (index);
      end loop;

      return Result / Float (X_Log_Y.Length);

   end Log_Loss;

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

   function Relu (Activation : Float_List) return Float_List is
      Result : Float_List;
   begin
      for index in Activation.First_Index .. Activation.Last_Index loop
         Result.Append (Float'Max (0.0, Activation.Element (index)));
      end loop;

      return Result;
   end Relu;

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

   function Squared_Loss (Y_True, Y_Pred : Float_List) return Float is
   begin
      return Utilities.Mean (Y_True - Y_Pred) / 2.0;

   end Squared_Loss;

   --  -------------------------------------------------------------------------

end Base;
