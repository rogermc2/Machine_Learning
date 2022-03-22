--  Based on scikit-learn/sklearn/neural_network/_base.py

with Maths;
with Neural_Maths;

package body Base_Neural is

   --  -------------------------------------------------------------------------

   function Binary_Log_Loss (Y_True : Integer_List_2D; Y_Prob : Float_List_2D)
                             return Float is
      use Maths.Float_Math_Functions;
      use Float_Package;
      Y_P      : Float_List;
      Y_T      : Integer_List;
      YP_Float : Float_List;
      YT_Float : Float_List;
      X_Log_Y1 : Float_List;
      X_Log_Y2 : Float_List;
      Sum1     : Float := 0.0;
      Sum2     : Float := 0.0;
   begin
      for index in Y_Prob.First_Index .. Y_Prob.Last_Index loop
         Y_P := Y_Prob (index);
         Y_P.Replace_Element (1, 1.0 - Y_P (1));
         YP_Float.Append (Float (Y_P.Element (1)));
         Y_T := Y_True (index);
         Y_T.Replace_Element (1, 1 - Y_T (1));
         YT_Float.Append (Float (Y_T.Element (1)));
      end loop;

      --  xlogy = x*log(y) so that the result is 0 if x = 0
      for index in Y_Prob.First_Index .. Y_Prob.Last_Index loop
         Y_P := Y_Prob (index);
         Y_T := Y_True (index);
         X_Log_Y1.Append (YT_Float (index) * Log (Y_P (1)));
         X_Log_Y2.Append (Float (Y_T.Element (1)) * Log (YP_Float (index)));
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

   procedure Identity_Derivative (Z   : Float_List_2D;
                                  Del : in out Float_List_2D) is
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

   procedure Logistic_Derivative (Z   : Float_List_2D;
                                  Del : in out Float_List_2D) is
      List_Z : Float_List;
      List_Z2 : Float_List_2D;
   begin
      Del := Del * Z;
      --  List_Z = 1 - Z
      for index in Z.First_Index .. Z.Last_Index loop
         List_Z := Z (index);
         for index2 in List_Z.First_Index .. List_Z.Last_Index loop
            List_Z (index) := 1.0 - List_Z (index);
         end loop;
         List_Z2.Append (List_Z);
      end loop;
      Del := Del * List_Z2;

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
   function Log_Loss (Y_True : Integer_List_2D; Y_Prob : Float_List_2D)
                      return Float is
      use Maths.Float_Math_Functions;
      use Float_Package;
      Y_P      : Float_List;
      Y_T      : Integer_List;
      YP_Float : Float_List;
      YT_Float : Float_List;
      X_Log_Y  : Float_List;
      Result   : Float := 0.0;
   begin
      for index in Y_Prob.First_Index .. Y_Prob.Last_Index loop
         Y_P := Y_Prob (index);
         Y_P.Replace_Element (1, 1.0 - Y_P (1));
         YP_Float.Append (Float (Y_P.Element (1)));
         Y_T := Y_True (index);
         Y_T.Replace_Element (1, 1 - Y_T (1));
         YT_Float.Append (Float (Y_T.Element (1)));
      end loop;

      for index in Y_Prob.First_Index .. Y_Prob.Last_Index loop
         YP_Float := YP_Float & Y_Prob (index).Element (1);
         YT_Float := YT_Float & Float (Y_True.Element (index).Element (1));
      end loop;

      --  xlogy = x*log(y) so that the result is 0 if x = 0
      for index in Y_Prob.First_Index .. Y_Prob.Last_Index loop
         X_Log_Y.Append (-YT_Float (index) * Log (Y_Prob (index).Element (1)));
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

   procedure Relu_Derivative (Z : Float_List_2D; Del : in out Float_List_2D) is
      List_Z    : Float_List;
      List_Del  : Float_List;
   begin
      for index in Z.First_Index .. Z.Last_Index loop
         List_Z := Z (index);
         List_Del := Del (index);
         for index2 in Z.First_Index .. Z.Last_Index loop
            if List_Z (index2) = 0.0 then
               List_Del (index) := 0.0;
            end if;
         end loop;
         Del (index) := List_Del;
      end loop;

   end Relu_Derivative;

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

   function Squared_Loss (Y_True : Integer_List_2D; Y_Pred : Float_List_2D)
                           return Float is
      Y_P      : Float_List;
      YT_Int   : Integer;
      YT_Float : Float_List;
      YP_Float : Float_List;
   begin
      for index in Y_Pred.First_Index .. Y_Pred.Last_Index loop
         YT_Int := Y_True (index).Element (1);
         YT_Float.Append (Float (YT_Int));

         Y_P := Y_Pred (index);
         YP_Float.Append (Y_P.Element (1));
      end loop;

      return Neural_Maths.Mean ((YT_Float - YP_Float) ** 2) / 2.0;

   end Squared_Loss;

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

   procedure Tanh_Derivative (Z : Float_List_2D; Del : in out Float_List_2D) is
      List_Z : Float_List;
      List_Z2 : Float_List_2D;
   begin
      Del := Del * Z;
      --  List_Z = 1 - Z ** 2
      for index in Z.First_Index .. Z.Last_Index loop
         List_Z := Z (index);
         for index2 in List_Z.First_Index .. List_Z.Last_Index loop
            List_Z (index) := 1.0 - List_Z (index) ** 2;
         end loop;
         List_Z2.Append (List_Z);
      end loop;

      Del := Del * List_Z2;

   end Tanh_Derivative;

   --  -------------------------------------------------------------------------

end Base_Neural;
