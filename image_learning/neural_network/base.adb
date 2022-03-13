--  Based on scikit-learn/sklearn/neural_network/_base.py

with Maths;

package body Base is

   function Identity (Activation : Float_List) return Float_List is
   begin
      return Activation;
   end Identity;

   --  -------------------------------------------------------------------------

   function Logistic (Activation : Float_List) return Float_List is
   begin
      return Activation;
   end Logistic;

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

end Base;
