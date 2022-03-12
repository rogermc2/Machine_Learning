--  Based on scikit-learn/sklearn/neural_network/_stochastic_optimizers.py

with Maths;
with Utilities;

package body Stochastic_Optimizers is

   procedure C_Init (Self                  : out Adam_Optimizer;
                     Params                : Float_List;
                     Initial_Learning_Rate : Float := 0.1;
                     Beta_1                : Float := 0.9;
                     Beta_2                : Float := 0.999;
                     Epsilon               : Float) is
   begin
      Self.Params := Params;
      Self.Initial_Learning_Rate := Initial_Learning_Rate;
      Self.Beta_1 := Beta_1;
      Self.Beta_2 := Beta_2;
      Self.Epsilon := Epsilon;
   end C_Init;

   --  -------------------------------------------------------------------------

   procedure C_Init (Self                  : out SGD_Optimizer;
                     Params                : Float_List;
                     Initial_Learning_Rate : Float := 0.1;
                     LR_Schedule           : LR_Schedule_Type := Constant_LR_Schedule;
                     Momentum              : Float := 0.9;
                     Use_Nesterov          : Boolean := True;
                     Power_T               : Float := 0.5) is
   begin
      Self.Params := Params;
      Self.Initial_Learning_Rate := Initial_Learning_Rate;
      Self.LR_Schedule := LR_Schedule;
      Self.Momentum := Momentum;
      Self.Use_Nesterov := Use_Nesterov;
      Self.Power_T := Power_T;
   end C_Init;

   --  -------------------------------------------------------------------------

   function Get_Updates (Self  : in out Adam_Optimizer;
                         Grads : Float_List) return Float_List is
      use Utilities;
      use Maths.Float_Math_Functions;
      Zip_Moments_Grads  : Float_Zip_List :=
                             Zip (Self.First_Moments, Grads);
      Grad_Items         : Float_Zip_Item;
      Learning_Rate      : Float;
      Updates            : Float_List;
   begin
      Self.Time_Step := Self.Time_Step + 1;

      for m in Zip_Moments_Grads.First_Index ..
        Zip_Moments_Grads.Last_Index loop
         Grad_Items := Zip_Moments_Grads.Element (m);
         Self.First_Moments.Append
           (Float (m) * Self.Beta_1 + Grad_Items.Float_1 * (1.0 - Self.Beta_1));
         Self.First_Moments.Append
           (Float (m) * Self.Beta_1 + Grad_Items.Float_2 * (1.0 - Self.Beta_1));
      end loop;

      Zip_Moments_Grads := Zip (Self.Second_Moments, Grads);
      for v in Zip_Moments_Grads.First_Index ..
        Zip_Moments_Grads.Last_Index loop
         Grad_Items := Zip_Moments_Grads.Element (v);
         Self.First_Moments.Append
           (Float (v) * Self.Beta_2 + Grad_Items.Float_1 ** 2 * (1.0 - Self.Beta_1));
         Self.First_Moments.Append
           (Float (v) * Self.Beta_2 + Grad_Items.Float_2 ** 2 * (1.0 - Self.Beta_1));
      end loop;

      Self.Learning_Rate := Sqrt
        (1.0 - Self.Beta_2 ** Self.Time_Step) * Self.Initial_Learning_Rate /
        (1.0 - Self.Beta_1 ** Self.Time_Step);

      Zip_Moments_Grads := Zip (Self.First_Moments, Self.Second_Moments);
      for m in Zip_Moments_Grads.First_Index ..
        Zip_Moments_Grads.Last_Index loop
         Learning_Rate := -Float (m) * Self.Learning_Rate;
         Grad_Items := Zip_Moments_Grads.Element (m);
         Updates.Append
           (Learning_Rate / (Sqrt (Grad_Items.Float_1) + Self.Epsilon));
         Updates.Append
           (Learning_Rate / (Sqrt (Grad_Items.Float_2) + Self.Epsilon));
      end loop;

      return Updates;

   end Get_Updates;

   --  -------------------------------------------------------------------------

   function Get_Updates (Self  : in out SGD_Optimizer;
                         Grads : Float_List) return Float_List is
      use Utilities;
      Zip_Velocities_Grads : constant Float_Zip_List :=
                               Zip (Self.Velocities, Grads);
      Velocities           : Float_Zip_Item;
      M_V                  : Float;
      Updates              : Float_List;
   begin

      for velocity in Zip_Velocities_Grads.First_Index ..
        Zip_Velocities_Grads.Last_Index loop
         Velocities := Zip_Velocities_Grads.Element (velocity);
         M_V := Self.Momentum * Float (velocity);
         Updates.Append (M_V - Velocities.Float_1 * Self.Learning_Rate);
         Updates.Append (M_V - Velocities.Float_2 * Self.Learning_Rate);
      end loop;

      Self.Velocities := Updates;

      return Updates;

   end Get_Updates;

   --  -------------------------------------------------------------------------

end Stochastic_Optimizers;
