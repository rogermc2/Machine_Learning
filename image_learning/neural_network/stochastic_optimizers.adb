--  Based on scikit-learn/sklearn/neural_network/_stochastic_optimizers.py

with Maths;
with Utilities;

package body Stochastic_Optimizers is

   procedure C_Init (Self                  : out Adam_Optimizer;
                     Params                : Float_List_3D;
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
                     Params                : Float_List_3D;
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

   function Get_Updates (Self   : in out Adam_Optimizer;
                         Params : Parameters_Record) return Parameters_Record is
      use Maths.Float_Math_Functions;
      Learning_Rate    : Float;
      Coeff_Params_1D  : Float_List;
      Coeff_M_V        : Float;
      Coeff_Updates_1D : Float_List;
      Updates          : Parameters_Record;
   begin
      Self.Time_Step := Self.Time_Step + 1;
      Self.First_Moments.Clear;
      Self.Second_Moments.Clear;

      for m in Self.First_Moments.First_Index ..
        Self.First_Moments.Last_Index loop
         Coeff_Params_1D := Params.Coeff_Params.Element (m);
         Coeff_Updates_1D.Clear;
         for coeff in Coeff_Params_1D.First_Index ..
              Coeff_Params_1D.Last_Index loop
                Self.First_Moments.Append
                  (Float (m) * Self.Beta_1 +
                   (1.0 - Self.Beta_1) * Coeff_Updates_1D.Element (m));
                Self.Second_Moments.Append
                  (Float (m) * Self.Beta_2 +
                   (1.0 - Self.Beta_2) * Coeff_Updates_1D.Element (m) ** 2);
         end loop;

         --  Process Intercept_Param
         Self.First_Moments.Append
              (Float (m) * Self.Beta_1 +
               (1.0 - Self.Beta_1) * Params.Intercept_Params.Element (m));
         Self.Second_Moments.Append
           (Float (m) * Self.Beta_2 +
            (1.0 - Self.Beta_2) * Params.Intercept_Params.Element (m) ** 2);
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
   --  L169
   function Get_Updates
     (Self : in out SGD_Optimizer; Params : Parameters_Record)
      return Parameters_Record is
      use Float_List_Package;
      M_V              : Float;
      Coeff_Params_1D  : Float_List;
      Coeff_M_V        : Float;
      Coeff_Updates_1D : Float_List;
      Updates          : Parameters_Record;
   begin
      Self.Velocities.Clear;
      for index in Self.Velocities.First_Index ..
          Self.Velocities.Last_Index loop
         M_V := Self.Momentum * Self.Velocities (index);

         Coeff_Params_1D := Params.Coeff_Params.Element (index);
         Coeff_Updates_1D.Clear;
         for coeff in Coeff_Params_1D.First_Index ..
              Coeff_Params_1D.Last_Index loop
                Coeff_M_V :=
                  M_V -  Self.Learning_Rate * Coeff_Params_1D (coeff);
                Coeff_Updates_1D.Append (Coeff_M_V);
         end loop;
         Updates.Coeff_Params.Append (Coeff_Updates_1D);

         M_V := M_V -  Self.Learning_Rate * Params.Intercept_Params (index);
         Updates.Intercept_Params.Append (M_V);
         Self.Velocities.Append (M_V);
      end loop;

      return Updates;

   end Get_Updates;

   --  -------------------------------------------------------------------------
   --  L29
   procedure Update_Params (Self   : in out SGD_Optimizer;
                            Params : Parameters_Record;
                            Grads  : Float_List) is
      Updates : Parameters_Record;
   begin
      Updates := Get_Updates (Self, Params);
      for index in Updates.Coeff_Params.First_Index ..
        Updates.Coeff_Params.Last_Index loop
         null;
      end loop;

   end Update_Params;

   --  -------------------------------------------------------------------------

end Stochastic_Optimizers;
