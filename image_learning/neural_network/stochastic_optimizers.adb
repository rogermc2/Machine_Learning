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

   procedure Get_Updates
      (Self             : in out Adam_Optimizer; Coeff_Params : Float_List_2D;
      Intercept_Params  : Float_List; Coeff_Updates : out Float_List_2D;
      Intercept_Updates : out Float_List) is
      use Maths.Float_Math_Functions;
      Learning_Rate    : Float;
      Coeff_Params_1D  : Float_List;
      Coeff_M_V        : Float;
      Coeff_Updates_1D : Float_List;
      Updates          : Float_List;
   begin
      Self.Time_Step := Self.Time_Step + 1;
      Self.First_Moments.Clear;
      Self.Second_Moments.Clear;
      Coeff_Updates.Clear;
      Intercept_Updates.Clear;

      for m in Self.First_Moments.First_Index ..
        Self.First_Moments.Last_Index loop
         Coeff_Params_1D := Coeff_Params.Element (m);
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

         Self.First_Moments.Append
              (Float (m) * Self.Beta_1 +
               (1.0 - Self.Beta_1) * Intercept_Params.Element (m));
         Self.Second_Moments.Append
           (Float (m) * Self.Beta_2 +
            (1.0 - Self.Beta_2) * Intercept_Params.Element (m) ** 2);
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

   end Get_Updates;

   --  -------------------------------------------------------------------------
   --  L169
   procedure Get_Updates
     (Self              : in out SGD_Optimizer; Coeff_Params : Float_List_2D;
      Intercept_Params  : Float_List; Coeff_Updates : out Float_List_2D;
      Intercept_Updates : out Float_List) is
      use Float_List_Package;
      M_V              : Float;
      Coeff_Params_1D  : Float_List;
      Coeff_M_V        : Float;
      Coeff_Updates_1D : Float_List;
   begin
      Coeff_Updates.Clear;
      Intercept_Updates.Clear;
      Self.Velocities.Clear;
      for index in Self.Velocities.First_Index ..
          Self.Velocities.Last_Index loop
         M_V := Self.Momentum * Self.Velocities (index);

         Coeff_Params_1D := Coeff_Params.Element (index);
         Coeff_Updates_1D.Clear;
         for coeff in Coeff_Params_1D.First_Index ..
              Coeff_Params_1D.Last_Index loop
                Coeff_M_V :=
                  M_V -  Self.Learning_Rate * Coeff_Params_1D (coeff);
                Coeff_Updates_1D.Append (Coeff_M_V);
         end loop;
         Coeff_Updates.Append (Coeff_Updates_1D);

         M_V := M_V -  Self.Learning_Rate * Intercept_Params (index);
         Intercept_Updates.Append (M_V);
         Self.Velocities.Append (M_V);
      end loop;

   end Get_Updates;

   --  -------------------------------------------------------------------------
   --  L29
   procedure Update_Params (Self         : in out SGD_Optimizer;
                            Coeff_Params : Float_List_3D;
                            Intercept_Params : Float_List_2D;
                            Grads : Float_List) is
      Updates : Float_List;
   begin
      Get_Updates (Self, Coeff_Params, Intercept_Params, Coeff_Updates,
                   Intercept_Updates);
      for index in Updates.First_Index .. Updates.Last_Index loop
         null;
      end loop;

   end Update_Params;

   --  -------------------------------------------------------------------------

end Stochastic_Optimizers;
