--  Based on scikit-learn/sklearn/neural_network/_stochastic_optimizers.py

with Ada.Containers;

with Maths;

package body Stochastic_Optimizers is

   procedure C_Init (Self                  : out Adam_Optimizer;
                     --  Coeff_Params: laysers x features x values
                     Coeff_Params          : Float_List_3D;
                     --  Intercept_Params: laysers x values
                     Intercept_Params      : Float_List_2D;
                     Initial_Learning_Rate : Float := 0.1;
                     Beta_1                : Float := 0.9;
                     Beta_2                : Float := 0.999;
                     Epsilon               : Float) is
      use Ada.Containers;
      Zeros : Float_List;
   begin
      Self.Coeff_Params := Coeff_Params;
      Self.Intercept_Params := Intercept_Params;
      Self.Initial_Learning_Rate := Initial_Learning_Rate;
      Self.Beta_1 := Beta_1;
      Self.Beta_2 := Beta_2;
      Self.Epsilon := Epsilon;

      Self.Time_Step := 0;
      for index in 1 .. Intercept_Params.Length loop
            Self.Intercept_First_Moments.Append (0.0);
            Self.Intercept_Second_Moments.Append (0.0);
      end loop;

      for index in 1 .. Coeff_Params.Length loop
            Zeros.Append (0.0);
      end loop;
      for index in 1 .. Coeff_Params.Length loop
            Self.Coeff_First_Moments .Append (Zeros);
            Self.Coeff_Second_Moments.Append (Zeros);
      end loop;

   end C_Init;

   --  -------------------------------------------------------------------------

   procedure C_Init (Self                  : out SGD_Optimizer;
                     --  Coeff_Params: laysers x features x values
                     Coeff_Params          : Float_List_3D;
                     --  Intercept_Params: laysers x values
                     Intercept_Params      : Float_List_2D;
                     Initial_Learning_Rate : Float := 0.1;
                     LR_Schedule           : LR_Schedule_Type := Constant_LR_Schedule;
                     Momentum              : Float := 0.9;
                     Use_Nesterov          : Boolean := True;
                     Power_T               : Float := 0.5) is
      use Ada.Containers;
      Zeros : Float_List;
   begin
      Self.Coeff_Params := Coeff_Params;
      Self.Intercept_Params := Intercept_Params;
      Self.Initial_Learning_Rate := Initial_Learning_Rate;
      Self.LR_Schedule := LR_Schedule;
      Self.Momentum := Momentum;
      Self.Use_Nesterov := Use_Nesterov;
      Self.Power_T := Power_T;

      for index in 1 .. Intercept_Params.Length loop
            Self.Intercept_Velocities.Append (0.0);
      end loop;

      for index in 1 .. Coeff_Params.Length loop
            Zeros.Append (0.0);
      end loop;
      for index in 1 .. Coeff_Params.Length loop
            Self.Coeff_Velocities.Append (Zeros);
      end loop;

   end C_Init;

   --  -------------------------------------------------------------------------

   function Get_Updates (Self   : in out Adam_Optimizer;
                         Params : Parameters_Record) return Parameters_Record is
      use Maths.Float_Math_Functions;
      Learning_Rate    : Float;
      Coeff_Params_1D  : Float_List;
      Coeff_Updates_1D : Float_List;
      Updates          : Parameters_Record;
   begin
      Self.Time_Step := Self.Time_Step + 1;

      --  Update first and second coeff moments
      for m in Self.Coeff_First_Moments.First_Index ..
        Self.Coeff_First_Moments.Last_Index loop
         Coeff_Params_1D := Params.Coeff_Params.Element (m);
         Coeff_Updates_1D.Clear;
         for coeff in Coeff_Params_1D.First_Index ..
              Coeff_Params_1D.Last_Index loop
                Self.Coeff_First_Moments.Append
                  (Float (m) * Self.Beta_1 +
                   (1.0 - Self.Beta_1) * Coeff_Updates_1D.Element (m));
                Self.Coeff_Second_Moments.Append
                  (Float (m) * Self.Beta_2 +
                   (1.0 - Self.Beta_2) * Coeff_Updates_1D.Element (m) ** 2);
         end loop;

         --  Update first and second intercept moments
         Self.Intercept_First_Moments.Append
              (Float (m) * Self.Beta_1 +
               (1.0 - Self.Beta_1) * Params.Intercept_Params.Element (m));
         Self.Intercept_Second_Moments.Append
           (Float (m) * Self.Beta_2 +
            (1.0 - Self.Beta_2) * Params.Intercept_Params.Element (m) ** 2);
      end loop;

      --  Update learning rate
      Self.Learning_Rate := Sqrt
        (1.0 - Self.Beta_2 ** Self.Time_Step) * Self.Initial_Learning_Rate /
        (1.0 - Self.Beta_1 ** Self.Time_Step);

      for m in Self.Coeff_First_Moments.First_Index ..
        Self.Coeff_First_Moments.Last_Index loop
         Coeff_Params_1D.Clear;
         for m2 in Self.Coeff_First_Moments.First_Index ..
           Self.Coeff_First_Moments.Last_Index loop
            Learning_Rate := -Float (m) * Self.Learning_Rate;
            Coeff_Params_1D.Append
              (Learning_Rate / (Sqrt (Self.Intercept_Second_Moments (m)) +
                   Self.Epsilon));
         end loop;
            Updates.Coeff_Params.Append (Coeff_Params_1D);
      end loop;

      for m in Self.Intercept_First_Moments.First_Index ..
        Self.Intercept_First_Moments.Last_Index loop
         Learning_Rate := -Float (m) * Self.Learning_Rate;
         Updates.Intercept_Params.Append
           (Learning_Rate / (Sqrt (Self.Intercept_Second_Moments (m)) + Self.Epsilon));
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
      for index in Self.Intercept_Velocities.First_Index ..
          Self.Intercept_Velocities.Last_Index loop
         M_V := Self.Momentum * Self.Intercept_Velocities (index);

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
         Updates.Intercept_Params .Append (M_V);
         Self.Intercept_Velocities.Append (M_V);
      end loop;

      return Updates;

   end Get_Updates;

   --  -------------------------------------------------------------------------
   --  L29
   procedure Update_Params (Self   : in out SGD_Optimizer;
                            Params : in out Parameters_Record;
                            Grads  : Parameters_Record) is
      Coef_1D         : Float_List;
      Coef_Updates_1D : Float_List;
      Updates         : Parameters_Record;
   begin
      Updates := Get_Updates (Self, Grads);
      for index in Updates.Coeff_Params.First_Index ..
        Updates.Coeff_Params.Last_Index loop
         Coef_1D := Params.Coeff_Params (index);
         Coef_Updates_1D.Clear;
         for index in Coef_1D.First_Index .. Coef_1D.Last_Index loop
            Coef_Updates_1D.Append (Coef_1D (index) + Coef_Updates_1D (index));
         end loop;
         Params.Coeff_Params (index) := Coef_Updates_1D;
      end loop;

      for index in Updates.Intercept_Params.First_Index ..
        Updates.Intercept_Params.Last_Index loop
         Params.Intercept_Params (index) :=
         Params.Intercept_Params (index) + Updates.Intercept_Params (index);
      end loop;

   end Update_Params;

   --  -------------------------------------------------------------------------

end Stochastic_Optimizers;
