--  Based on scikit-learn/sklearn/neural_network/_stochastic_optimizers.py

with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

package body Stochastic_Optimizers is

   procedure C_Init (Self                  : out Adam_Optimizer;
                     --  Coeff_Params: layers x features x values
                     --  Intercept_Params: laysers x values
                     Params                : Parameters_Record;
                     Initial_Learning_Rate : Float := 0.1;
                     Beta_1                : Float := 0.9;
                     Beta_2                : Float := 0.999;
                     Epsilon               : Float) is
      use Ada.Containers;
      Zeros : Float_List;
   begin
      Self.Params := Params;
      Self.Initial_Learning_Rate := Initial_Learning_Rate;
      Self.Beta_1 := Beta_1;
      Self.Beta_2 := Beta_2;
      Self.Epsilon := Epsilon;

      Self.Time_Step := 0;
      for index in 1 .. Params.Intercept_Params.Length loop
         Self.Intercept_First_Moments.Append (0.0);
         Self.Intercept_Second_Moments.Append (0.0);
      end loop;

      for index in 1 .. Params.Coeff_Params.Length loop
         Zeros.Append (0.0);
      end loop;

      for index in 1 .. Params.Coeff_Params.Length loop
         Self.Coeff_First_Moments .Append (Zeros);
         Self.Coeff_Second_Moments.Append (Zeros);
      end loop;

   end C_Init;

   --  -------------------------------------------------------------------------

   procedure C_Init (Self                  : out SGD_Optimizer;
                     --  Coeff_Params: laysers x features x values
                     --  Intercept_Params: laysers x values
                     Params                : Parameters_Record;
                     Initial_Learning_Rate : Float := 0.1;
                     Learning_Rate         : Float := 0.1;
                     Learning_Rate_Kind    : Learning_Rate_Type :=
                       Constant_Rate;
                     LR_Schedule           : LR_Schedule_Type :=
                       Constant_LR_Schedule;
                     Momentum              : Float := 0.9;
                     Use_Nesterov          : Boolean := True;
                     Power_T               : Float := 0.5) is
      use Ada.Containers;
      Zeros    : Float_List;
   begin
      Self.Params := Params;
      Self.Initial_Learning_Rate := Initial_Learning_Rate;
      Self.Learning_Rate := Learning_Rate;
      Self.Learning_Rate_Kind := Learning_Rate_Kind;
      Self.LR_Schedule := LR_Schedule;
      Self.Momentum := Momentum;
      Self.Use_Nesterov := Use_Nesterov;
      Self.Power_T := Power_T;

      for index in 1 .. Self.Velocities.Intercept_Params.Length loop
         Zeros.Append (0.0);
      end loop;

      for index in 1 .. Self.Velocities.Intercept_Params.Length loop
         Self.Velocities.Intercept_Params.Append (Zeros);
      end loop;

      for index in 1 .. Integer (Self.Velocities.Coeff_Params.Length) loop
         Self.Velocities.Coeff_Params (index).Append (Zeros);
      end loop;

   end C_Init;

   --  -------------------------------------------------------------------------
   --  L256
   function Get_Adam_Updates (Self   : in out Adam_Optimizer;
                              Params : Parameters_Record)
                              return Parameters_Record is
      use Maths.Float_Math_Functions;
      Routine_Name         : constant String :=
                             "Stochastic_Optimizers.Get_Adam_Updates ";
      Learning_Rate        : Float;
      Coeff_Params_2D      : Float_List_2D;
      Coeff_Params_1D      : Float_List;
      Intercept_Params_1D  : Float_List;
      Intercept_Updates_1D : Float_List;
      Updates              : Parameters_Record;
   begin
      Self.Time_Step := Self.Time_Step + 1;

      --  L271  Update first and second coeff moments
      for m in Self.Coeff_First_Moments.First_Index ..
        Self.Coeff_First_Moments.Last_Index loop
         Put_Line (Routine_Name & "m:" & Integer'Image (m));
         Coeff_Params_2D := Params.Coeff_Params.Element (m);
         Intercept_Updates_1D := Params.Intercept_Params.Element (m);

         for coeff in Coeff_Params_2D.First_Index ..
           Coeff_Params_2D.Last_Index loop
            Coeff_Params_1D := Coeff_Params_2D (coeff);
            Self.Coeff_First_Moments.Append
              (Float (m) * Self.Beta_1 +
               (1.0 - Self.Beta_1) * Coeff_Params_1D.Element (m));
            Self.Coeff_Second_Moments.Append
              (Float (m) * Self.Beta_2 +
               (1.0 - Self.Beta_2) * Coeff_Params_1D.Element (m) ** 2);
         end loop;
         Put_Line (Routine_Name & "Coeff Moments set");

         --  Update first and second intercept moments
         Self.Intercept_First_Moments.Append
           (Float (m) * Self.Beta_1 +
            (1.0 - Self.Beta_1) * Intercept_Updates_1D.Element (m));
         Self.Intercept_Second_Moments.Append
           (Float (m) * Self.Beta_2 +
            (1.0 - Self.Beta_2) * Intercept_Updates_1D.Element (m) ** 2);
      end loop;
      Put_Line (Routine_Name & "intercept Moments set");

      --  Update learning rate
      Self.Learning_Rate := Sqrt
        (1.0 - Self.Beta_2 ** Self.Time_Step) * Self.Initial_Learning_Rate /
        (1.0 - Self.Beta_1 ** Self.Time_Step);

      for layer in Self.Coeff_First_Moments.First_Index ..
        Self.Coeff_First_Moments.Last_Index loop
         Put_Line (Routine_Name & "layer:" & Integer'Image (layer));
         Coeff_Params_2D.Clear;
         for row in Self.Coeff_First_Moments.First_Index ..
           Self.Coeff_First_Moments.Last_Index loop
            Learning_Rate := -Float (layer) * Self.Learning_Rate;
            Coeff_Params_1D.Append
              (Learning_Rate / (Sqrt (Self.Intercept_Second_Moments (row)) +
                   Self.Epsilon));
         end loop;
         Coeff_Params_2D.Append (Coeff_Params_1D);
         Updates.Coeff_Params.Append (Coeff_Params_2D);
      end loop;

      for m in Self.Intercept_First_Moments.First_Index ..
        Self.Intercept_First_Moments.Last_Index loop
         Learning_Rate := -Float (m) * Self.Learning_Rate;
         Intercept_Params_1D.Append (Learning_Rate / (Sqrt (Self.Intercept_Second_Moments (m)) +
                                       Self.Epsilon));
         Updates.Intercept_Params.Append (Intercept_Params_1D);
      end loop;

      return Updates;

   end Get_Adam_Updates;

   --  -------------------------------------------------------------------------
   --  L169
   function Get_SGD_Updates
     (Self : in out SGD_Optimizer; Params : Parameters_Record)
      return Parameters_Record is
      use Float_List_Package;
      M_V                  : Float;
      Coeff_Params_2D      : Float_List_2D;
      Coeff_Params_1D      : Float_List;
      Intercept_Params_1D  : Float_List;
      Coeff_Updates_1D     : Float_List;
      Coeff_Updates_2D     : Float_List_2D;
      Intercept_Updates_1D : Float_List;
      Updates              : Parameters_Record;
   begin

      for layer in Self.Velocities.Coeff_Params.First_Index ..
        Self.Velocities.Coeff_Params.Last_Index loop

         Coeff_Params_2D := Params.Coeff_Params.Element (layer);
         Intercept_Params_1D := Params.Intercept_Params (layer);

         Coeff_Updates_1D.Clear;
         M_V := 0.0;
         for coeff in Coeff_Params_2D.First_Index ..
           Coeff_Params_2D.Last_Index loop
            Coeff_Params_1D := Coeff_Params_2D (coeff);
            for index in Coeff_Params_1D.First_Index ..
              Coeff_Params_1D.Last_Index loop
               M_V := M_V - Self.Learning_Rate * Coeff_Params_1D (index);
               Coeff_Updates_1D.Append (M_V);
            end loop;
            Coeff_Updates_2D.Append (Coeff_Updates_1D);
         end loop;
         Updates.Coeff_Params.Replace_Element (layer, Coeff_Updates_2D);

         M_V := 0.0;
         for index in Intercept_Params_1D.First_Index ..
           Intercept_Params_1D.Last_Index loop
            M_V := M_V - Self.Learning_Rate * Intercept_Params_1D (index);
            Intercept_Updates_1D.Append (M_V);
         end loop;
         Updates.Intercept_Params.Replace_Element (layer, Intercept_Updates_1D);
      end loop;

      Self.Velocities := Updates;

      return Updates;

   end Get_SGD_Updates;

   --  -------------------------------------------------------------------------
   --  L29
   procedure Update_Params (Self   : in out Optimizer_Record;
                            Params : in out Parameters_Record;
                            Grads  : Parameters_Record) is
      Routine_Name       : constant String :=
                             "Stochastic_Optimizers.Update_Params ";
      Coef_1D           : Float_List;
      Coef_2D           : Float_List_2D;
      Intercept_1D      : Float_List;
      Coef_Updates_1D   : Float_List;
      Coef_Updates_2D   : Float_List_2D;
      Intercept_Updates : Float_List;
      Updates           : Parameters_Record;
   begin
      Put_Line (Routine_Name);
      --  L42
      case Self.Kind is
         when Optimizer_Adam =>
            Updates := Get_Adam_Updates (Self.Adam, Grads);
         when Optimizer_SGD =>
            Updates := Get_SGD_Updates (Self.SGD, Grads);
         when No_Optimizer => null;
      end case;

      Put_Line (Routine_Name & "L43");
      --  L43 for each layer p:
      for layer in Updates.Coeff_Params.First_Index ..
        Updates.Coeff_Params.Last_Index loop
         Coef_2D := Params.Coeff_Params (layer);
         Coef_Updates_2D.Clear;
         for index in Coef_2D.First_Index .. Coef_2D.Last_Index loop
            Coef_Updates_1D.Clear;
            for index in Coef_1D.First_Index .. Coef_1D.Last_Index loop
               Coef_Updates_1D.Append (Coef_1D (index) + Coef_Updates_1D (index));
            end loop;
            Coef_Updates_2D.Append (Coef_Updates_1D);
         end loop;
         Params.Coeff_Params (layer) := Coef_Updates_2D;
      end loop;
      Put_Line (Routine_Name & "Coeff_Params set");

      for layer in Updates.Intercept_Params.First_Index ..
        Updates.Intercept_Params.Last_Index loop
         Put_Line (Routine_Name & "layer:" & Integer'Image (layer));
         Intercept_1D := Params.Intercept_Params (layer);
         Intercept_Updates.Clear;
         for index in Intercept_1D.First_Index .. Intercept_1D.Last_Index loop
            Put_Line (Routine_Name & "index:" & Integer'Image (index));
            Intercept_Updates.Append (Intercept_1D (index) +
                                        Intercept_Updates (index));
         end loop;
         Params.Intercept_Params (layer) := Intercept_Updates;
      end loop;

   end Update_Params;

   --  -------------------------------------------------------------------------

end Stochastic_Optimizers;
