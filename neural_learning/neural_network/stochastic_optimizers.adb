--  Based on scikit-learn/sklearn/neural_network/_stochastic_optimizers.py

with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

package body Stochastic_Optimizers is

   procedure C_Init (Self                  : out Adam_Optimizer;
                     --  Coeff_Params: layers x features x values
                     --  Intercept_Params: laysers x values
                     Params                : Parameters_List;
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

      Self.Time_Step := 0;
      Self.First_Moments.Clear;
      Self.Second_Moments.Clear;

   end C_Init;

   --  -------------------------------------------------------------------------

   procedure C_Init (Self                  : out SGD_Optimizer;
                     --  Coeff_Params: laysers x features x values
                     --  Intercept_Params: laysers x values
                     Params                : Parameters_List;
                     Initial_Learning_Rate : Float := 0.1;
                     Learning_Rate         : Float := 0.1;
                     Learning_Rate_Kind    : Learning_Rate_Type :=
                       Constant_Rate;
                     LR_Schedule           : LR_Schedule_Type :=
                       Constant_LR_Schedule;
                     Momentum              : Float := 0.9;
                     Use_Nesterov          : Boolean := True;
                     Power_T               : Float := 0.5) is
   begin
      Self.Params := Params;
      Self.Initial_Learning_Rate := Initial_Learning_Rate;
      Self.Learning_Rate := Learning_Rate;
      Self.Learning_Rate_Kind := Learning_Rate_Kind;
      Self.LR_Schedule := LR_Schedule;
      Self.Momentum := Momentum;
      Self.Use_Nesterov := Use_Nesterov;
      Self.Power_T := Power_T;

      Self.Velocities.Clear;

   end C_Init;

   --  -------------------------------------------------------------------------
   --  L256
   function Get_Adam_Updates (Self   : in out Adam_Optimizer;
                              Params : Parameters_List)
                              return Moments_List is
      use Maths.Float_Math_Functions;
      Routine_Name         : constant String :=
                               "Stochastic_Optimizers.Get_Adam_Updates ";
      Learning_Rate        : Float;
      Layer_Params         : Parameters_Record;
      Coeff_Params_2D      : Float_List_2D;
      Coeff_Params_1D      : Float_List;
      Intercept_Params_1D  : Float_List;
      Updates              : Moments_List;
   begin
      Self.Time_Step := Self.Time_Step + 1;
      --  L279 Update learning rate
      Self.Learning_Rate := Sqrt
        (1.0 - Self.Beta_2 ** Self.Time_Step) * Self.Initial_Learning_Rate /
        (1.0 - Self.Beta_1 ** Self.Time_Step);

      for layer in Params.First_Index .. Params.Last_Index loop
         Layer_Params := Params (layer);
         --  L271, L274  Update first and second coeff moments
         for m in Layer_Params.Coeff_Params.First_Index ..
           Layer_Params.Coeff_Params.Last_Index loop
            Put_Line (Routine_Name & "m:" & Integer'Image (m));
            Coeff_Params_2D := Layer_Params.Coeff_Params;

            for coeff in Coeff_Params_2D.First_Index ..
              Coeff_Params_2D.Last_Index loop
               --              Put_Line (Routine_Name & "coeff:" & Integer'Image (coeff));
               Coeff_Params_1D := Coeff_Params_2D (coeff);
               --  L272
               Self.First_Moments (layer).Coeff_Moments.Append
                 (Float (m) * Self.Beta_1 +
                  (1.0 - Self.Beta_1) * Coeff_Params_1D (m));

               --  L276
               Self.Second_Moments (layer).Coeff_Moments.Append
                 (Float (m) * Self.Beta_2 +
                  (1.0 - Self.Beta_2) * Coeff_Params_1D (m) ** 2);
            end loop;

            --  Update first and second intercept moments
            Intercept_Params_1D := Layer_Params.Intercept_Params;
            for interc in Intercept_Params_1D.First_Index ..
              Intercept_Params_1D.Last_Index loop
               Self.First_Moments (layer).Intercept_Moments.Append
                 (Float (m) * Self.Beta_1 +
                  (1.0 - Self.Beta_1) * Intercept_Params_1D (interc));
               Self.Second_Moments (layer).Intercept_Moments.Append
                 (Float (m) * Self.Beta_2 +
                  (1.0 - Self.Beta_2) * Intercept_Params_1D.Element (interc) ** 2);
            end loop;
         end loop;

         --  L284
         Coeff_Params_1D.Clear;
         for index in Self.First_Moments (layer).Coeff_Moments.First_Index ..
           Self.First_Moments (layer).Coeff_Moments.Last_Index loop
            --              Put_Line (Routine_Name & "index:" & Integer'Image (row));
            Learning_Rate :=
              -Self.Learning_Rate *
              Self.First_Moments (layer).Coeff_Moments (index);
            --              Put_Line (Routine_Name & "Learning_Rate set");
            Coeff_Params_1D.Append
              (Learning_Rate
               / (Sqrt (Self.Second_Moments (layer).Coeff_Moments (index)) +
                     Self.Epsilon));
            --              Put_Line (Routine_Name & "Coeff_Params_1D appended");
         end loop;

         Updates (layer).Coeff_Moments.Append (Coeff_Params_1D);

         Intercept_Params_1D.Clear;
         for index in Self.First_Moments (layer).Intercept_Moments.First_Index ..
           Self.First_Moments (layer).Intercept_Moments.Last_Index loop
            --              Put_Line (Routine_Name & "index:" & Integer'Image (row));
            Learning_Rate :=
              -Self.Learning_Rate * Self.First_Moments (layer).Intercept_Moments (index);
            --              Put_Line (Routine_Name & "Learning_Rate set");
            Intercept_Params_1D.Append
              (Learning_Rate /
                 (Sqrt (Self.Second_Moments (layer).Intercept_Moments (index)) +
                      Self.Epsilon));
            --              Put_Line (Routine_Name & "Intercept_Params_1D appended");
         end loop;

         Updates (layer).Intercept_Moments.Append (Intercept_Params_1D);
      end loop;

      return Updates;

   end Get_Adam_Updates;

   --  -------------------------------------------------------------------------
   --  L169
   function Get_SGD_Updates
     (Self : in out SGD_Optimizer; Params : Parameters_List)
      return Moments_List is
      use Float_List_Package;
      Layer_Velocities     : Parameters_Record;
      M_V                  : Float;
      Coeff_Params_2D      : Float_List_2D;
      Coeff_Params_1D      : Float_List;
      Intercept_Params_1D  : Float_List;
      Coeff_Updates_1D     : Float_List;
      Coeff_Updates_2D     : Float_List_2D;
      Intercept_Updates_1D : Float_List;
      Updates              : Moments_List;
   begin
      for layer in Self.Velocities.First_Index ..
        Self.Velocities.Last_Index loop
         Layer_Velocities := Self.Velocities (layer);
         for index in Layer_Velocities.Coeff_Params.First_Index ..
           Layer_Velocities.Coeff_Params.Last_Index loop
            Coeff_Params_2D := Params (layer).Coeff_Params;
            Intercept_Params_1D := Params (layer).Intercept_Params;

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
            Updates (Layer).Coeff_Moments.Append (Coeff_Updates_2D);

            M_V := 0.0;
            for index in Intercept_Params_1D.First_Index ..
              Intercept_Params_1D.Last_Index loop
               M_V := M_V - Self.Learning_Rate * Intercept_Params_1D (index);
               Intercept_Updates_1D.Append (M_V);
            end loop;
            Updates (Layer).Intercept_Params.Replace_Element (layer, Intercept_Updates_1D);
         end loop;
      end loop;

      Self.Velocities := Updates;

      return Updates;

   end Get_SGD_Updates;

   --  -------------------------------------------------------------------------
   --  L29
   procedure Update_Params (Self   : in out Optimizer_Record;
                            Params : in out Parameters_List;
                            Grads  : Parameters_List) is
      Routine_Name       : constant String :=
                             "Stochastic_Optimizers.Update_Params ";
      Coef_1D            : Float_List;
      Coef_2D            : Float_List_2D;
      Intercept_1D       : Float_List;
      Coef_Updates_1D    : Float_List;
      Coef_Updates_2D    : Float_List_2D;
      Intercept_Updates  : Float_List;
      Updates            : Moments_List;
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
