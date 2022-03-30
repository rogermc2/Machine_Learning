--  Based on scikit-learn/sklearn/neural_network/_stochastic_optimizers.py

with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

package body Stochastic_Optimizers is

   function Moments_Sqrt (M : Moments_Record; Epsilon : Float := 0.0)
                          return Moments_Record;

   --  -------------------------------------------------------------------------

   function "+" (L, R : Parameters_List) return Parameters_List is
      use Parameters_Package;
      L_Item  : Parameters_Record;
      R_Item  : Parameters_Record;
      Sum_Rec : Parameters_Record;
      Sum     : Parameters_List;
   begin
      for index in L.First_Index .. L.Last_Index loop
         L_Item := L (index);
         R_Item := R (index);
         Sum_Rec.Coeff_Params := L_Item.Coeff_Params + R_Item.Coeff_Params;
         Sum_Rec.Intercept_Params := L_Item.Intercept_Params +
           R_Item.Intercept_Params;
         Sum.Append (Sum_Rec);
      end loop;

      return Sum;

   end "+";

   --  -------------------------------------------------------------------------

   function "-" (M : Moments_Record) return Moments_Record is
      Minus : Moments_Record := M;
   begin
      for index in Minus.Coeff_Moments.First_Index ..
        Minus.Coeff_Moments.Last_Index loop
         Minus.Coeff_Moments (index) := - Minus.Coeff_Moments (index);
         Minus.Intercept_Moments (index) := - Minus.Intercept_Moments (index);
      end loop;

      return Minus;

   end "-";

   --  -------------------------------------------------------------------------

   function "*" (L : Float; R : Moments_Record) return Moments_Record is
      Product : Moments_Record := R;
   begin
      for index in Product.Coeff_Moments.First_Index ..
        Product.Coeff_Moments.Last_Index loop
         Product.Coeff_Moments (index) := L * Product.Coeff_Moments (index);
         Product.Intercept_Moments (index) :=
           L * Product.Intercept_Moments (index);
      end loop;

      return Product;

   end "*";

   --  -------------------------------------------------------------------------

   function "/" (L, R : Moments_Record) return Moments_Record is
      Result  : Moments_Record;
   begin
      for index in L.Coeff_Moments.First_Index ..
        L.Coeff_Moments.Last_Index loop
         Result.Coeff_Moments (index) :=
           L.Coeff_Moments (index) / R.Coeff_Moments (index);
         Result.Intercept_Moments (index) :=
           L.Intercept_Moments (index) / R.Intercept_Moments (index);
      end loop;

      return Result;

   end "/";

   --  -------------------------------------------------------------------------

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
                              Grads  : Parameters_List)
                              return Parameters_List is
      use Maths.Float_Math_Functions;
      Routine_Name          : constant String :=
                                "Stochastic_Optimizers.Get_Adam_Updates ";
      Layer_Grads           : Parameters_Record;
      First_Coeff_Moments   : Float_List;
      Second_Coeff_Moments  : Float_List;
      Coeff_Params          : Float_List;
      Updated_Coeff_Params  : Float_List;
      Update_First_Moments  : Moments_Record;
      Update_Second_Moments : Moments_Record;
      First_Moment_Updates  : Moments_List;
      Second_Moment_Updates : Moments_List;
      Moments               : Moments_Record;
      Moment_Updates        : Moments_List;
      Updates               : Parameters_List;
   begin
      Self.Time_Step := Self.Time_Step + 1;
      --  L279 Update learning rate
      Self.Learning_Rate := Sqrt
        (1.0 - Self.Beta_2 ** Self.Time_Step) * Self.Initial_Learning_Rate /
        (1.0 - Self.Beta_1 ** Self.Time_Step);

      for layer in Grads.First_Index .. Grads.Last_Index loop
         Layer_Grads := Grads (layer);
         Updated_Coeff_Params.Clear;
         --  L271, L274  Update first and second coeff moments
         for grad in Layer_Grads.Coeff_Params.First_Index ..
           Layer_Grads.Coeff_Params.Last_Index loop
            Put_Line (Routine_Name & "grad:" & Integer'Image (grad));
            First_Coeff_Moments := Self.First_Moments (grad).Coeff_Moments;
            Second_Coeff_Moments := Self.Second_Moments (grad).Coeff_Moments;
            Coeff_Params := Layer_Grads.Coeff_Params (grad);

            Put_Line (Routine_Name & "L272");
            --  L272
            Update_First_Moments.Coeff_Moments.Append
              (Self.Beta_1 * First_Coeff_Moments +
                 (1.0 - Self.Beta_1) * Coeff_Params);
            Updated_Coeff_Params.Append (Update_First_Moments.Coeff_Moments);

            Put_Line (Routine_Name & "L276");
            --  L276
            Update_Second_Moments.Coeff_Moments.Append
              (Self.Beta_2 * Second_Coeff_Moments +
                 (1.0 - Self.Beta_2) * Coeff_Params ** 2);
            Layer_Grads.Coeff_Params (grad) := Updated_Coeff_Params;

         end loop;
         First_Moment_Updates.Append (Update_First_Moments);
         Second_Moment_Updates.Append (Update_Second_Moments);
         Updates.Append (Layer_Grads);
      end loop;

      Self.First_Moments := First_Moment_Updates;
      Self.Second_Moments := Second_Moment_Updates;

      for layer in First_Moment_Updates.First_Index ..
        First_Moment_Updates.Last_Index loop
         --  L284
         Update_First_Moments := First_Moment_Updates (layer);
         Update_Second_Moments := Second_Moment_Updates (layer);
         Moments := - Self.Learning_Rate * Update_First_Moments /
           Moments_Sqrt (Update_Second_Moments, Self.Epsilon);
         Moment_Updates.Append (Moments);
      end loop;

      return Updates;

   end Get_Adam_Updates;

   --  -------------------------------------------------------------------------
   --  L169
   function Get_SGD_Updates
     (Self : in out SGD_Optimizer; Params : Parameters_List)
      return Parameters_List is
      use Float_List_Package;
      Layer_Velocities     : Parameters_Record;
      M_V                  : Float;
      Coeff_Params_2D      : Float_List_2D;
      Coeff_Params_1D      : Float_List;
      Intercept_Params_1D  : Float_List;
      Coeff_Updates_1D     : Float_List;
      Coeff_Updates_2D     : Float_List_2D;
      Intercept_Updates    : Float_List;
      Update_Params        : Parameters_Record;
      Updates              : Parameters_List;
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
               --                      Coeff_Updates_2D.Append (Coeff_Updates_1D);
               Update_Params.Coeff_Params.Append (Coeff_Params_1D);
               --                      Moments.Coeff_Moments := Coeff_Updates_1D;
            end loop;
            Layer_Velocities.Coeff_Params := Coeff_Updates_2D;

            M_V := 0.0;
            for index in Intercept_Params_1D.First_Index ..
              Intercept_Params_1D.Last_Index loop
               M_V := M_V - Self.Learning_Rate * Intercept_Params_1D (index);
               Intercept_Updates.Append (M_V);
            end loop;
            Update_Params.Intercept_Params.Append (Intercept_Updates);
            Layer_Velocities.Intercept_Params := Intercept_Updates;
            Self.Velocities.Append (Layer_Velocities);
         end loop;
         Updates.Append (Update_Params);
      end loop;

      return Updates;

   end Get_SGD_Updates;

   --  -------------------------------------------------------------------------

   function Moments_Sqrt (M : Moments_Record; Epsilon : Float := 0.0)
                          return Moments_Record is
      use Maths.Float_Math_Functions;
      Result : Moments_Record := M;
   begin
      for index in Result.Coeff_Moments.First_Index ..
        Result.Coeff_Moments.Last_Index loop
         Result.Coeff_Moments (index) :=
           Sqrt (Result.Coeff_Moments (index)) + Epsilon;
         Result.Intercept_Moments (index) :=
           Sqrt (Result.Intercept_Moments (index)) + Epsilon;
      end loop;

      return Result;

   end Moments_Sqrt;

   --  -------------------------------------------------------------------------
   --  L29
   procedure Update_Params (Self   : in out Optimizer_Record;
                            Params : in out Parameters_List;
                            Grads  : Parameters_List) is
      Routine_Name : constant String :=
                       "Stochastic_Optimizers.Update_Params ";
      Updates      : Parameters_List;
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

      Put_Line (Routine_Name & "L44");
      --  L44
      Params := Params + Updates;
      Put_Line (Routine_Name & "Params updated");

   end Update_Params;

   --  -------------------------------------------------------------------------

end Stochastic_Optimizers;
