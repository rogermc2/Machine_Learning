--  Based on scikit-learn/sklearn/neural_network/_stochastic_optimizers.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

package body Stochastic_Optimizers is

   function Moments_Sqrt (M : Parameters_Record; Epsilon : Float := 0.0)
                          return Parameters_Record;
   procedure Zero_Init (Params : in out Parameters_List);

   --  -------------------------------------------------------------------------

   function "+" (L, R : Parameters_Record) return Parameters_Record is
      use Float_List_Package;
      Sum_Rec : Parameters_Record;
   begin
      Sum_Rec.Coeff_Params := L.Coeff_Params + R.Coeff_Params;
      Sum_Rec.Intercept_Params := L.Intercept_Params + R.Intercept_Params;

      return Sum_Rec;

   end "+";

   --  -------------------------------------------------------------------------

   function "+" (L, R : Parameters_List) return Parameters_List is
      use Parameters_Package;
      Sum : Parameters_List;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Sum.Append (L (index) + R (index));
      end loop;

      return Sum;

   end "+";

   --  -------------------------------------------------------------------------

   function "-" (M : Parameters_Record) return Parameters_Record is
      Minus : Parameters_Record;
   begin
      for index in M.Coeff_Params.First_Index ..
        M.Coeff_Params.Last_Index loop
         for index_2 in M.Coeff_Params (index).First_Index ..
           M.Coeff_Params (index).Last_Index loop
            Minus.Coeff_Params (index) (index_2) :=
              - Minus.Coeff_Params (index) (index_2);
         end loop;
      end loop;

      for index in M.Intercept_Params.First_Index ..
              M.Intercept_Params.Last_Index loop
         Minus.Intercept_Params (index) := - Minus.Intercept_Params (index);
      end loop;

      return Minus;

   end "-";

   --  ------------------------------------------------------------------------

   function "-" (L, R : Parameters_Record) return Parameters_Record is
      Minus : Parameters_Record;
   begin
      for index in L.Coeff_Params.First_Index ..
        L.Coeff_Params.Last_Index loop
         for index_2 in L.Coeff_Params (index).First_Index ..
           L.Coeff_Params (index).Last_Index loop
            Minus.Coeff_Params (index) (index_2) :=
              L.Coeff_Params (index) (index_2)
              - R.Coeff_Params (index) (index_2);
         end loop;
      end loop;

      for index in L.Intercept_Params.First_Index ..
        L.Intercept_Params.Last_Index loop
         Minus.Intercept_Params (index) :=
           L.Intercept_Params (index) - R.Intercept_Params (index);
      end loop;

      return Minus;

   end "-";

   --  ------------------------------------------------------------------------

   function "*" (L : Float; R : Parameters_Record) return Parameters_Record is
      use Float_List_Package;
      Coeffs  : Float_List;
      Product : Parameters_Record;
   begin
      for index in R.Coeff_Params.First_Index ..
        R.Coeff_Params.Last_Index loop
         Coeffs.Clear;
         for index_2 in R.Coeff_Params (index).First_Index ..
           R.Coeff_Params (index).Last_Index loop
            Coeffs.Append (L * R.Coeff_Params (index) (index_2));
         end loop;
         Product.Coeff_Params.Append (Coeffs);
      end loop;

      for index in R.Intercept_Params.First_Index ..
        R.Intercept_Params.Last_Index loop
         Product.Intercept_Params.Append (L * R.Intercept_Params (index));
      end loop;

      return Product;

   end "*";

   --  -------------------------------------------------------------------------

   function "**" (Rec : Parameters_Record; P : Integer)
                  return Parameters_Record is
      Result : Parameters_Record;
   begin
      Result.Coeff_Params := Rec.Coeff_Params ** P;
      Result.Intercept_Params := Rec.Intercept_Params ** P;

      return Result;

   end "**";

   --  -------------------------------------------------------------------------

   function "/" (L, R : Parameters_Record) return Parameters_Record is
      use Float_List_Package;
      Result  : Parameters_Record;
   begin
      for index in L.Coeff_Params.First_Index .. L.Coeff_Params.Last_Index loop
         for index2 in L.Coeff_Params (index).First_Index ..
           L.Coeff_Params (index).Last_Index loop
            Result.Coeff_Params (index) (index2) :=
              L.Coeff_Params (index) (index2) / R.Coeff_Params (index) (index2);
         end loop;
      end loop;

      for index in L.Intercept_Params.First_Index ..
          L.Intercept_Params.Last_Index loop
         Result.Intercept_Params (index) :=
           L.Intercept_Params (index) / R.Intercept_Params (index);
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
      Routine_Name          : constant String :=
                                "Stochastic_Optimizers.C_Init Adam ";
   begin
      Put_Line (Routine_Name & "Params length" &
                  Integer'Image (Integer (Params.Length)));
      Self.Params := Params;
      Self.Initial_Learning_Rate := Initial_Learning_Rate;
      Self.Learning_Rate := Initial_Learning_Rate;
      Self.Beta_1 := Beta_1;
      Self.Beta_2 := Beta_2;
      Self.Epsilon := Epsilon;

      Self.Time_Step := 0;
      Self.First_Moments := Params;
      Zero_Init (Self.First_Moments);
      Self.Second_Moments := Self.First_Moments;

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

      Self.Velocities := Params;
      Zero_Init (Self.Velocities);

   end C_Init;

   --  -------------------------------------------------------------------------
   --  L256
   function Get_Adam_Updates (Self  : in out Adam_Optimizer;
                              Grads : Parameters_List)
                              return Parameters_List is
      use Ada.Containers;
      use Maths.Float_Math_Functions;
      use Parameters_Package;
      Routine_Name          : constant String :=
                                "Stochastic_Optimizers.Get_Adam_Updates ";
      Layer_Grads           : Parameters_Record;
      Update_First_Moments  : Parameters_Record;
      Update_Second_Moments : Parameters_Record;
      First_Moments         : Parameters_Record;
      Second_Moments        : Parameters_Record;
      First_Moment_Updates  : Moments_List;
      Second_Moment_Updates : Moments_List;
      Coef_Update           : Parameters_Record;
      Updates               : Parameters_List;
   begin
      Assert (not Self.First_Moments.Is_Empty, Routine_Name &
                "Self.First_Moments Is_Empty.");
      Assert (not Self.Second_Moments.Is_Empty, Routine_Name &
                "Self.Second_Moments Is_Empty.");
      Assert (not Grads.Is_Empty, Routine_Name & "Grads Is_Empty.");

      Self.Time_Step := Self.Time_Step + 1;
      --  L279 Update learning rate
      Self.Learning_Rate := Sqrt
        (1.0 - Self.Beta_2 ** Self.Time_Step) * Self.Initial_Learning_Rate /
        (1.0 - Self.Beta_1 ** Self.Time_Step);

      for layer in Grads.First_Index .. Grads.Last_Index loop
         Layer_Grads := Grads (layer);
         First_Moments := Self.First_Moments (layer);
         Second_Moments := Self.Second_Moments (layer);
         Put_Line (Routine_Name & "First_Moments Coeff_Params length" &
                     Count_Type'Image (First_Moments.Coeff_Params.Length));
         Put_Line (Routine_Name & "First_Moments Coeff_Params (1) length" &
                     Count_Type'Image (First_Moments.Coeff_Params (1).Length));
         Put_Line (Routine_Name & "First_Moments Intercept_Params length" &
                     Count_Type'Image (First_Moments.Intercept_Params.Length));
         Put_Line (Routine_Name & "Layer_Grads Coeff_Params length" &
                     Count_Type'Image (Layer_Grads.Coeff_Params.Length));
         Put_Line (Routine_Name & "Layer_Grads Coeff_Params (1) length" &
                     Count_Type'Image (Layer_Grads.Coeff_Params (1).Length));
         Put_Line (Routine_Name & "Layer_Grads Intercept_Params length" &
                     Count_Type'Image (Layer_Grads.Intercept_Params.Length));

         --  L272
         Update_First_Moments := Self.Beta_1 * First_Moments  +
             (1.0 - Self.Beta_1) * Layer_Grads;

         Put_Line (Routine_Name & "L276");
         --  L276
         Update_Second_Moments := Self.Beta_2 * Second_Moments +
           (1.0 - Self.Beta_2) * (Layer_Grads ** 2);

         First_Moment_Updates.Append (Update_First_Moments);
         Second_Moment_Updates.Append (Update_Second_Moments);
      end loop;

      Self.First_Moments := First_Moment_Updates;
      Self.Second_Moments := Second_Moment_Updates;

      for layer in First_Moment_Updates.First_Index ..
        First_Moment_Updates.Last_Index loop
         --  L284
         Update_First_Moments := First_Moment_Updates (layer);
         Update_Second_Moments := Second_Moment_Updates (layer);
         Coef_Update := - Self.Learning_Rate * Update_First_Moments /
           Moments_Sqrt (Update_Second_Moments, Self.Epsilon);
         Updates.Append (Coef_Update);
      end loop;

      return Updates;

   end Get_Adam_Updates;

   --  -------------------------------------------------------------------------

   --  L169
   function Get_SGD_Updates
     (Self : in out SGD_Optimizer; Grads : Parameters_List)
      return Parameters_List is
      Routine_Name : constant String :=
                       "Stochastic_Optimizers. ";
      Velocity     : Parameters_Record;
      M_V          : Parameters_Record;
      Layer_Grads  : Parameters_Record;
      Updates      : Parameters_List;

      procedure Do_Update is
      begin
         for layer in Self.Velocities.First_Index ..
           Self.Velocities.Last_Index loop
            Velocity := Self.Velocities (layer);
            Layer_Grads := Grads (layer);
            M_V := Self.Momentum * Velocity - Self.Learning_Rate * Layer_Grads;
            Updates.Append (M_V);
         end loop;
      end Do_Update;

   begin
      Assert (not Self.Velocities.Is_Empty, Routine_Name &
                "Self.Velocities Is_Empty");
      Do_Update;

      Self.Velocities := Updates;

      if Self.Use_Nesterov then
         Updates.Clear;
         Do_Update;  --  again, with updated Self.Velocities
      end if;

      return Updates;

   end Get_SGD_Updates;

   --  -------------------------------------------------------------------------

   function Moments_Sqrt (M : Parameters_Record; Epsilon : Float := 0.0)
                          return Parameters_Record is
      use Maths.Float_Math_Functions;
      Result : Parameters_Record := M;
   begin
      for index in Result.Coeff_Params.First_Index ..
        Result.Coeff_Params.Last_Index loop
         for index2 in Result.Coeff_Params (index).First_Index ..
           Result.Coeff_Params (index).Last_Index loop
            Result.Coeff_Params (index) (index2) :=
              Sqrt (Result.Coeff_Params (index) (index2)) + Epsilon;
         end loop;
         Result.Intercept_Params (index) :=
           Sqrt (Result.Intercept_Params (index)) + Epsilon;
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

   procedure Zero_Init (Params : in out Parameters_List) is
      Routine_Name : constant String :=
                       "Stochastic_Optimizers.Zero_Init ";
      Data      : Parameters_Record;
      Coeffs_2D : Float_List_2D;
      Coeffs    : Float_List;
   begin
      for index in Params.First_Index .. Params.Last_Index loop
         Data := Params (index);
         Coeffs_2D := Data.Coeff_Params;
         for index2 in Coeffs_2D.First_Index .. Coeffs_2D.Last_Index loop
            Coeffs := Coeffs_2D (index2);
            for index3 in Coeffs.First_Index .. Coeffs.Last_Index loop
               Coeffs.Replace_Element (index3, 0.0);
            end loop;
            Coeffs_2D.Replace_Element (index2, Coeffs);
         end loop;

         Data.Coeff_Params := Coeffs_2D;
         Data.Intercept_Params.Replace_Element (index, 0.0);
         Params.Replace_Element (index, Data);
      end loop;
      Put_Line (Routine_Name & "Params length" &
                  Integer'Image (Integer (Params.Length)));

   end Zero_Init;

   --  -------------------------------------------------------------------------

end Stochastic_Optimizers;
