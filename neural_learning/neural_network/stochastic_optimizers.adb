--  Based on scikit-learn/sklearn/neural_network/_stochastic_optimizers.py

with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;
with Ada.Numerics.Elementary_Functions;

--  with Printing;

package body Stochastic_Optimizers is

   function Moments_Sqrt (M : Parameters_Record; Epsilon : Float := 0.0)
                           return Parameters_Record;
   pragma Inline (Moments_Sqrt);
   procedure Zero_Init (Params : in out Parameters_List);

   --  -------------------------------------------------------------------------

   function "+" (L, R : Parameters_Record) return Parameters_Record is
      use Real_Float_Arrays;
      Sum_Rec : Parameters_Record := L;
   begin
      Sum_Rec.Coeff_Gradients := L.Coeff_Gradients + R.Coeff_Gradients;
      Sum_Rec.Intercept_Grads := L.Intercept_Grads + R.Intercept_Grads;

      return Sum_Rec;

   end "+";
   pragma Inline ("+");

   --  ------------------------------------------------------------------------

   function "+" (L, R : Parameters_List) return Parameters_List is
      use Parameters_Package;
      Sum    : Parameters_List;
      L_Curs : Cursor := L.First;
      R_Curs : Cursor := R.First;
   begin
      while Has_Element (L_Curs) loop
         Sum.Append (L (L_Curs) + R (R_Curs));
         Next (L_Curs);
         Next (R_Curs);
      end loop;

      return Sum;

   end "+";

   --  -------------------------------------------------------------------------

   function "+" (L : Real_Float_Matrix; R : Real_Float_Vector)
                 return Coeffs_Matrix is
      Sum : Coeffs_Matrix (L'Range, L'Range (2));
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Sum (row, col) := L (row, col) + R (col);
         end loop;
      end loop;

      return Sum;

   end "+";

   --  -------------------------------------------------------------------------

   function "-" (M : Parameters_Record) return Parameters_Record is
      Minus : Parameters_Record := M;
   begin
      for row in Minus.Coeff_Gradients'Range loop
         for col in Minus.Coeff_Gradients'Range (2) loop
            Minus.Coeff_Gradients (row, col) :=
              - Minus.Coeff_Gradients (row, col);
         end loop;
      end loop;

      for row in Minus.Intercept_Grads'Range loop
         Minus.Intercept_Grads (row) := - Minus.Intercept_Grads (row);
      end loop;

      return Minus;

   end "-";
   pragma Inline ("-");

   --  ------------------------------------------------------------------------
   --  Iteration_Ends perform updates to learning rate and potential other
   --  states at the end of an iteration
   procedure Iteration_Ends (Self      : in out SGD_Optimizer;
                             Time_Step : Integer) is
      use Ada.Numerics.Elementary_Functions;
   begin
      if Self.LR_Schedule = Invscaling_LR_Schedule then
         Self.Learning_Rate := Self.Initial_Learning_Rate /
            (Float(Time_Step + 1) ** Self.Power_T);
      end if;
   end Iteration_Ends;

   --  ------------------------------------------------------------------------

--     function Pack (Params : Parameters_List) return Real_Float_Vector is
--        use Coeffs_Package;
--        type Coeff_Vector is array (Positive range <>) of Float;
--        --        Routine_Name   : constant String := "Multilayer_Perceptron.Pack ";
--        Coeffs_Ints  : Coeffs_List;
--        Result_Size  : Natural := 0;
--
--        function Flatten (Mat : Coeffs_Matrix) return Coeff_Vector is
--           Result : Coeff_Vector (1 .. Mat'Length * Mat'Length (2));
--        begin
--           for row in Mat'Range loop
--              for col in Mat'Range (2) loop
--                 Result ((row - 1) * Mat'Length (2) + col) :=
--                   Mat (row, col);
--              end loop;
--           end loop;
--
--           return Result;
--
--        end Flatten;
--
--     begin
--        for index in Params.First_Index .. Params.Last_Index loop
--           Coeffs_Ints.Append (Params.Element (index).Coeff_Gradients +
--                                 Params.Element (index).Intercept_Grads);
--           Result_Size := Result_Size + Coeffs_Ints.Last_Element'Length *
--             Coeffs_Ints.Last_Element'Length (2);
--        end loop;
--
--        declare
--           Result       : Real_Float_Vector (1 .. Result_Size);
--           Result_Index : Natural := 0;
--        begin
--           for index in Coeffs_Ints.First_Index .. Coeffs_Ints.Last_Index loop
--              declare
--                 Coeff_Mat : constant Coeffs_Matrix := Coeffs_Ints (index);
--                 Coeff_Vec : constant Coeff_Vector := Flatten (Coeff_Mat);
--              begin
--                 for index in Coeff_Vec'Range loop
--                    Result_Index := Result_Index + 1;
--                    Result (Result_Index) := Coeff_Vec (index);
--                 end loop;
--              end;
--           end loop;
--
--           return Result;
--        end;
--
--     end Pack;

   --  -------------------------------------------------------------------------

   function Square (Rec : Parameters_Record) return Parameters_Record is
      Result : Parameters_Record := Rec;
   begin
      for row in Rec.Coeff_Gradients'Range loop
         for col in Rec.Coeff_Gradients'Range (2) loop
            Result.Coeff_Gradients (row, col) :=
              Rec.Coeff_Gradients (row, col) ** 2;
         end loop;
      end loop;

      for row in Rec.Intercept_Grads'Range loop
         Result.Intercept_Grads (row) := Rec.Intercept_Grads (row) ** 2;
      end loop;

      return Result;

   end Square;

   --  -------------------------------------------------------------------------

   function Sqrt (Rec : Parameters_Record; Epsilon : Float := 0.0)
                   return Parameters_Record is
      use Maths.Float_Math_Functions;
      Result : Parameters_Record := Rec;
   begin
      for row in Rec.Coeff_Gradients'Range loop
         for col in Rec.Coeff_Gradients'Range (2) loop
            Result.Coeff_Gradients (row, col) :=
              Sqrt (Rec.Coeff_Gradients (row, col)) + Epsilon;
         end loop;
      end loop;

      for row in Rec.Intercept_Grads'Range loop
         Result.Intercept_Grads (row) := Sqrt (Rec.Intercept_Grads (row));
      end loop;

      return Result;

   end Sqrt;

   --  -------------------------------------------------------------------------

   function "*" (L : Float; R : Parameters_Record) return Parameters_Record is
      Result  : Parameters_Record := R;
   begin
      for row in Result.Coeff_Gradients'Range loop
         for col in Result.Coeff_Gradients'Range (2) loop
            Result.Coeff_Gradients (row, col) :=
              L * R.Coeff_Gradients (row, col);
         end loop;
      end loop;

      for row in Result.Intercept_Grads'Range loop
         Result.Intercept_Grads (row) := L * R.Intercept_Grads (row);
      end loop;

      return Result;

   end "*";

   --  -------------------------------------------------------------------------

   function "/" (L, R : Parameters_Record) return Parameters_Record is
      Result  : Parameters_Record := L;
   begin
      for row in Result.Coeff_Gradients'Range loop
         for col in Result.Coeff_Gradients'Range (2) loop
            Result.Coeff_Gradients (row, col) :=
              Result.Coeff_Gradients (row, col) /
              R.Coeff_Gradients (row, col);
         end loop;
      end loop;

      for row in Result.Intercept_Grads'Range loop
         Result.Intercept_Grads (row) :=
           Result.Intercept_Grads (row) / R.Intercept_Grads (row);
      end loop;

      return Result;

   end "/";

   --  -------------------------------------------------------------------------

   function "/" (L : Parameters_Record; R : Float) return Parameters_Record is
      Result  : Parameters_Record := L;
   begin
      for row in Result.Coeff_Gradients'Range loop
         for col in Result.Coeff_Gradients'Range (2) loop
            Result.Coeff_Gradients (row, col) :=
              Result.Coeff_Gradients (row, col) / R;
         end loop;
      end loop;

      for row in Result.Intercept_Grads'Range loop
         Result.Intercept_Grads (row) :=
           Result.Intercept_Grads (row) / R;
      end loop;

      return Result;

   end "/";

   --  -------------------------------------------------------------------------

   function "-" (L, R : Parameters_Record) return Parameters_Record is
      Result  : Parameters_Record := L;
   begin
      for row in Result.Coeff_Gradients'Range loop
         for col in Result.Coeff_Gradients'Range (2) loop
            Result.Coeff_Gradients (row, col) :=
              Result.Coeff_Gradients (row, col) - R.Coeff_Gradients (row, col);
         end loop;
      end loop;

      for row in Result.Intercept_Grads'Range loop
         Result.Intercept_Grads (row) :=
           Result.Intercept_Grads (row) - R.Intercept_Grads (row);
      end loop;

      return Result;

   end "-";

   --  -------------------------------------------------------------------------

   procedure C_Init (Self                  : out Adam_Optimizer;
                     --  Coeff_Params: layers x features x values
                     --  Intercept_Params: laysers x values
                     Params                : Parameters_List;
                     Initial_Learning_Rate : Float := 0.1;
                     Beta_1                : Float := 0.9;
                     Beta_2                : Float := 0.999;
                     Epsilon               : Float := 10.0 ** (-8)) is
      --          Routine_Name : constant String := "Stochastic_Optimizers.C_Init Adam ";
   begin
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

   procedure C_Init (Self                  : out Base_Optimizer;
                     Initial_Learning_Rate : Float := 0.1) is
   begin
      Self.Initial_Learning_Rate := Initial_Learning_Rate;
      Self.Learning_Rate := Initial_Learning_Rate;

   end C_Init;

   --  -------------------------------------------------------------------------

   procedure C_Init (Self                  : out SGD_Optimizer;
                     --  Coeff_Params: layers x features x values
                     --  Intercept_Params: layers x values
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
      use Maths.Float_Math_Functions;
      use Parameters_Package;
      --          Routine_Name          : constant String :=
      --                                    "Stochastic_Optimizers.Get_Adam_Updates ";
      First_Moment_Updates  : Moments_List;
      Second_Moment_Updates : Moments_List;
      F_Cursor              : Cursor := Self.First_Moments.First;
      S_Cursor              : Cursor := Self.Second_Moments.First;
      Updates               : Parameters_List;
   begin
      Self.Time_Step := Self.Time_Step + 1;
      --  L279 Update learning rate
      Self.Learning_Rate := Sqrt
        (1.0 - Self.Beta_2 ** Self.Time_Step) * Self.Initial_Learning_Rate /
        (1.0 - Self.Beta_1 ** Self.Time_Step);
      --  L272
      --  "of" implies that layer is a cursor
      for layer of Grads loop
         declare
            Layer_Grads           : constant Parameters_Record := layer;
            First_Moments         : constant Parameters_Record :=
                                      Self.First_Moments (F_Cursor);
            Second_Moments        : constant Parameters_Record :=
                                      Self.Second_Moments (S_Cursor);
            Update_First_Moments  : Parameters_Record :=
                                      Self.Beta_1 * First_Moments;
            --  L276
            Update_Second_Moments : Parameters_Record :=
                                      Self.Beta_2 * Second_Moments;
         begin
            Update_First_Moments := Update_First_Moments +
              (1.0 - Self.Beta_1) * Layer_Grads;
            Update_Second_Moments := Update_Second_Moments +
              (1.0 - Self.Beta_2) * Square (Layer_Grads);
            First_Moment_Updates.Append (Update_First_Moments);
            Second_Moment_Updates.Append (Update_Second_Moments);
         end;  --  declare
         Next (F_Cursor);
         Next (S_Cursor);
      end loop;

      Self.First_Moments := First_Moment_Updates;
      Self.Second_Moments := Second_Moment_Updates;

      F_Cursor := First_Moment_Updates.First;
      S_Cursor := Second_Moment_Updates.First;
      for layer of Self.Params loop
         declare
            --  L284
            Update_First_Moments  : constant Parameters_Record :=
                                      First_Moment_Updates (F_Cursor);
            Update_Second_Moments : constant Parameters_Record :=
                                      Second_Moment_Updates (S_Cursor);
            Coef_Update           : Parameters_Record := layer;
         begin
            Coef_Update := - Self.Learning_Rate * Update_First_Moments /
              Moments_Sqrt (Update_Second_Moments, Self.Epsilon);
            Updates.Append (Coef_Update);
         end;  --  declare
         Next (F_Cursor);
         Next (S_Cursor);
      end loop;

      return Updates;

   end Get_Adam_Updates;

   --  -------------------------------------------------------------------------

   --  L169
   function Get_SGD_Updates
     (Self : in out SGD_Optimizer; Gradients : Parameters_List)
       return Parameters_List is
      --          Routine_Name : constant String :=
      --                           "Stochastic_Optimizers.Get_SGD_Updates ";
      Updates      : Parameters_List;

      procedure Do_Update (Layer : Positive) is
         Velocity    : constant Parameters_Record := Self.Velocities (layer);
         Layer_Grads : constant Parameters_Record := Gradients (layer);
         M_V         : Parameters_Record := Self.Velocities (layer);
      begin
         M_V := Self.Momentum * Velocity - Self.Learning_Rate * Layer_Grads;
         Updates.Append (M_V);

      end Do_Update;

   begin
      for layer in Self.Velocities.First_Index ..
        Self.Velocities.Last_Index loop
         Do_Update (layer);
      end loop;

      Self.Velocities := Updates;

      if Self.Use_Nesterov then
         Updates.Clear;
         for layer in Self.Velocities.First_Index ..
           Self.Velocities.Last_Index loop
            Do_Update (layer);  --  again, with updated Self.Velocities
         end loop;
      end if;

      return Updates;

   end Get_SGD_Updates;

   --  -------------------------------------------------------------------------

   function Moments_Sqrt (M : Parameters_Record; Epsilon : Float := 0.0)
                           return Parameters_Record is
      use Maths.Float_Math_Functions;
      Result : Parameters_Record := M;
   begin
      for row in Result.Coeff_Gradients'Range loop
         for col in Result.Coeff_Gradients'Range (2) loop
            Result.Coeff_Gradients (row, col) :=
              Sqrt (Result.Coeff_Gradients (row, col)) + Epsilon;
         end loop;
      end loop;

      for row in Result.Intercept_Grads'Range loop
         Result.Intercept_Grads (row) :=
           Sqrt (Result.Intercept_Grads (row)) + Epsilon;
      end loop;

      return Result;

   end Moments_Sqrt;

   --  -------------------------------------------------------------------------
   --  L52  Trigger_Stopping decides whether or not it's time to stop training
   function Trigger_Stopping (Self    : in out Optimizer_Record; Msg : String;
                              Verbose : Boolean) return Boolean is
      --          Routine_Name : constant String :=
      --                           "Stochastic_Optimizers.Trigger_Stopping ";
      Result : Boolean := True;
   begin
      case Self.Kind is
         when Optimizer_Adam | Optimizer_Base =>
            if Verbose then
               Put_Line (Msg & " stopping.");
            end if;

         when Optimizer_SGD =>
            if Self.SGD.LR_Schedule = Adaptive_LR_Schedule then
               Result := Self.SGD.Learning_Rate > 10.0 ** (-6);
               if Result then
                  Self.SGD.Learning_Rate := Self.SGD.Learning_Rate  / 5.0;
                  Result := False;
                  if Verbose then
                     Put_Line (Msg & " Learning rate set to " &
                                 Float'Image (Self.SGD.Learning_Rate));
                  end if;
               else
                  if Verbose then
                     Put_Line ("Learning rate too small stopping.");
                  end if;
               end if;
            else
               if Verbose then
                  Put_Line (Msg & " stopping.");
               end if;
            end if;

         when No_Optimizer => null;
      end case;

      return Result;

   end Trigger_Stopping;

   --  -------------------------------------------------------------------------
   --  L29  Update_Params updates parameters with given gradients
   procedure Update_Params (Self      : in out Optimizer_Record;
                            Params    : in out Parameters_List;
                            Gradients : Parameters_List) is
      --          Routine_Name : constant String :=
      --                             "Stochastic_Optimizers.Update_Params ";
      Updates      : Parameters_List;
   begin
      --  L42
      case Self.Kind is
         when Optimizer_Adam =>
            Updates := Get_Adam_Updates (Self.Adam, Gradients);
         when Optimizer_SGD =>
            Updates := Get_SGD_Updates (Self.SGD, Gradients);
         when Optimizer_Base =>
            Update_Params (Self, Updates, Gradients);
         when No_Optimizer => null;
      end case;

      --  L44
      Params := Params + Updates;

   end Update_Params;

   --  -------------------------------------------------------------------------

   procedure Update_Params (Self      : in out Adam_Optimizer;
                            Params    : in out Parameters_List;
                            Gradients : Parameters_List) is

   begin
      Params := Params + Get_Adam_Updates (Self, Gradients);

   end Update_Params;

   --  -------------------------------------------------------------------------

   procedure Update_Params (Self      : in out SGD_Optimizer;
                            Params    : in out Parameters_List;
                            Gradients : Parameters_List) is

   begin
      Params := Params + Get_SGD_Updates (Self, Gradients);

   end Update_Params;

   --  -------------------------------------------------------------------------

   procedure Zero_Init (Params : in out Parameters_List) is
      --          Routine_Name : constant String :=
      --                           "Stochastic_Optimizers.Zero_Init ";
   begin
      for index of Params loop
         declare
            Data                                  : constant Parameters_Record := index;
            Coeffs                                : constant Real_Float_Matrix
              (1 .. Data.Coeff_Gradients'Length,
               1 .. Data.Coeff_Gradients'Length (2)) :=
                                                      (others => (others => 0.0));
            Intercepts                            : constant Real_Float_Vector
              (1 .. Data.Intercept_Grads'Length) := (others => 0.0);
         begin
            index.Coeff_Gradients := Coeffs;
            index.Intercept_Grads := Intercepts;
         end;
      end loop;

   end Zero_Init;

   --  -------------------------------------------------------------------------

end Stochastic_Optimizers;
