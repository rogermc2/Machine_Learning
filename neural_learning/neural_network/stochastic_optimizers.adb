--  Based on scikit-learn/sklearn/neural_network/_stochastic_optimizers.py

with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

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

   function "-" (L, R : Parameters_Record) return Parameters_Record is
      Minus : Parameters_Record := L;
   begin
      for row in Minus.Coeff_Gradients'Range loop
         for col in Minus.Coeff_Gradients'Range (2) loop
            Minus.Coeff_Gradients (row, col) :=
              Minus.Coeff_Gradients (row, col) - R.Coeff_Gradients (row, col);
         end loop;
      end loop;

      for row in Minus.Intercept_Grads'Range loop
         Minus.Intercept_Grads (row) :=
           Minus.Intercept_Grads (row) - R.Intercept_Grads (row);
      end loop;

      return Minus;

   end "-";
   pragma Inline ("-");

   --  ------------------------------------------------------------------------

   function "*" (L : Float; R : Parameters_Record) return Parameters_Record is
      Product : Parameters_Record := R;
   begin
      for row in Product.Coeff_Gradients'Range loop
         for col in Product.Coeff_Gradients'Range (2) loop
            Product.Coeff_Gradients (row, col) :=
              L * Product.Coeff_Gradients (row, col);
         end loop;
      end loop;

      for row in Product.Intercept_Grads'Range loop
         Product.Intercept_Grads (row) := L * Product.Intercept_Grads (row);
      end loop;

      return Product;

   end "*";
   pragma Inline ("*");

   --  ------------------------------------------------------------------------

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

   function Sqrt (Rec : Parameters_Record) return Parameters_Record is
      use Maths.Float_Math_Functions;
      Result : Parameters_Record := Rec;
   begin
      for row in Rec.Coeff_Gradients'Range loop
         for col in Rec.Coeff_Gradients'Range (2) loop
            Result.Coeff_Gradients (row, col) :=
              Sqrt (Rec.Coeff_Gradients (row, col));
         end loop;
      end loop;

      for row in Rec.Intercept_Grads'Range loop
         Result.Intercept_Grads (row) := Sqrt (Rec.Intercept_Grads (row));
      end loop;

      return Result;

   end Sqrt;

   --  -------------------------------------------------------------------------

   function "/" (L, R : Parameters_Record) return Parameters_Record is
      Result  : Parameters_Record := L;
   begin
      for row in Result.Coeff_Gradients'Range loop
         for col in Result.Coeff_Gradients'Range (2) loop
            Result.Coeff_Gradients (row, col) :=
              Result.Coeff_Gradients (row, col) / R.Coeff_Gradients (row, col);
         end loop;
      end loop;

      for row in Result.Intercept_Grads'Range loop
         Result.Intercept_Grads (row) :=
           Result.Intercept_Grads (row) / R.Intercept_Grads (row);
      end loop;

      return Result;

   end "/";
   pragma Inline ("/");

   --  -------------------------------------------------------------------------

   procedure C_Init (Self                  : out Adam_Optimizer;
                     --  Coeff_Params: layers x features x values
                     --  Intercept_Params: laysers x values
                     Params                : Parameters_List;
                     Initial_Learning_Rate : Float := 0.1;
                     Beta_1                : Float := 0.9;
                     Beta_2                : Float := 0.999;
                     Epsilon               : Float) is
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
     (Self : in out SGD_Optimizer; Grads : Parameters_List)
       return Parameters_List is
      --          Routine_Name : constant String :=
      --                           "Stochastic_Optimizers. ";
      Velocity     : Parameters_Record := Self.Velocities.First_Element;
      M_V          : Parameters_Record := Self.Velocities.First_Element;
      Layer_Grads  : Parameters_Record := Grads.First_Element;
      Updates      : Parameters_List;

      procedure Do_Update is
         use Parameters_Package;
         Grads_Cursor : Parameters_Package.Cursor := Grads.First;
      begin
         for layer of Self.Velocities loop
            Velocity := layer;
            Layer_Grads := Grads (Grads_Cursor);
            M_V := Self.Momentum * Velocity - Self.Learning_Rate * Layer_Grads;
            Updates.Append (M_V);
            Next (Grads_Cursor);
         end loop;
      end Do_Update;

   begin
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
         when Optimizer_Adam =>
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
   procedure Update_Params (Self   : in out Optimizer_Record;
                            Params : in out Parameters_List;
                            Grads  : Parameters_List) is
      --          Routine_Name : constant String :=
      --                             "Stochastic_Optimizers.Update_Params ";
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

      --  L44
      Params := Params + Updates;

   end Update_Params;

   --  -------------------------------------------------------------------------

   procedure Update_Params (Self   : in out Adam_Optimizer;
                            Grads  : Parameters_List;
                            Params : in out Parameters_List) is
      --          Routine_Name : constant String := "Stochastic_Optimizers.Update_Params Adam ";

   begin
      Params := Params + Get_Adam_Updates (Self, Grads);

   end Update_Params;

   --  -------------------------------------------------------------------------

   procedure Zero_Init (Params : in out Parameters_List) is
      --          Routine_Name : constant String :=
      --                           "Stochastic_Optimizers.Zero_Init ";
   begin
      for index of Params loop
         declare
            Data                               : constant Parameters_Record := index;
            Coeffs                             : constant Real_Float_Matrix
              (1 .. Data.Coeff_Gradients'Length,
               1 .. Data.Coeff_Gradients'Length (2)) :=
                                                   (others => (others => 0.0));
            Intercepts                         : constant Real_Float_Vector
              (1 .. Data.Intercept_Grads'Length) := (others => 0.0);
         begin
            index.Coeff_Gradients := Coeffs;
            index.Intercept_Grads := Intercepts;
         end;
      end loop;

   end Zero_Init;

   --  -------------------------------------------------------------------------

end Stochastic_Optimizers;
