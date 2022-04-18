--  Based on scikit-learn/sklearn/neural_network/_stochastic_optimizers.py

with Ada.Assertions; use Ada.Assertions;
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
        Sum_Rec : Parameters_Record := L;
    begin
        Sum_Rec.Coeff_Grads := L.Coeff_Grads + R.Coeff_Grads;
        Sum_Rec.Intercept_Grads := L.Intercept_Grads + R.Intercept_Grads;

        return Sum_Rec;

    end "+";
    pragma Inline ("+");

    --  ------------------------------------------------------------------------

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
        Minus : Parameters_Record := M;
    begin
        for row in Minus.Coeff_Grads'Range loop
            for col in Minus.Coeff_Grads'Range (2) loop
                Minus.Coeff_Grads (row, col) := - Minus.Coeff_Grads (row, col);
            end loop;
            Minus.Intercept_Grads (row) := - Minus.Intercept_Grads (row);

        end loop;

        return Minus;

    end "-";
    pragma Inline ("-");

    --  ------------------------------------------------------------------------

    function "-" (L, R : Parameters_Record) return Parameters_Record is
        Minus : Parameters_Record := L;
    begin
        for row in Minus.Coeff_Grads'Range loop
            for col in Minus.Coeff_Grads'Range (2) loop
                Minus.Coeff_Grads (row, col) :=
                  Minus.Coeff_Grads (row, col) - R.Coeff_Grads (row, col);
            end loop;
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
        for row in Product.Coeff_Grads'Range loop
            for col in Product.Coeff_Grads'Range (2) loop
                Product.Coeff_Grads (row, col) :=
                  L * Product.Coeff_Grads (row, col);
            end loop;
            Product.Intercept_Grads (row) :=
              L * Product.Intercept_Grads (row);
        end loop;

        return Product;

    end "*";
    pragma Inline ("*");

    --  ------------------------------------------------------------------------

    function "**" (Rec : Parameters_Record; P : Integer)
                  return Parameters_Record is
        Result : Parameters_Record := Rec;
    begin
        Result.Coeff_Grads := Rec.Coeff_Grads ** P;
        Result.Intercept_Grads := Rec.Intercept_Grads ** P;

        return Result;

    end "**";

    --  -------------------------------------------------------------------------

    function "/" (L, R : Parameters_Record) return Parameters_Record is
        Result  : Parameters_Record := L;
    begin
        for row in Result.Coeff_Grads'Range loop
            for col in Result.Coeff_Grads'Range (2) loop
                Result.Coeff_Grads (row, col) :=
                  Result.Coeff_Grads (row, col) / R.Coeff_Grads  (row, col);
            end loop;
            Result.Intercept_Grads (row) :=
              Result.Intercept_Grads (row) / R.Intercept_Grads  (row);
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
        Layer_Grads           : Parameters_Record := Grads (1);
        Update_First_Moments  : Parameters_Record := Self.First_Moments (1);
        Update_Second_Moments : Parameters_Record := Self.Second_Moments (1);
        First_Moments         : Parameters_Record := Self.First_Moments (1);
        Second_Moments        : Parameters_Record := Self.Second_Moments (1);
        First_Moment_Updates  : Moments_List;
        Second_Moment_Updates : Moments_List;
        Coef_Update           : Parameters_Record := Self.Params (1);
        Updates               : Parameters_List;
    begin
        Put_Line (Routine_Name);
        Layer_Grads := Grads (1);
        Update_First_Moments := Self.First_Moments (1);
        Update_Second_Moments := Self.Second_Moments (1);
        First_Moments := Self.First_Moments (1);
        Second_Moments := Self.Second_Moments (1);
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
            --           Put_Line (Routine_Name & "First_Moments Coeff_Params length" &
            --                       Count_Type'Image (First_Moments.Coeff_Params.Length));
            --           Put_Line (Routine_Name & "First_Moments Coeff_Params (1) length" &
            --                       Count_Type'Image (First_Moments.Coeff_Params (1).Length));
            --           Put_Line (Routine_Name & "First_Moments Intercept_Params length" &
            --                       Count_Type'Image (First_Moments.Intercept_Params.Length));
            --           Put_Line (Routine_Name & "Layer_Grads Coeff_Params length" &
            --                       Count_Type'Image (Layer_Grads.Coeff_Params.Length));
            --           Put_Line (Routine_Name & "Layer_Grads Coeff_Params (1) length" &
            --                       Count_Type'Image (Layer_Grads.Coeff_Params (1).Length));
            Put_Line (Routine_Name & "L272 Layer_Grads Intercept_Params length" &
                      Count_Type'Image (Layer_Grads.Intercept_Grads'Length));

            --  L272
            Update_First_Moments := Self.Beta_1 * First_Moments  +
              (1.0 - Self.Beta_1) * Layer_Grads;

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
            --           Put_Line (Routine_Name & "layer:" &
            --                       Integer'Image (layer));
            --  L284
            Update_First_Moments := First_Moment_Updates (layer);
            Update_Second_Moments := Second_Moment_Updates (layer);
            --           Put_Line (Routine_Name & "Update_Second_Moments Coeff_Params length" &
            --                       Count_Type'Image (Update_Second_Moments.Coeff_Params.Length));
            --           Put_Line (Routine_Name & "Update_Second_Moments Coeff_Params (1) length" &
            --                       Count_Type'Image (Update_Second_Moments.Coeff_Params (1).Length));
            --           Put_Line (Routine_Name & "Update_Second_Moments Intercept_Params length" &
            --                       Count_Type'Image (Update_Second_Moments.Intercept_Params.Length));
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
        Velocity     : Parameters_Record := Self.Velocities (1);
        M_V          : Parameters_Record := Self.Velocities (1);
        Layer_Grads  : Parameters_Record := Grads (1);
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
        for row in Result.Coeff_Grads'Range loop
            for col in Result.Coeff_Grads'Range (2) loop
                Result.Coeff_Grads (row, col) :=
                  Sqrt (Result.Coeff_Grads (row, col)) + Epsilon;
            end loop;
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
                Put_Line (Routine_Name & "Get_Adam_Updates");
                Updates := Get_Adam_Updates (Self.Adam, Grads);
            when Optimizer_SGD =>
                Updates := Get_SGD_Updates (Self.SGD, Grads);
            when No_Optimizer => null;
        end case;

        Put_Line (Routine_Name & "L44");
        --  L44
        Params := Params + Updates;
        Put_Line (Routine_Name & "done");

    end Update_Params;

    --  -------------------------------------------------------------------------

    procedure Zero_Init (Params : in out Parameters_List) is
    --          Routine_Name : constant String :=
    --                           "Stochastic_Optimizers.Zero_Init ";
    begin
        for index in Params.First_Index .. Params.Last_Index loop
            declare
                Data    : constant Parameters_Record := Params (index);
                Coeffs  : constant Float_Matrix
                  (1 .. Data.Coeff_Grads'Length,
                   1 .. Data.Coeff_Grads'Length (2)) :=
                            (others => (others => 0.0));
                Intercepts  : constant Float_Array
                  (1 .. Data.Intercept_Grads'Length) := (others => 0.0);
            begin
                Params (index).Coeff_Grads := Coeffs;
                Params (index).Intercept_Grads := Intercepts;
            end;
        end loop;

    end Zero_Init;

    --  -------------------------------------------------------------------------

end Stochastic_Optimizers;
