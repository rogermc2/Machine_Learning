--  Based on scikit-learn/sklearn/neural_network/_base.py

--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Containers;
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Neural_Maths;
--  with Printing;

package body Base_Neural is

    EPS : constant Float := Float'Small;

    function X_Log_Y (X, Y : Float_Matrix) return Float_Matrix;
    pragma Inline (X_Log_Y);

    --  -------------------------------------------------------------------------

    function Binary_Log_Loss (Y_True : Integer_Matrix; Y_Prob : Float_Matrix)
                             return Float is
    --        Routine_Name : constant String :=
    --                         "Base_Neural.Binary_Log_Loss_Function ";
        YT       : constant Float_Matrix := To_Float_Matrix (Y_True);
        YP       : Float_Matrix := Y_Prob;
        X_Log_Y1 : Float_Matrix (YP'Range, YP'Range (2));
        X_Log_Y2 : Float_Matrix (YP'Range, YP'Range (2));
        Sum1     : Float := 0.0;
        Sum2     : Float := 0.0;
    begin
        --        Assert (Y_Prob'Length = Y_True'Length and
        --                  Y_Prob'Length (2) = Y_True'Length (2), Routine_Name &
        --                  "Y_Prob size" &
        --                  Integer'Image (Integer (Y_Prob'Length)) & " x" &
        --                  Integer'Image (Integer (Y_Prob'Length (2))) &
        --                  " should be the same as Y_True size" &
        --                  Integer'Image (Integer (Y_True'Length)) & " x" &
        --                  Integer'Image (Integer (Y_True'Length(2))));

        --  L226 Clip Y_Prob
        for row in YP'Range loop
            for col in YP'Range (2) loop
                if YP (row, col) < EPS then
                    YP (row, col) := EPS;
                elsif YP (row, col) > 1.0 - EPS then
                    YP (row, col) := 1.0 - EPS;
                end if;
            end loop;
        end loop;

        --  xlogy = x*log(y) so that the result is 0 if x = 0
        X_Log_Y1 := X_Log_Y (YT, YP);
        X_Log_Y2 := X_Log_Y (1.0 - YT, 1.0 - YP);

        for row in YP'Range loop
            for col in YP'Range (2) loop
                Sum1 := Sum1 + X_Log_Y1 (row, col);
                Sum2 := Sum2 + X_Log_Y2 (row, col);
            end loop;
        end loop;

        return - (Sum1 + Sum2) / Float (Y_Prob'Length);

    end Binary_Log_Loss;

    --  -------------------------------------------------------------------------

    procedure Identity (Activation : Float_Matrix) is
    begin
        null;
    end Identity;

    --  ------------------------------------------------------------------------

    procedure Identity_Derivative (Z   : Float_Matrix;
                                   Del : in out Float_Matrix) is
    begin
        null;
    end Identity_Derivative;

    --  ------------------------------------------------------------------------

    procedure Logistic (Activation : in out Float_Matrix) is
        use Maths.Float_Math_Functions;
        type Matrix_Float is new Float_Matrix (1 .. Activation'Length,
                                               1 .. Activation'Length (2));
        Sigmoid  : Matrix_Float;
    begin
        for row in Activation'Range loop
            for col in Activation'Range (2) loop
                Sigmoid (row, col) :=
                  (1.0 / (1.0 + Exp (Activation (row, col))));
            end loop;
        end loop;

        Activation := Float_Matrix (Sigmoid);

    end Logistic;

    --  -------------------------------------------------------------------------

    procedure Logistic_Derivative (Z   : Float_Matrix;
                                   Del : in out Float_Matrix) is
        type Matrix_Float is new Float_Matrix (1 .. Z'Length,
                                               1 .. Z'Length (2));
        Prod  : Matrix_Float;
    begin
        Del := Del * Z;
        for row in Z'Range loop
            for col in Z'Range (2) loop
                Prod (row, col) := 1.0 - Z (row, col);
            end loop;
        end loop;

        Del := Del * Float_Matrix (Prod);

    end Logistic_Derivative;

    --  -------------------------------------------------------------------------

    function Logistic_Sigmoid (X : Float) return Float is
        use Maths.Float_Math_Functions;
    begin
        return 1.0 / (1.0 + Exp (X));
    end Logistic_Sigmoid;

    --  ------------------------------------------------------------------------
    --  L177 Log Loss is the negative average of the log of corrected predicted
    --  probabilities for each instance.
    function Log_Loss (Y_True : Integer_Matrix; Y_Prob : Float_Matrix)
                      return Float is
--          use Ada.Containers;
--          Routine_Name : constant String := "Base_Neural.Log_Loss ";
        YT           : constant Float_Matrix := To_Float_Matrix (Y_True);
        YP           : Float_Matrix := Y_Prob;
        YT2          : Float_Matrix (YT'Range, YT'First (2) .. YT'Last (2) + 1);
        YP2          : Float_Matrix (YP'Range, YP'First (2) .. YP'Last (2) + 1);

        function Do_XlogY (Y_True, Y_Prob : Float_Matrix) return Float is
            X_Y : Float_Matrix (Y_Prob'Range, Y_Prob'Range (2));
            Sum : Float := 0.0;
        begin
            X_Y := X_Log_Y (Y_True, Y_Prob);
            for row in Y_Prob'Range loop
                for col in Y_Prob'Range (2) loop
                    Sum := Sum + X_Y (row, col);
                end loop;
            end loop;

            return - Sum / Float (Y_Prob'Length);
        end Do_XlogY;

    begin
--          Put_Line (Routine_Name);
--          Printing.Print_Float_Matrix (Routine_Name & "YP", YP, 1, 2);
--          Put_Line (Routine_Name & "YT size" & Count_Type'Image (YT'Length)
--                    & " x" & Count_Type'Image (YT'Length (2)));
        --  L194 Clip Y_Prob
        for row in YP'Range loop
            for col in YP'Range (2) loop
                if YP (row, col) < EPS then
                    YP (row, col) := EPS;
                elsif YP (row, col) > 1.0 - EPS then
                    YP (row, col) := 1.0 - EPS;
                end if;
            end loop;
        end loop;

        --  xlogy = x*log(y) so that the result is 0 if x = 0
        if YP'Length (2) = 1 then
            for row in YP'Range loop
                for col in YP'Range (2) loop
                    YP2 (row, col) := YP (row, col);
                    YP2 (row, col + 1) := 1.0 - YP (row, col);
                    YT2 (row, col) := YT (row, col);
                    YT2 (row, col + 1) := 1.0 - YT (row, col);
                end loop;
            end loop;

            return Do_XlogY (YT2, YP2);

        else
            return Do_XlogY (YT, YP);
        end if;

    end Log_Loss;

    --  -------------------------------------------------------------------------

    procedure Relu (Activation : in out Float_Matrix) is
        type Matrix_Float is new Float_Matrix (1 .. Activation'Length,
                                               1 .. Activation'Length (2));
        Result : Matrix_Float;
    begin
        for row in Activation'Range loop
            for col in Activation'Range (2) loop
                Result (row, col) := Float'Max (0.0, Activation (row, col));
            end loop;
        end loop;

        Activation := Float_Matrix (Result);

    end Relu;

    --  -------------------------------------------------------------------------

    procedure Relu_Derivative (Z : Float_Matrix; Del : in out Float_Matrix) is
    begin
        for row in Z'Range loop
            for col in Z'Range (2) loop
                if Z (row, col) = 0.0 then
                    Del (row, col) := 0.0;
                end if;
            end loop;
        end loop;

    end Relu_Derivative;

    --  -------------------------------------------------------------------------

    function Softmax (A : Float_Array) return Float_Array is
        use Maths.Float_Math_Functions;
        Sum_Exp : Float := 0.0;
        Exp_A   : Float_Array (A'Range);
        Result  : Float_Array (A'Range);
    begin
        for col in Result'Range loop
            Exp_A (col) := Exp (A (col));
            Sum_Exp := Sum_Exp + Exp_A (col);
        end loop;

        if Sum_Exp = 0.0 then
            Sum_Exp := 1.0;
        end if;

        for col in Result'Range loop
            Result (col) := Exp (A (col)) / Sum_Exp;
        end loop;

        return Result;

    end Softmax;

    --  -------------------------------------------------------------------------

    procedure Softmax (Activation : in out Float_Matrix) is
--          Routine_Name : constant String := "Base_Neural.Softmax ";
        Tmp   : Float_Matrix := Activation;
        aRow  : Float_Array (Activation'Range (2));
    begin
--          Put_Line (Routine_Name);
--        Put_Line (Routine_Name & "Activation length" &
--                   Integer'Image (Activation'Length) & " x" &
--                   Integer'Image (Activation'Length (2)));
      Tmp := Diff_Max (Activation);
--        Put_Line (Routine_Name & "Tmp length" &
--                   Integer'Image (Tmp'Length) & " x" &
--                   Integer'Image (Tmp'Length (2)));

        for row in Activation'Range loop
            aRow := Softmax (Get_Row (Tmp, row));
            for col in aRow'Range loop
                Activation (row, col) := aRow (col);
            end loop;
        end loop;

    end Softmax;

    --  -------------------------------------------------------------------------
    --  L158
    function Squared_Loss (Y_True : Integer_Matrix; Y_Pred : Float_Matrix)
                          return Float is
        YT_Float : constant Float_Matrix := To_Float_Matrix (Y_True);
    begin
        return Neural_Maths.Mean ((YT_Float - Y_Pred) ** 2) / 2.0;

    end Squared_Loss;

    --  -------------------------------------------------------------------------

    procedure Tanh (Activation : in out Float_Matrix) is
        use Maths.Float_Math_Functions;
        type Matrix_Float is new Float_Matrix (1 .. Activation'Length,
                                               1 .. Activation'Length (2));
        Result   : Matrix_Float;
    begin
        for row in Activation'Range loop
            for col in Activation'Range (2) loop
                Result (row, col) := Tanh (Activation (row, col));
            end loop;
        end loop;

        Activation := Float_Matrix (Result);

    end Tanh;

    --  -------------------------------------------------------------------------

    procedure Tanh_Derivative (Z   : Float_Matrix;
                               Del : in out Float_Matrix) is
        type Matrix_Float is new Float_Matrix (1 .. Z'Length,
                                               1 .. Z'Length (2));
        Del_2  : Matrix_Float;
    begin
        for row in Z'Range loop
            for col in Z'Range (2) loop
                Del_2 (row, col) := 1.0 - Z (row, col) ** 2;
            end loop;
        end loop;

        Del := Del * Float_Matrix (Del_2);

    end Tanh_Derivative;

    --  -------------------------------------------------------------------------
    --  scipy/special/_xlogy.pxd
    --  xlogy = x*log(y) so that the result is 0 if x = 0
    function X_Log_Y (X, Y : Float_Matrix) return Float_Matrix is
        use Maths.Float_Math_Functions;
        Y1     : Float_Matrix := Y;
        Result : Float_Matrix (X'Range, x'Range (2));
    begin
        for row in X'Range loop
            for col in X'Range (2) loop
                if X (row, col) /= 0.0 then
                    if Y1 (row, col) < EPS then
                        Y1 (row, col) := EPS;
                    end if;
                    Result (row, col) := X (row, col) * Log (Y1 (row, col));
                end if;
            end loop;
        end loop;

        return Result;

    end X_Log_Y;

    --  -------------------------------------------------------------------------

end Base_Neural;
