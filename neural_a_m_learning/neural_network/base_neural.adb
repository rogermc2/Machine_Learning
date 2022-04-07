--  Based on scikit-learn/sklearn/neural_network/_base.py

--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Classifier_Utilities;
with Neural_Maths;
--  with Printing;

package body Base_Neural is

    EPS : constant Float := Float'Small;

    function X_Log_Y (X, Y : Float) return Float;
    pragma Inline (X_Log_Y);

    --  -------------------------------------------------------------------------

    function Binary_Log_Loss (Y_True : Integer_Matrix; Y_Prob : Float_Matrix)
                              return Float is
    --        Routine_Name : constant String :=
    --                         "Base_Neural.Binary_Log_Loss_Function ";
        type Matrix_Float is new Float_Matrix (1 .. Y_Prob'Length,
                                               1 .. Y_Prob'Length (2));
        Y_P      : Float;
        YP_Float : Matrix_Float := Matrix_Float (Y_Prob);
        YT_Float : Matrix_Float;
        X_Log_Y1 : Matrix_Float;
        X_Log_Y2 : Matrix_Float;
        Sum1     : Float := 0.0;
        Sum2     : Float := 0.0;
    begin
        --  L226 Clip Y_Prob
        for row in Y_Prob'First .. Y_Prob'Last loop
            for col in Y_Prob'First (2) .. Y_Prob'Last (2) loop
                YT_Float (row, col) := Float (Y_True (row, col));
                Y_P := Y_Prob (row, col);
                if Y_P < EPS then
                    YP_Float (row, col) := EPS;
                elsif Y_P > 1.0 - EPS then
                    YP_Float (row, col) := 1.0 - EPS;
                end if;
            end loop;
        end loop;

        --  xlogy = x*log(y) so that the result is 0 if x = 0
        for row in Y_Prob'First .. Y_Prob'Last loop
            for col in Y_Prob'First (2) .. Y_Prob'Last (2) loop
                X_Log_Y1 (row, col) :=
                  X_Log_Y (YT_Float (row, col), YP_Float (row, col));
                X_Log_Y2 (row, col) :=
                  X_Log_Y (1.0 - YT_Float (row, col),
                           1.0 - YP_Float (row, col));
            end loop;
        end loop;

        for row in Y_Prob'First .. Y_Prob'Last loop
            for col in Y_Prob'First (2) .. Y_Prob'Last (2) loop
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
        for row in Activation'First .. Activation'Last loop
            for col in Activation'First (2) .. Activation'Last (2) loop
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
        for row in Z'First .. Z'Last loop
            for col in Z'First (2) .. Z'Last (2) loop
                Prod (row,col) := 1.0 - Z (row,col);
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
    --  Log Loss is the negative average of the log of corrected predicted
    --  probabilities for each instance.
    function Log_Loss (Y_True : Integer_Matrix; Y_Prob : Float_Matrix)
                   return Float is
        type Matrix_Float is new Float_Matrix (1 .. Y_Prob'Length,
                                               1 .. Y_Prob'Length (2));
        Y_P      : Float;
        YP_Float : Matrix_Float := Matrix_Float (Y_Prob);
        YT_Float : Matrix_Float;
        X_Y      : Matrix_Float;
        Sum      : Float := 0.0;
    begin
        --  L194 Clip Y_Prob
        for row in Y_Prob'First .. Y_Prob'Last loop
            for col in Y_Prob'First (2) .. Y_Prob'Last (2) loop
                YT_Float (row, col) := Float (Y_True (row, col));
                Y_P := Y_Prob (row, col);
                if Y_P < EPS then
                    YP_Float (row, col) := EPS;
                elsif Y_P > 1.0 - EPS then
                    YP_Float (row, col) := 1.0 - EPS;
                end if;
            end loop;
        end loop;

        --  xlogy = x*log(y) so that the result is 0 if x = 0
        for row in Y_Prob'First .. Y_Prob'Last loop
            for col in Y_Prob'First (2) .. Y_Prob'Last (2) loop
                X_Y (row, col) :=
                  X_Log_Y (YT_Float (row, col), YP_Float (row, col));
            end loop;
        end loop;

        for row in Y_Prob'First .. Y_Prob'Last loop
            for col in Y_Prob'First (2) .. Y_Prob'Last (2) loop
                Sum := Sum + X_Y (row, col);
            end loop;
        end loop;

        return - Sum / Float (Y_Prob'Length);

    end Log_Loss;

    --  -------------------------------------------------------------------------

    procedure Relu (Activation : in out Float_Matrix) is
        type Matrix_Float is new Float_Matrix (1 .. Activation'Length,
                                               1 .. Activation'Length (2));
        Result : Matrix_Float;
    begin
        for row in Activation'First .. Activation'Last loop
            for col in Activation'First (2) .. Activation'Last (2) loop
                Result (row, col) := Float'Max (0.0, Activation (row, col));
            end loop;
        end loop;

        Activation := Float_Matrix (Result);

    end Relu;

    --  -------------------------------------------------------------------------

    procedure Relu_Derivative (Z : Float_Matrix; Del : in out Float_Matrix) is
    begin
        for row in Z'First .. Z'Last loop
            for col in Z'First (2) .. Z'Last (2) loop
                if Z (row, col) = 0.0 then
                    Del (row, col) := 0.0;
                end if;
            end loop;
        end loop;

    end Relu_Derivative;

    --  -------------------------------------------------------------------------

    procedure Softmax (Activation : in out Float_Matrix) is
        use Maths.Float_Math_Functions;
        Exp_Sum  : Float := 0.0;
    begin
        for row in Activation'First .. Activation'Last loop
            for col in Activation'First (2) .. Activation'Last (2) loop
                Exp_Sum := Exp_Sum + Exp (Activation (row, col));
            end loop;
        end loop;

        Activation := Activation / Exp_Sum;

    end Softmax;

    --  -------------------------------------------------------------------------
    --  L158
    function Squared_Loss (Y_True : Integer_Matrix; Y_Pred : Float_Matrix)
                       return Float is
        use Classifier_Utilities;
    begin
        return Neural_Maths.Mean ((Y_True - Y_Pred) ** 2) / 2.0;

    end Squared_Loss;

    --  -------------------------------------------------------------------------

    procedure Tanh (Activation : in out Float_Matrix) is
        use Maths.Float_Math_Functions;
        type Matrix_Float is new Float_Matrix (1 .. Y_Prob'Length,
                                               1 .. Y_Prob'Length (2));
        Act_List : Float_Array;
        Result   : Float_Matrix;
    begin
        for row in Activation.First_Index .. Activation.Last_Index loop
            Act_List := Activation (row);
            for index in Act_List.First_Index .. Act_List.Last_Index loop
                Act_List (index) :=  Tanh (Act_List (index));
            end loop;
            Result.Append (Act_List);
        end loop;

        Activation := Result;

    end Tanh;

    --  -------------------------------------------------------------------------

    procedure Tanh_Derivative (Z   : Float_Matrix;
                               Del : in out Float_Matrix) is
        type Matrix_Float is new Float_Matrix (1 .. Z'Length,
                                               1 .. Z'Length (2));
        Del_2  : Matrix_Float;
    begin
        for row in Z'First .. Z'Last loop
            for col in Z'First (2) .. Z'Last (2) loop
                Del_2 (row,col) := 1.0 - Z (row, col) ** 2;
            end loop;
        end loop;

        Del := Del * Float_Matrix (Del_2);

    end Tanh_Derivative;

    --  -------------------------------------------------------------------------
    --  scipy/special/_xlogy.pxd
    --  xlogy = x*log(y) so that the result is 0 if x = 0
    function X_Log_Y (X, Y : Float) return Float is
        use Maths.Float_Math_Functions;
        Y1     : Float := Y;
        Result : Float := 0.0;
    begin
        if X /= 0.0 then
            if Y1 < EPS then
                Y1 := EPS;
            end if;
            Result := X * Log (Y1);
        end if;

        return Result;

    end X_Log_Y;

    --  -------------------------------------------------------------------------

end Base_Neural;
