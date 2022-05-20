--  Based on scikit-learn/sklearn/neural_network/_base.py

--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Neural_Maths;
with Printing;

package body Base_Neural is

   EPS : constant Float := Float'Small;

   function X_Log_Y (X : Boolean_Matrix; Y : Real_Float_Matrix)
   return Real_Float_Matrix;
   pragma Inline (X_Log_Y);

   --  -------------------------------------------------------------------------

   function Binary_Log_Loss (Y_True : Boolean_Matrix;
                             Y_Prob : Real_Float_Matrix) return Float is
      use Real_Float_Arrays;
      --        Routine_Name : constant String :=
      --                         "Base_Neural.Binary_Log_Loss_Function ";
      Unit_Matrix : constant Real_Float_Matrix (Y_Prob'Range, Y_Prob'Range (2))
          := (others => (others => 1.0));
      YP          : Real_Float_Matrix := Y_Prob;
      X_Log_Y1    : Real_Float_Matrix (YP'Range, YP'Range (2));
      X_Log_Y2    : Real_Float_Matrix (YP'Range, YP'Range (2));
      Sum1        : Float := 0.0;
      Sum2        : Float := 0.0;
   begin
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
      X_Log_Y1 := X_Log_Y (Y_True, YP);
      X_Log_Y2 := X_Log_Y (not Y_True, Unit_Matrix - YP);

      for row in YP'Range loop
         for col in YP'Range (2) loop
            Sum1 := Sum1 + X_Log_Y1 (row, col);
            Sum2 := Sum2 + X_Log_Y2 (row, col);
         end loop;
      end loop;

      return - (Sum1 + Sum2) / Float (Y_Prob'Length);

   end Binary_Log_Loss;

   --  -------------------------------------------------------------------------

   procedure Identity (Activation : Real_Float_Matrix) is
   begin
      null;
   end Identity;

   --  ------------------------------------------------------------------------

   procedure Identity_Derivative (Z   : Real_Float_Matrix;
                                  Del : in out Real_Float_Matrix) is
   begin
      null;
   end Identity_Derivative;

   --  ------------------------------------------------------------------------

   procedure Logistic (Activation : in out Real_Float_Matrix) is
      use Maths.Float_Math_Functions;
      type Matrix_Float is new Real_Float_Matrix (1 .. Activation'Length,
                                                  1 .. Activation'Length (2));
      Sigmoid  : Matrix_Float;
   begin
      for row in Sigmoid'Range loop
         for col in Sigmoid'Range (2) loop
            Sigmoid (row, col) :=
             1.0 / (1.0 + Exp (Activation (row, col)));
         end loop;
      end loop;

      Activation := Real_Float_Matrix (Sigmoid);

   end Logistic;

   --  -------------------------------------------------------------------------

   procedure Logistic_Derivative (Z   : Real_Float_Matrix;
                                  Del : in out Real_Float_Matrix) is
      use Real_Float_Arrays;
      type Matrix_Float is new Real_Float_Matrix (1 .. Z'Length,
                                                  1 .. Z'Length (2));
      Prod  : Matrix_Float;
   begin
      Del := Del * Z;
      for row in Z'Range loop
         for col in Z'Range (2) loop
            Prod (row, col) := 1.0 - Z (row, col);
         end loop;
      end loop;

      Del := Del * Real_Float_Matrix (Prod);

   end Logistic_Derivative;

   --  -------------------------------------------------------------------------

   function Logistic_Sigmoid (X : Long_Float) return Float is
      use Maths.Long_Float_Math_Functions;
   begin
      return Float (1.0 / (1.0 + Exp (X)));
   end Logistic_Sigmoid;

   --  ------------------------------------------------------------------------
   --  L177 Log Loss is the negative average of the log of corrected predicted
   --  probabilities for each instance.
   function Log_Loss (Y_True : Boolean_Matrix; Y_Prob : Real_Float_Matrix)
                       return Float is
      --          Routine_Name : constant String := "Base_Neural.Log_Loss ";
      YP           : Real_Float_Matrix := Y_Prob;
      YT2          : Boolean_Matrix (Y_True'Range, Y_True'First (2) ..
                                       Y_True'Last (2) + 1);
      YP2          : Real_Float_Matrix (YP'Range, YP'First (2) .. YP'Last (2) + 1);

      function Do_XlogY (Y_True : Boolean_Matrix; Y_Prob : Real_Float_Matrix)
                           return Float is
         X_Y : Real_Float_Matrix (Y_Prob'Range, Y_Prob'Range (2));
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
               YT2 (row, col) := Y_True (row, col);
               YT2 (row, col + 1) := not Y_True (row, col);
            end loop;
         end loop;

         return Do_XlogY (YT2, YP2);

      else
         return Do_XlogY (Y_True, YP);
      end if;

   end Log_Loss;

   --  -------------------------------------------------------------------------

   procedure Rect_LU (Activation : in out Real_Float_Matrix) is
      type Matrix_Float is new Real_Float_Matrix (1 .. Activation'Length,
                                                  1 .. Activation'Length (2));
      Result : Matrix_Float;
   begin
      for row in Activation'Range loop
         for col in Activation'Range (2) loop
            Result (row, col) := Float'Max (0.0, Activation (row, col));
         end loop;
      end loop;

      Activation := Real_Float_Matrix (Result);

   end Rect_LU;

   --  -------------------------------------------------------------------------

   procedure Rect_LU_Derivative (Z : Real_Float_Matrix;
                              Del : in out Real_Float_Matrix) is
   begin
      for row in Z'Range loop
         for col in Z'Range (2) loop
            if Z (row, col) = 0.0 then
               Del (row, col) := 0.0;
            end if;
         end loop;
      end loop;

   end Rect_LU_Derivative;

   --  -------------------------------------------------------------------------
   --  SoftMax returns the probability of each class
   --  probability = exp (value) / sum of all exp (v)
   procedure Softmax (Activation : in out Real_Float_Matrix) is
      Routine_Name : constant String := "Base_Neural.Softmax ";
      --  Max returns a vector containing the maximum value of each matrix
      Tmp  : Real_Float_Matrix := Activation - Max (Activation);
   begin
--        Printing.Print_Float_Matrix (Routine_Name & "Activation",
--                                     Activation, 123, 125);
--        Printing.Print_Float_Matrix (Routine_Name & "Tmp", Tmp, 123, 125);
      Tmp := NL_Arrays_And_Matrices.Exp (Tmp);
--        Printing.Print_Float_Matrix (Routine_Name & "Tmp", Tmp, 123, 125);
      Activation := Tmp / Sum (Tmp);
--        Printing.Print_Float_Matrix (Routine_Name & "Activation out",
--                                     Activation, 123, 125);

   end Softmax;

   --  ------------------------------------------------------------------------
   --  L158
   function Squared_Loss (Y_True : Boolean_Matrix; Y_Pred : Real_Float_Matrix)
                           return Float is
      use Real_Float_Arrays;
      Diff : Real_Float_Matrix := -Y_Pred;
   begin
      for row in Diff'Range loop
         for col in Diff'Range (2) loop
            if Y_True (row, col) then
               Diff (row, col) := 1.0 + Diff (row, col);
            end if;
         end loop;
      end loop;

      return Neural_Maths.Mean (Diff * Diff) / 2.0;

   end Squared_Loss;

   --  -------------------------------------------------------------------------

   procedure Tanh (Activation : in out Real_Float_Matrix) is
      use Maths.Float_Math_Functions;
      type Matrix_Float is new Real_Float_Matrix (1 .. Activation'Length,
                                                  1 .. Activation'Length (2));
      Result : Matrix_Float;
   begin
      for row in Activation'Range loop
         for col in Activation'Range (2) loop
            Result (row, col) := Tanh (Activation (row, col));
         end loop;
      end loop;

      Activation := Real_Float_Matrix (Result);

   end Tanh;

   --  -------------------------------------------------------------------------

   procedure Tanh_Derivative (Z   : Real_Float_Matrix;
                              Del : in out Real_Float_Matrix) is

      use Real_Float_Arrays;
      type Matrix_Float is new Real_Float_Matrix (1 .. Z'Length,
                                                  1 .. Z'Length (2));
      Del_2 : Matrix_Float;
   begin
      for row in Z'Range loop
         for col in Z'Range (2) loop
            Del_2 (row, col) := 1.0 - Z (row, col) ** 2;
         end loop;
      end loop;

      Del := Del * Real_Float_Matrix (Del_2);

   end Tanh_Derivative;

   --  -------------------------------------------------------------------------
   --  scipy/special/_xlogy.pxd
   --  xlogy = x*log(y) so that the result is 0 if x = 0
   function X_Log_Y (X : Boolean_Matrix; Y : Real_Float_Matrix)
                      return Real_Float_Matrix is
      use Maths.Float_Math_Functions;
      Y1     : Real_Float_Matrix := Y;
      Result : Real_Float_Matrix (Y'Range, Y'Range (2));
   begin
      for row in X'Range loop
         for col in X'Range (2) loop
            if X (row, col) then
               if Y1 (row, col) < EPS then
                  Y1 (row, col) := EPS;
               end if;
               if X (row, col) then
                  Result (row, col) :=  Log (Y1 (row, col));
               end if;
            end if;
         end loop;
      end loop;

      return Result;

   end X_Log_Y;

   --  -------------------------------------------------------------------------

end Base_Neural;
