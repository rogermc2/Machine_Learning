--  Based on scikit-learn/sklearn/neural_network/_base.py

--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Neural_Maths;
--  with Test_Support;

package body Base_Neural is

   EPS : constant Float := 10.0 ** (-16);  --  temporary fof comparision with
   --  Python results
   --     EPS : constant Float := Float'Safe_Small;

   --      function X_Log_Y (X : Integer_Matrix; Y : Real_Float_Matrix)
   --                        return Real_Float_Matrix;

   --  -------------------------------------------------------------------------

   procedure Clip (Mat : in out Real_Float_Matrix) is
   begin
      for row in Mat'Range loop
         for col in Mat'Range (2) loop
            if Mat (row, col) < EPS then
               Mat (row, col) := EPS;
            elsif Mat (row, col) > 1.0 - EPS then
               Mat (row, col) := 1.0 - EPS;
            end if;
         end loop;
      end loop;

   end Clip;

   --  -------------------------------------------------------------------------
   --  L205
   --  - 1/N Sum (i=1..N)
   --          (y_i * log (p(y_i)) + (1 - y_i) * log (p(1 - y_i)))
   --  y is the label (0 and 1 for binary)
   --  p(y) is the predicted probability of the data point being 1 for all
   --  N points.
   --  y_prob : n_samples x n_classes, predicted probabilities returned by a
   --  classifier's predict_proba method.
   function Log_Loss (Y_True, Y_Prob : Real_Float_Matrix) return Float is
--        Routine_Name : constant String := "Base_Neural.Log_Loss_Function ";

      --  ----------------------------------------------------------------------

      function Check_Columns (Mat : Real_Float_Matrix)
                              return Real_Float_Matrix is
      begin
         if Mat'Length (2) = 1 then
            declare
               Mat2  : Real_Float_Matrix (Mat'Range, 1 .. 2);
            begin
               for row in Mat2'Range loop
                  Mat2 (row, 1) := 1.0 - Mat (row, 1);
                  Mat2 (row, 2) := Mat (row, 1);
               end loop;
               return Mat2;
            end;

         else
            return Mat;
         end if;

      end Check_Columns;

      --  ----------------------------------------------------------------------

      function Sum_XlogY (Y_True, Y_Pred : Real_Float_Matrix) return Float is
         X_Y : constant Real_Float_Matrix := X_Log_Y (Y_True, Y_Pred);
         Sum : Float := 0.0;
      begin
         for row in X_Y'Range loop
            for col in X_Y'Range (2) loop
               Sum := Sum + X_Y (row, col);
            end loop;
         end loop;

         return Sum;

      end Sum_XlogY;

      --  ----------------------------------------------------------------------

      YP_Clip : Real_Float_Matrix := Y_Prob;
   begin
      Clip (YP_Clip);
      declare
         YT : constant Real_Float_Matrix := Check_Columns (Y_True);
         YP : constant Real_Float_Matrix := Check_Columns (YP_Clip);
      begin
         return - (Sum_XlogY (YT,  YP)) / Float (YP'Length);
      end;

   end Log_Loss;

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
   begin
      Activation := Logistic_Sigmoid (Activation);

   end Logistic;

   --  -------------------------------------------------------------------------
   --   Z : Activation data generated by the forward pass.
   procedure Logistic_Derivative (Z   : Real_Float_Matrix;
                                  Del : in out Real_Float_Matrix) is
   begin
      for row in Del'Range loop
         for col in Del'Range (2) loop
            Del (row, col) :=
              Del (row, col) * Z (row, col) * (1.0 - Z (row, col));
         end loop;
      end loop;

   end Logistic_Derivative;

   --  -------------------------------------------------------------------------

   function Logistic_Sigmoid (X : Long_Float) return Float is
      use Maths.Long_Float_Math_Functions;
   begin
      return Float (1.0 / (1.0 + Exp (-X)));
   end Logistic_Sigmoid;

   --  ------------------------------------------------------------------------

   function Logistic_Sigmoid (X : Real_Float_Matrix)
                              return Real_Float_Matrix is
      use Maths.Float_Math_Functions;
      Sigmoid  : Real_Float_Matrix (X'Range, X'Range (2));
   begin
      for row in Sigmoid'Range loop
         for col in Sigmoid'Range (2) loop
            Sigmoid (row, col) :=
              1.0 / (1.0 + Exp (-X (row, col)));
         end loop;
      end loop;

      return Sigmoid;

   end Logistic_Sigmoid;

   --  ------------------------------------------------------------------------
   --  L205
   function Binary_Log_Loss (Y_True, Y_Prob : Real_Float_Matrix)
                             return Float is
--        Routine_Name : constant String := "Base_Neural.Binary_Log_Loss ";
      YT2          : Real_Float_Matrix
        (Y_True'Range, Y_True'First (2) .. Y_True'Last (2) + 1);
      YP_Clip      : Real_Float_Matrix := Y_Prob;
      YP2          : Real_Float_Matrix
        (Y_Prob'Range, Y_Prob'First (2) .. Y_Prob'Last (2) + 1);

      function Do_XlogY (Y_True, Y_Pred : Real_Float_Matrix)
                         return Float is
         X_Y : Real_Float_Matrix (Y_Pred'Range, Y_Pred'Range (2));
         Sum : Float := 0.0;
      begin
         X_Y := X_Log_Y (Y_True, Y_Pred);
         for row in Y_Pred'Range loop
            for col in Y_Pred'Range (2) loop
               Sum := Sum + X_Y (row, col);
            end loop;
         end loop;

         return - Sum / Float (Y_Pred'Length);

      end Do_XlogY;

   begin
      --  L226
      Clip (YP_Clip);
--        Test_Support.Print_Float_Matrix (Routine_Name & "Y_Prob", Y_Prob, 1, 3);
--        Test_Support.Print_Float_Matrix (Routine_Name & "Y_True", Y_True, 1, 3);
      --  xlogy = x*log(y) so that the result is 0 if x = 0
      for row in Y_Prob'Range loop
         for col in Y_Prob'Range (2) loop
            YP2 (row, col) := YP_Clip (row, col);
            YP2 (row, col + 1) := 1.0 - YP_Clip (row, col);
            YT2 (row, col) := Y_True (row, col);
            YT2 (row, col + 1) := 1.0 - Y_True (row, col);
         end loop;
      end loop;

      return Do_XlogY (YT2, YP2);

   end Binary_Log_Loss;

   --  -------------------------------------------------------------------------

   --      function Log_Loss (Y_True : Integer_Matrix; Y_Prob : Real_Float_Matrix)
   --                         return Float is
   --      --          Routine_Name : constant String := "Base_Neural.Log_Loss ";
   --          YT2          : Integer_Matrix (Y_True'Range, Y_True'First (2) ..
   --                                           Y_True'Last (2) + 1);
   --          YP2          : Real_Float_Matrix
   --            (Y_Prob'Range, Y_Prob'First (2) .. Y_Prob'Last (2) + 1);
   --
   --          function Do_XlogY (Y_True : Integer_Matrix; Y_Pred : Real_Float_Matrix)
   --                             return Float is
   --              X_Y : Real_Float_Matrix (Y_Pred'Range, Y_Pred'Range (2));
   --              Sum : Float := 0.0;
   --          begin
   --              X_Y := X_Log_Y (Y_True, Y_Pred);
   --              for row in Y_Pred'Range loop
   --                  for col in Y_Pred'Range (2) loop
   --                      Sum := Sum + X_Y (row, col);
   --                  end loop;
   --              end loop;
   --
   --              return - Sum / Float (Y_Pred'Length);
   --
   --          end Do_XlogY;
   --
   --      begin
   --          --  xlogy = x*log(y) so that the result is 0 if x = 0
   --          if Y_Prob'Length (2) = 1 then
   --              for row in Y_Prob'Range loop
   --                  for col in Y_Prob'Range (2) loop
   --                      YP2 (row, col) := Y_Prob (row, col);
   --                      YP2 (row, col + 1) := 1.0 - Y_Prob (row, col);
   --                      YT2 (row, col) := Y_True (row, col);
   --                      YT2 (row, col + 1) := 1 - Y_True (row, col);
   --                  end loop;
   --              end loop;
   --
   --              return Do_XlogY (YT2, YP2);
   --
   --          else
   --              return Do_XlogY (Y_True, Y_Prob);
   --          end if;
   --
   --      end Log_Loss;

   --  -------------------------------------------------------------------------

   procedure Rect_LU (Activation : in out Real_Float_Matrix) is
   begin
      for row in Activation'Range loop
         for col in Activation'Range (2) loop
            Activation (row, col) := Float'Max (0.0, Activation (row, col));
         end loop;
      end loop;

   end Rect_LU;

   --  -------------------------------------------------------------------------
   --  L132  del[Z == 0] = 0 means
   --        for each element of Z that is 0, repalce the corresponding
   --        element of del with 0   (f(x) = max(0 , x))
   procedure Rect_LU_Derivative (Z   : Real_Float_Matrix;
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
   --  Activation: n_samples x n_features
   procedure Softmax (Activation : in out Real_Float_Matrix) is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Base_Neural.Softmax ";
      --  Max returns a vector with the maximum value of each row of a matrix
      Tmp          : Real_Float_Matrix := Activation - Max (Activation);
      Sum_Tmp      : Real_Float_Vector (Activation'Range);
   begin
      Tmp := NL_Arrays_And_Matrices.Exp (Tmp);
      Activation := Tmp;
      Sum_Tmp := Sum (Tmp);
      Activation := Activation / Sum_Tmp;

   end Softmax;

   --  ------------------------------------------------------------------------
   --  L158
   function Squared_Loss (Y_True, Y_Pred : Real_Float_Matrix)
                          return Float is
      use Real_Float_Arrays;
      Diff : constant Real_Float_Matrix := Y_True - Y_Pred;
   begin

      return Neural_Maths.Mean (Diff * Diff) / 2.0;

   end Squared_Loss;

   --  -------------------------------------------------------------------------
   --  L158
   function Squared_Loss (Y_True : Integer_Matrix; Y_Pred : Real_Float_Matrix)
                          return Float is
      use Real_Float_Arrays;
      Diff : constant Real_Float_Matrix :=
               To_Real_Float_Matrix (Y_True) - Y_Pred;
   begin

      return Neural_Maths.Mean (Diff * Diff) / 2.0;

   end Squared_Loss;

   --  -------------------------------------------------------------------------

   procedure Tanh (Activation : in out Real_Float_Matrix) is
      use Maths.Float_Math_Functions;
   begin
      for row in Activation'Range loop
         for col in Activation'Range (2) loop
            Activation (row, col) := Tanh (Activation (row, col));
         end loop;
      end loop;

   end Tanh;

   --  -------------------------------------------------------------------------

   procedure Tanh_Derivative (Z   : Real_Float_Matrix;
                              Del : in out Real_Float_Matrix) is

      use Real_Float_Arrays;
      Del_2 : Real_Float_Matrix (1 .. Z'Length, 1 .. Z'Length (2));
   begin
      for row in Z'Range loop
         for col in Z'Range (2) loop
            Del_2 (row, col) := 1.0 - Z (row, col) ** 2;
         end loop;
      end loop;

      Del := Multiply_Elements (Del, Del_2);

   end Tanh_Derivative;

   --  -------------------------------------------------------------------------

   function X_Log_Y (X, Y : Float) return Float is
      use Maths.Float_Math_Functions;
      --  Routine_Name : constant String := "Base_Neural.X_Log_Y ";
      Y1           : Float := Y;
      Result       : Float;
   begin
      if Y1 = 0.0 then
         Y1 := EPS;
      end if;

      if X = 0.0 then
         Result := 0.0;
      else
         Result := X * Log (Y1);
      end if;

      return Result;

   end X_Log_Y;

   --  -------------------------------------------------------------------------
   --  scipy/special/_xlogy.pxd
   --  xlogy = x*log(y) so that the result is 0 if x = 0
   function X_Log_Y (X, Y : Real_Float_Matrix) return Real_Float_Matrix is
      Result : Real_Float_Matrix (Y'Range, Y'Range (2)) :=
                 (others => (others => 0.0));
   begin
      for row in X'Range loop
         for col in X'Range (2) loop
            Result (row, col) := X_Log_Y (X (row, col), Y (row, col));
         end loop;
      end loop;

      return Result;

   end X_Log_Y;

   --  -------------------------------------------------------------------------
   --  scipy/special/_xlogy.pxd
   --  xlogy = x*log(y) so that the result is 0 if x = 0
   --      function X_Log_Y (X : Integer_Matrix; Y : Real_Float_Matrix)
   --                        return Real_Float_Matrix is
   --      begin
   --
   --          return X_Log_Y (To_Real_Float_Matrix (X), Y);
   --
   --      end X_Log_Y;

   --  -------------------------------------------------------------------------

end Base_Neural;
