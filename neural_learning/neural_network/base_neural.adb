--  Based on scikit-learn/sklearn/neural_network/_base.py

--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Containers;
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Neural_Maths;
--  with Printing;

package body Base_Neural is

   EPS : constant Long_Float := Long_Float'Small;

   function X_Log_Y (X : Boolean_Matrix; Y : Long_Float_Matrix)
   return Long_Float_Matrix;
   pragma Inline (X_Log_Y);

   --  -------------------------------------------------------------------------

   function Binary_Log_Loss (Y_True : Boolean_Matrix;
                             Y_Prob : Long_Float_Matrix) return Float is
      --        Routine_Name : constant String :=
      --                         "Base_Neural.Binary_Log_Loss_Function ";
      Unit_Matrix : constant Long_Float_Matrix (Y_Prob'Range, Y_Prob'Range (2))
          := (others => (others => 1.0));
      YP          : Long_Float_Matrix := Y_Prob;
      X_Log_Y1    : Long_Float_Matrix (YP'Range, YP'Range (2));
      X_Log_Y2    : Long_Float_Matrix (YP'Range, YP'Range (2));
      Sum1        : Long_Float := 0.0;
      Sum2        : Long_Float := 0.0;
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
      X_Log_Y1 := X_Log_Y (Y_True, YP);
      X_Log_Y2 := X_Log_Y (not Y_True, Unit_Matrix - YP);

      for row in YP'Range loop
         for col in YP'Range (2) loop
            Sum1 := Sum1 + X_Log_Y1 (row, col);
            Sum2 := Sum2 + X_Log_Y2 (row, col);
         end loop;
      end loop;

      return - Float ((Sum1 + Sum2) / Long_Float (Y_Prob'Length));

   end Binary_Log_Loss;

   --  -------------------------------------------------------------------------

   procedure Identity (Activation : Long_Float_Matrix) is
   begin
      null;
   end Identity;

   --  ------------------------------------------------------------------------

   procedure Identity_Derivative (Z   : Long_Float_Matrix;
                                  Del : in out Long_Float_Matrix) is
   begin
      null;
   end Identity_Derivative;

   --  ------------------------------------------------------------------------

   procedure Logistic (Activation : in out Long_Float_Matrix) is
      use Maths.Long_Float_Math_Functions;
      type Matrix_Float is new Long_Float_Matrix (1 .. Activation'Length,
                                                  1 .. Activation'Length (2));
      Sigmoid  : Matrix_Float;
   begin
      for row in Sigmoid'Range loop
         for col in Sigmoid'Range (2) loop
            Sigmoid (row, col) :=
             1.0 / (1.0 + Exp (Activation (row, col)));
         end loop;
      end loop;

      Activation := Long_Float_Matrix (Sigmoid);

   end Logistic;

   --  -------------------------------------------------------------------------

   procedure Logistic_Derivative (Z   : Long_Float_Matrix;
                                  Del : in out Long_Float_Matrix) is
      type Matrix_Float is new Long_Float_Matrix (1 .. Z'Length,
                                                  1 .. Z'Length (2));
      Prod  : Matrix_Float;
   begin
      Del := Del * Z;
      for row in Z'Range loop
         for col in Z'Range (2) loop
            Prod (row, col) := 1.0 - Z (row, col);
         end loop;
      end loop;

      Del := Del * Long_Float_Matrix (Prod);

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
   function Log_Loss (Y_True : Boolean_Matrix; Y_Prob : Long_Float_Matrix)
                       return Float is
      --          Routine_Name : constant String := "Base_Neural.Log_Loss ";
      YP           : Long_Float_Matrix := Y_Prob;
      YT2          : Boolean_Matrix (Y_True'Range, Y_True'First (2) ..
                                       Y_True'Last (2) + 1);
      YP2          : Long_Float_Matrix (YP'Range, YP'First (2) .. YP'Last (2) + 1);

      function Do_XlogY (Y_True : Boolean_Matrix; Y_Prob : Long_Float_Matrix)
                           return Long_Float is
         X_Y : Long_Float_Matrix (Y_Prob'Range, Y_Prob'Range (2));
         Sum : Long_Float := 0.0;
      begin
         X_Y := X_Log_Y (Y_True, Y_Prob);
         for row in Y_Prob'Range loop
            for col in Y_Prob'Range (2) loop
               Sum := Sum + X_Y (row, col);
            end loop;
         end loop;

         return - Sum / Long_Float (Y_Prob'Length);
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

         return Float (Do_XlogY (YT2, YP2));

      else
         return Float (Do_XlogY (Y_True, YP));
      end if;

   end Log_Loss;

   --  -------------------------------------------------------------------------

   procedure Relu (Activation : in out Long_Float_Matrix) is
      type Matrix_Float is new Long_Float_Matrix (1 .. Activation'Length,
                                                  1 .. Activation'Length (2));
      Result : Matrix_Float;
   begin
      for row in Activation'Range loop
         for col in Activation'Range (2) loop
            Result (row, col) := Long_Float'Max (0.0, Activation (row, col));
         end loop;
      end loop;

      Activation := Long_Float_Matrix (Result);

   end Relu;

   --  -------------------------------------------------------------------------

   procedure Relu_Derivative (Z : Long_Float_Matrix;
                              Del : in out Long_Float_Matrix) is
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

   procedure Softmax (Activation : in out Long_Float_Matrix) is
--        Routine_Name : constant String := "Base_Neural.Softmax ";
      Tmp  : Long_Float_Matrix := Activation - Max (Activation);
   begin
      Tmp := NL_Arrays_And_Matrices.Exp (Tmp);
      Activation := Tmp / Sum (Tmp);

   end Softmax;

   --  ------------------------------------------------------------------------
   --  L158
   function Squared_Loss (Y_True : Boolean_Matrix; Y_Pred : Long_Float_Matrix)
                           return Float is
      Diff : Long_Float_Matrix := -Y_Pred;
   begin
      for row in Diff'Range loop
         for col in Diff'Range (2) loop
            if Y_True (row, col) then
               Diff (row, col) := 1.0 + Diff (row, col);
            end if;
         end loop;
      end loop;

      return Float (Neural_Maths.Mean (Diff * Diff) / 2.0);

   end Squared_Loss;

   --  -------------------------------------------------------------------------

   procedure Tanh (Activation : in out Long_Float_Matrix) is
      use Maths.Long_Float_Math_Functions;
      type Matrix_Float is new Long_Float_Matrix (1 .. Activation'Length,
                                                  1 .. Activation'Length (2));
      Result : Matrix_Float;
   begin
      for row in Activation'Range loop
         for col in Activation'Range (2) loop
            Result (row, col) := Tanh (Activation (row, col));
         end loop;
      end loop;

      Activation := Long_Float_Matrix (Result);

   end Tanh;

   --  -------------------------------------------------------------------------

   procedure Tanh_Derivative (Z   : Long_Float_Matrix;
                              Del : in out Long_Float_Matrix) is
      type Matrix_Float is new Long_Float_Matrix (1 .. Z'Length,
                                                  1 .. Z'Length (2));
      Del_2 : Matrix_Float;
   begin
      for row in Z'Range loop
         for col in Z'Range (2) loop
            Del_2 (row, col) := 1.0 - Z (row, col) ** 2;
         end loop;
      end loop;

      Del := Del * Long_Float_Matrix (Del_2);

   end Tanh_Derivative;

   --  -------------------------------------------------------------------------
   --  scipy/special/_xlogy.pxd
   --  xlogy = x*log(y) so that the result is 0 if x = 0
   function X_Log_Y (X : Boolean_Matrix; Y : Long_Float_Matrix)
                      return Long_Float_Matrix is
      use Maths.Long_Float_Math_Functions;
      Y1     : Long_Float_Matrix := Y;
      Result : Long_Float_Matrix (Y'Range, Y'Range (2));
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
