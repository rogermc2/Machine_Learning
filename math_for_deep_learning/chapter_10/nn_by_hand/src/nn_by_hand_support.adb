
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Maths;

with Load_Dataset;
with NL_Types;

package body NN_By_Hand_Support is

   type Net_Cursor is new Network_Package.Cursor;

   Max_Initial_Weight : constant Float := 0.0005;
   Network            : Network_Package.Map;

   function Forward (Net : Network_Package.Map; X : Real_Float_Matrix)
                     return Real_Float_Vector;
   function Means (M : Real_Float_Matrix) return Real_Float_Vector;
   function Sigmoid (Val : Float) return float;
   function Standard_Deviation (M : Real_Float_Matrix)
                                return Real_Float_Vector;
   --  -------------------------------------------------------------------------

   function Build_Dataset return Dataset is
      Iris_Data         : constant Load_Dataset.Iris_Data_Record :=
                            Load_Dataset.Load_Iris ("../../iris.csv");
      Features          : constant NL_Types.Float_List_2D := Iris_Data.Features;
      Target            : constant ML_Types.Integer_List := Iris_Data.Target;
      Train_Length      : constant Positive := 70;
      Test_Length       : constant Positive := 30;
      Feature_Row       : NL_Types.Float_List;
      X                 : Real_Float_Matrix
        (Features.First_Index .. Features.Last_Index, 1 .. 2);
      X_Means           : Real_Float_Vector (X'Range (2));
      X_SDs             : Real_Float_Vector (X'Range (2));
      I0                : ML_Types.Integer_List;
      I1                : ML_Types.Integer_List;
      theDataset        : Dataset (Train_Length, Test_Length, 2);
   begin
      for row in X'Range loop
         Feature_Row := Features (row);
         X (row, 1) := Feature_Row (1);
         X (row, 2) := Feature_Row (2);
      end loop;
      X_Means := Means (X);
      X_SDs := Standard_Deviation (X);
      for row in X'Range loop
         Feature_Row := Features (row);
         X (row, 1) := (X (row, 1) - X_Means (1)) / X_SDs (1);
         X (row, 2) := (X (row, 2) - X_Means (2)) / X_SDs (2);
      end loop;

      for index in Target.First_Index .. Target.Last_Index loop
         if Target (index) = 0 then
            I0.Append (index);
         elsif Target (index) = 1 then
            I1.Append (index);
         end if;
      end loop;

      declare
         I0_Length : constant Natural := Integer (I0.Length);
         I1_Length : constant Natural := Integer (I1.Length);
         X_Trimmed : Real_Float_Matrix
           (1 .. I0_Length + I1_Length, 1 .. 2);
      begin
         for row in 1 .. I0_Length loop
            X_Trimmed (row, 1) := X (I0 (row), 1);
            X_Trimmed (row, 2) := X (I0 (row), 2);
         end loop;

         for row in 1 .. I1_Length loop
            X_Trimmed (I0_Length + row, 1) := X (I1 (row), 1);
            X_Trimmed (I0_Length + row, 2) := X (I1 (row), 2);
         end loop;

         for row in 1 .. 35 loop
            theDataset.X_Train (row, 1) := X_Trimmed (row, 1);
            theDataset.X_Train (row, 2) := X_Trimmed (row, 2);
         end loop;

         for row in 36 .. 70 loop
            theDataset.X_Train (row, 1) := X_Trimmed (row + 14, 1);
            theDataset.X_Train (row, 2) := X_Trimmed (row + 14, 2);
         end loop;

         for index in 1 .. 70 loop
            if index < 36 then
               theDataset.Y_Train (index) := 0;
            else
               theDataset.Y_Train (index) := 1;
            end if;
         end loop;

         for row in 1 .. 15 loop
            theDataset.X_Test (row, 1) := X_Trimmed (row + 35, 1);
            theDataset.X_Test (row, 2) := X_Trimmed (row + 35, 2);
         end loop;

         for row in 16 .. 30 loop
            theDataset.X_Test (row, 1) := X_Trimmed (row + 85, 1);
            theDataset.X_Test (row, 2) := X_Trimmed (row + 85, 2);
         end loop;

         for index in 1 .. 30 loop
            if index < 16 then
               theDataset.Y_Test (index) := 0;
            else
               theDataset.Y_Test (index) := 1;
            end if;
         end loop;
      end;

      return theDataset;

   end Build_Dataset;

   --  -------------------------------------------------------------------------

   procedure Evaluate
     (Net            : Network_Package.Map; X : Real_Float_Matrix;
      Y              : Integer_Array;
      Tn, Fp, Fn, Tp : out Natural; Pred : out ML_Types.Integer_List) is
      F_Data : constant Real_Float_Vector := Forward (Net, X);
      C      : Boolean;
   begin
      Tn := 0;
      Fp := 0;
      Fn := 0;
      Tp := 0;

      for index in Y'Range loop
         C :=  F_Data (index) >= 0.5;
         if C then
            Pred.Append (1);
         else
            Pred.Append (0);
         end if;

         if not C and then Y (index) = 0 then
            Tn := Tn + 1;
         elsif not C and then Y (index) = 1 then
            Fn := Fn + 1;
         elsif C and then Y (index) = 0 then
            Fp := Fp + 1;
         else
            Tp := Tp + 1;
         end if;
      end loop;

   end Evaluate;

   --  -------------------------------------------------------------------------

   function Forward (Net : Network_Package.Map; X : Real_Float_Matrix)
                     return Real_Float_Vector is
      A0     : Float;
      A1     : Float;
      Result : Real_Float_Vector (X'Range) := (others => 0.0);
   begin
      for row in X'Range loop
         A0 := Sigmoid (Net ("w0") * X (row, 1) + Net ("w2") * X (row, 2) +
                          Net ("b0"));
         A1 := Sigmoid (Net ("w1") * X (row, 1) + Net ("w3") * X (row, 2) +
                          Net ("b1"));
         Result (row) := Net ("w4") * A0 + Net ("w5") * A1 + Net ("b2");
      end loop;

      return Result;

   end Forward;

   --  -------------------------------------------------------------------------

   function Means (M : Real_Float_Matrix) return Real_Float_Vector is
      M_Length : constant Float := Float (M'Length);
      Sum1     : Float := 0.0;
      Sum2     : Float := 0.0;
      Result   : Real_Float_Vector (1 .. 2);
   begin
      for row in M'Range loop
         Sum1 := Sum1 + M (row, 1);
         Sum2 := Sum2 + M (row, 2);
      end loop;

      for row in M'Range loop
         Result (1) := Sum1 / M_Length;
         Result (2) := Sum2 / M_Length;
      end loop;

      return Result;

   end Means;

   --  -------------------------------------------------------------------------

   function Net_Map return Network_Package.Map is
   begin

      return Network;

   end Net_Map;

   --  -------------------------------------------------------------------------

   function Sigmoid (Val : Float) return float is
      use Maths.Float_Math_Functions;
   begin

      return 1.0 / (1.0 + Exp (Val));

   end Sigmoid;

   --  -------------------------------------------------------------------------

   function Standard_Deviation (M : Real_Float_Matrix)
                                return Real_Float_Vector is
      use maths.Float_Math_Functions;
      M_Length  : constant Float := Float (M'Length);
      Mean_Vals : constant Real_Float_Vector := Means (M);
      Diffs_Sq  : Real_Float_Matrix (M'Range, M'Range (2));
      Sum1      : Float := 0.0;
      Sum2      : Float := 0.0;
      SD        : Real_Float_Vector (1 .. 2);
   begin
      for row in M'Range loop
         Diffs_Sq (row,1) := (M (row, 1) - Mean_Vals (1)) ** 2;
         Diffs_Sq (row,2) := (M (row, 2) - Mean_Vals (2)) ** 2;
      end loop;

      for row in M'Range loop
         Sum1 := Sum1 + Diffs_Sq (row,1);
         Sum2 := Sum2 + Diffs_Sq (row,2);
      end loop;

      SD (1) := Sqrt (Sum1 / (M_Length - 1.0));
      SD (2) := Sqrt (Sum2 / (M_Length - 1.0));

      return SD;

   end Standard_Deviation;

   --  -------------------------------------------------------------------------

begin
   Network.Include("b2", 0.0);
   Network.Include("b1", 0.0);
   Network.Include("b0", 0.0);
   Network.Include("w0", Max_Initial_Weight * Maths.Random_Float);
   Network.Include("w1", Max_Initial_Weight * Maths.Random_Float);
   Network.Include("w2", Max_Initial_Weight * Maths.Random_Float);
   Network.Include("w3", Max_Initial_Weight * Maths.Random_Float);
   Network.Include("w4", Max_Initial_Weight * Maths.Random_Float);
   Network.Include("w5", Max_Initial_Weight * Maths.Random_Float);

end NN_By_Hand_Support;
