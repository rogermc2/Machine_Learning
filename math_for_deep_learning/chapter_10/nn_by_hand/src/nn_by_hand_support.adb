
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Load_Dataset;
with Neural_Maths;
with NL_Types;

package body NN_By_Hand_Support is

   Max_Init_Weight : constant Float := 0.0005;
   Neural_Network  : Network_Package.Map;

   function Forward (Net : Network_Package.Map; X : Real_Float_Matrix)
                     return Real_Float_Vector;
   function Means (M : Real_Float_Matrix) return Real_Float_Vector;
--     function Sigmoid (Val : Float) return Float;
   function Standard_Deviation (M : Real_Float_Matrix)
                                return Real_Float_Vector;
   --  -------------------------------------------------------------------------

   function Build_Dataset return Dataset is
      Routine_Name      : constant String :=
                            "NN_By_Hand_Support.Build_Dataset ";
      Iris_Data         : constant Load_Dataset.Iris_Data_Record :=
                            Load_Dataset.Load_Iris ("../../datasets/iris.csv");
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
      Put_Line (Routine_Name);
      for row in X'Range loop
         Feature_Row := Features (row);
         X (row, 1) := Feature_Row (1);
         X (row, 2) := Feature_Row (2);
      end loop;
      Put_Line (Routine_Name & "features set.");

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
      Put_Line (Routine_Name & "done.");

      return theDataset;

   end Build_Dataset;

   --  -------------------------------------------------------------------------

   procedure Evaluate
     (Net      : Network_Package.Map; X : Real_Float_Matrix;
      Y        : Integer_Array; Tn, Fp, Fn, Tp : out Natural;
      Accuracy : out Float;     Pred : out ML_Types.Integer_List) is
      Result : constant Real_Float_Vector := Forward (Net, X);
      C      : Boolean;
   begin
      Tn := 0;
      Fp := 0;
      Fn := 0;
      Tp := 0;

      for index in Y'Range loop
         Assert (Y (index) = 0 or Y (index) = 1, "Evaluate, invalid Y value");
         C := Result (index) >= 0.5;
         if C then
            Pred.Append (1);
         else
            Pred.Append (0);
         end if;

         if C then  -- Result (index) >= 0.5
            if Y (index) = 1 then
               Tp := Tp + 1;
            else
               Fp := Fp + 1;
            end if;
         else  --  Result (index) < 0.5
            if Y (index) = 0 then
               Tn := Tn + 1;
            else
               Fn := Fn + 1;
            end if;
         end if;
      end loop;

      Accuracy := Float (Tn + Tp) / Float (Tn + Tp + Fn + Fp);

   end Evaluate;

   --  -------------------------------------------------------------------------
   --  Forward runs the data X through the neural network defined by Net
   function Forward (Net : Network_Package.Map; X : Real_Float_Matrix)
                     return Real_Float_Vector is
      use Neural_Maths;
      Node_11 : Float;
      Node_12 : Float;
      Result  : Real_Float_Vector (X'Range);
   begin
      for row in X'Range loop
         Node_11 := Sigmoid (Net ("w0") * X (row, 1) + Net ("w2") * X (row, 2) +
                               Net ("b0"));
         Node_12 := Sigmoid (Net ("w1") * X (row, 1) + Net ("w3") * X (row, 2) +
                               Net ("b1"));
         Result (row) :=
           Net ("w4") * Node_11 + Net ("w5") * Node_12 + Net ("b2");
      end loop;

      return Result;

   end Forward;

   --  -------------------------------------------------------------------------

   procedure Gradient_Descent
     (Net : in out Network_Package.Map; X   : Real_Float_Matrix;
      Y   : Integer_Array; Epochs : Positive; Eta : Float) is
      use Neural_Maths;
      Eta_F         : constant Float := Eta / Float (Y'Length);
      Y_Float       : constant Real_Float_Vector := To_Real_Float_Vector (Y);
      Node_11       : Float;
      Node_12       : Float;
      Node_2        : Float;
      dw0           : Float;
      dw1           : Float;
      dw2           : Float;
      dw3           : Float;
      dw4           : Float;
      dw5           : Float;
      db0           : Float;
      db1           : Float;
      db2           : Float;
      Error         : Float;
   begin
      --  Pass over training set accumulating deltas
      for count in 1 .. Epochs loop
         dw0 := 0.0;
         dw1 := 0.0;
         dw2 := 0.0;
         dw3 := 0.0;
         dw4 := 0.0;
         dw5 := 0.0;
         db0 := 0.0;
         db1 := 0.0;
         db2 := 0.0;

         for row in X'Range loop
            --  Forward pass for this row (sample)
            Node_11 := Sigmoid (Net ("w0") * X (row, 1) +
                                  Net ("w2") * X (row, 2) + Net ("b0"));
            Node_12 := Sigmoid (Net ("w1") * X (row, 1) +
                                  Net ("w3") * X (row, 2) + Net ("b1"));
            --  Result
            Node_2 := Net ("w4") * Node_11 + Net ("w5") * Node_12 + Net ("b2");

            --  Backward pass
            --  Loss function L = 0.5 (Y - Node_2)^2
            --  The increments to the deltas, db2 etc. are the
            --  partial derivatives of the loss function
            Error := Node_2 - Y_Float (row);
            db2 := db2 + Error;
            dw4 := dw4 + Error * Node_11;
            dw5 := dw5 + Error * Node_12;
            db1 := db1 + Error * Net ("w5") * Node_12 * (1.0 - Node_12);
            dw1 := dw1 +
              Error * Net ("w5") * Node_12 * (1.0 - Node_12) * X (row, 1);
            dw3 := dw3 +
              Error * Net ("w5") * Node_12 * (1.0 - Node_12) * X (row, 2);
            db0 := db0 + Error * Net ("w4") * Node_11 * (1.0 - Node_11);
            dw0 := dw0 +
              Error * Net ("w4") * Node_11 * (1.0 - Node_11) * X (row, 1);
            dw2 := dw2 +
              Error * Net ("w4") * Node_11 * (1.0 - Node_11) * X (row, 2);
         end loop;

         --  Use average deltas to update the network
         Net ("b2") := Net ("b2") - Eta_F * db2;
         Net ("w4") := Net ("w4") - Eta_F * dw4;
         Net ("w5") := Net ("w5") - Eta_F * dw5;
         Net ("b1") := Net ("b1") - Eta_F * db1;
         Net ("w1") := Net ("w1") - Eta_F * dw1;
         Net ("w3") := Net ("w3") - Eta_F * dw3;
         Net ("b0") := Net ("b0") - Eta_F * db0;
         Net ("w0") := Net ("w0") - Eta_F * dw0;
         Net ("w2") := Net ("w2") - Eta_F * dw2;
      end loop;

   end Gradient_Descent;

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

   function Neural_Net return Network_Package.Map is
   begin

      return Neural_Network;

   end Neural_Net;

   --  -------------------------------------------------------------------------

   function Standard_Deviation (M : Real_Float_Matrix)
                                return Real_Float_Vector is
      use maths.Float_Math_Functions;
      M_Length   : constant Float := Float (M'Length);
      Mean_Vals  : constant Real_Float_Vector := Means (M);
      Errors_Sq  : Real_Float_Matrix (M'Range, M'Range (2));
      Sum1       : Float := 0.0;
      Sum2       : Float := 0.0;
      SD         : Real_Float_Vector (1 .. 2);
   begin
      for row in M'Range loop
         Errors_Sq (row,1) := (M (row, 1) - Mean_Vals (1)) ** 2;
         Errors_Sq (row,2) := (M (row, 2) - Mean_Vals (2)) ** 2;
      end loop;

      for row in M'Range loop
         Sum1 := Sum1 + Errors_Sq (row,1);
         Sum2 := Sum2 + Errors_Sq (row,2);
      end loop;

      SD (1) := Sqrt (Sum1 / (M_Length - 1.0));
      SD (2) := Sqrt (Sum2 / (M_Length - 1.0));

      return SD;

   end Standard_Deviation;

   --  -------------------------------------------------------------------------

begin
   Neural_Network.Include("b2", 0.0);
   Neural_Network.Include("b1", 0.0);
   Neural_Network.Include("b0", 0.0);
   Neural_Network.Include("w0", Max_Init_Weight * Maths.Random_Float);
   Neural_Network.Include("w1", Max_Init_Weight * Maths.Random_Float);
   Neural_Network.Include("w2", Max_Init_Weight * Maths.Random_Float);
   Neural_Network.Include("w3", Max_Init_Weight * Maths.Random_Float);
   Neural_Network.Include("w4", Max_Init_Weight * Maths.Random_Float);
   Neural_Network.Include("w5", Max_Init_Weight * Maths.Random_Float);

end NN_By_Hand_Support;
