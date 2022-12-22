
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;

package body ML is

--     function Mult_2 (L, R : Real_Float_Matrix) return Real_Float_Matrix;
   function Mult_3 (L, M, R : Real_Float_Vector) return Real_Float_Vector;
   function Sigmoid (H : Real_Float_Vector) return Real_Float_Vector;
   function Vec_To_1xN_Matrix (Vec : Real_Float_Vector)
                               return Real_Float_Matrix;

   --  -------------------------------------------------------------------------

   procedure Fit (Weights : in out Real_Float_Vector; Data : Integer_Matrix;
                  Labels  : Integer_Array) is
      use Real_Float_Arrays;
      Routine_Name    : constant String := "ML.Fit ";
      F_Data          : constant Real_Float_Matrix :=
        To_Real_Float_Matrix (Data);
      F_Labels        : constant Real_Float_Vector :=
        To_Real_Float_Vector (Labels);
      Learn_Rate      : Float := 0.1;
      --  Y1 = h = np.matmul(alldat,w) dot product
      --  Y1i = w1xi1 + w2x21 + ... wnxn1 + w_offset
      Y1              : Real_Float_Vector (Data'Range);
      Y2              : Real_Float_Vector (Data'Range);
--        Exp_Y1          : Real_Float_Vector (Data'Range);
      Y               : Real_Float_Vector (Data'Range);
      Errors          : Real_Float_Vector (Data'Range);
--        Scaled_Errors   : Real_Float_Vector (Data'Range);
      Mult_Vec        : Real_Float_Vector (Data'Range);
      Scale_Matrix    : Real_Float_Matrix ( 1 .. 1, Data'Range);
      Delta_Matrix    : Real_Float_Matrix (1 .. 1, Data'Range (2));
      Delta_Weights   : Real_Float_Vector (Weights'Range);
      New_Weights     : Real_Float_Vector (Weights'Range);
      Current_Loss    : Float;
--        Sum             : Float;
      --  Stop searching near a local minimum
      Done            : Boolean := False;
   begin
      Assert (Weights'Length = Data'Length (2), Routine_Name &
                "Invalid Weights length");
      while not Done loop
         if Maths.Random_Float < 0.01 then
            Put_Line ("***************");
            Put_Line ("Learning Rate: " & Float'Image (Learn_Rate));
            Print_Float_Vector ("Weights", Weights);
            Put_Line ("Loss: " & Float'Image (Loss (Weights, Data, Labels)));
            New_Line;
         end if;
         --  The next few lines compute the gradient
         --  Delta_Weight is the change in  weights suggested by the gradient

         Y1 :=  F_Data * Weights;  --  h
         --           Put_Line ("Y1 Max" & Float'Image (Max (Y1)));
         --  transform using the sigmoid function
--           Y := 1.0 / (1.0 + Exp_Y1);
         Y := Sigmoid (Y1);
--           Put_Line ("Y Min, Max:" & Float'Image (Min (Y)) & ", " &
--                       Float'Image (Max (Y)));
--  delta_w is the change in the weights suggested by the gradient
--  Delta_Weight_i = error_i * derivative of the sigmoid *
--                   activation_i
--                   derivative of the sigmoid = exp(-h) * y**2
--                   activation_i = alldat_i
--  Delta_Weight = np.add.reduce
--  (np.reshape((labs-y) * np.exp(-h)*y**2,(len(y),1)) * alldat)
--  np.exp(-h)*y**2 is the derivative (gradient) of the sigmoid function
--  add.reduce appears to do matrix multiplication of (error * sigmoid gradient) and alldat
         --  exp(-h)*y = exp(-h) * 1.0 / (1.0 + exp(-h))
         --            = exp(-h) / (1.0 + exp(-h))
         --            = 1.0 / (1.0 + 1 / exp(-h))
         --  0.0 <= exp(-h) * y <= 1.0

         Errors := F_Labels - Y;
         Put_Line ("Errors Min, Max: " & Float'Image (Min (Errors)) & ", " &
                     Float'Image (Max (Errors)));
--           Print_Float_Vector ("Errors 100001 .. 100006: ", Errors, 100001, 100006);
         --  (labs-y) * np.exp(-h)*y**2
--           Scaled_Errors := Mult_2 (Errors, Exp (-Y1));
--           Put_Line ("Scaled_Errors Min, Max: " &
--                       Float'Image (Min (Scaled_Errors)) & ", " &
--                       Float'Image (Max (Scaled_Errors)));
         Y2 := Y ** 2;
         Mult_Vec := Mult_3 (Errors, Exp (-Y1), Y2);
         Scale_Matrix := Vec_To_1xN_Matrix (Mult_3 (Errors, Exp (-Y1), Y2));
         Put_Line ("Mult_Vec Min, Max:" & Float'Image (Min (Mult_Vec)) & ", " &
                     Float'Image (Max (Mult_Vec)));
         Put_Line ("Min, Max F_Data: " & Float'Image (Min (F_Data)) & ", " &
                     Float'Image (Max (F_Data)));
         Print_Matrix_Dimensions ("Scale_Matrix", Scale_Matrix);
         Print_Matrix_Dimensions ("F_Data", F_Data);
         Delta_Matrix := Scale_Matrix * F_Data;
--           Delta_Matrix := Mult_2 (Scale_Matrix, F_Data);
--           Print_Float_Matrix ("Scale_Matrix", Scale_Matrix, 1, 5);
--           Print_Float_Matrix ("F_Data", F_Data, 1, 5);
         Print_Float_Matrix ("Delta_Matrix", Delta_Matrix);
         Delta_Weights := Sum_Each_Column (Delta_Matrix);
         Print_Float_Vector ("Delta_Weights", Delta_Weights);

         --  Get new weights by taking a step of size alpha and updating
         Current_Loss := Loss (Weights, Data, Labels);
         Learn_Rate := 2.0 * Learn_Rate;
         New_Weights := Weights + Learn_Rate * Delta_Weights;
         Put_Line ("Learn_Rate: " & Float'Image (Learn_Rate));
         Put_Line ("Current_Loss: " & Float'Image (Current_Loss));
         Print_Float_Vector ("New_Weights", New_Weights);
         Put_Line ("Next Loss: " &
                     Float'Image (Loss (New_Weights, Data, Labels)));

         while Loss (New_Weights, Data, Labels) >= Current_Loss and
           not Done loop
            Learn_Rate := Learn_Rate / 2.0;
            Done := Learn_Rate * Max (abs (Delta_Weights)) < 0.0001;
            if Done then
               Put_Line ("Learn_Rate: " & Float'Image (Learn_Rate));
               Print_Float_Vector ("Delta_Weights", Delta_Weights);
            else
               New_Weights := Weights + Learn_Rate * Delta_Weights;
               if not done then
                  Weights := New_Weights;
               end if;
            end if;
         end loop;
         Done := Learn_Rate * Max (abs (Delta_Weights)) < 0.0001;

      end loop;

   end Fit;

   --  -------------------------------------------------------------------------

   function Loss (Weights : Real_Float_Vector; Data : Integer_Matrix;
                  Labels  : Integer_Array) return Float is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "ML.Loss ";
      H      : constant Real_Float_Vector := Dot (Data, Weights);
      --  transform using the sigmoid function
      Exp_H  : constant Real_Float_Vector := Exp (-H);
      Y      : constant Real_Float_Vector := 1.0 / (1.0 + Exp_H);
      Errors : Real_Float_Vector (Labels'Range);
   begin
      --        Print_Float_Vector (Routine_Name & "H", H);
      --        Put_Line (Routine_Name & "Max (Exp_H): " & Float'Image (Max (Exp_H)));
      --  take the difference between the labels and the output of the
      --  sigmoid squared, then sum over all instances to get the
      --  total loss.
      Errors := (To_Real_Float_Vector (Labels) - Y) ** 2;

      return Sum (Errors);

   end Loss;

   --  -------------------------------------------------------------------------

--     function Mult_2 (L, R : Real_Float_Matrix) return Real_Float_Matrix is
--        Routine_Name : constant String := "ML.Mult_2 ";
--        Result       : Real_Float_Matrix (R'Range, R'Range (2));
--     begin
--        Assert (L'Length = R'Length, Routine_Name &
--                  "vectors have different lengths.");
--        for row in R'Range loop
--           for col in R'Range (2) loop
--              Result (row, col) := L (row, 1) * R (row, col);
--           end loop;
--        end loop;
--
--        return Result;
--
--     end Mult_2;

   --  -------------------------------------------------------------------------

   function Mult_3 (L, M, R : Real_Float_Vector) return Real_Float_Vector is
      Routine_Name : constant String := "ML.Mult_3 ";
      Result       : Real_Float_Vector := L;
   begin
      Assert (L'Length = M'Length and L'Length = R'Length, Routine_Name &
                "vectors have different lengths.");
      for index in L'Range loop
         Result (index) := Result (index) * M (index) * R (index);
      end loop;

      return Result;

   end Mult_3;

   --  -------------------------------------------------------------------------

   function Sigmoid (H : Real_Float_Vector) return Real_Float_Vector is
      use Real_Float_Arrays;
   begin

      return 1.0 / (1.0 + Exp (-H));

   end Sigmoid;

   --  -------------------------------------------------------------------------

   function Vec_To_1xN_Matrix (Vec : Real_Float_Vector)
                               return Real_Float_Matrix is
--        Routine_Name : constant String := "ML.Vec_To_Nx1_Matrix ";
      Result : Real_Float_Matrix ( 1 .. 1, Vec'Range);
   begin
      for index in Vec'Range loop
         Result (1, index) := Vec (index);
      end loop;

      return Result;

   end Vec_To_1xN_Matrix;

   --  -------------------------------------------------------------------------

end ML;
