
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;

package body ML is

   function Mult_3 (L, M, R : Real_Float_Vector) return Real_Float_Vector;
   function Sigmoid (H : Real_Float_Vector) return Real_Float_Vector;

   --  -------------------------------------------------------------------------

   procedure Fit (Weights : in out Real_Float_Vector; All_Data : Integer_Matrix;
                  Labels  : Integer_Array) is
      use Real_Float_Arrays;
      Routine_Name    : constant String := "ML.Fit ";
      F_All_Data      : constant Real_Float_Matrix :=
                          To_Real_Float_Matrix (All_Data);
      F_Labels        : constant Real_Float_Vector :=
                          To_Real_Float_Vector (Labels);
      Learn_Rate      : Float := 0.1;
      --  Y = h = np.matmul(alldat,w) dot product
      --  Y1i = w1xi1 + w2x21 + ... wnxn1 + w_offset
      Y               : Real_Float_Vector (All_Data'Range);
      Y_Sig           : Real_Float_Vector (All_Data'Range);
      Y_Sig_Sq        : Real_Float_Vector (All_Data'Range);
      Errors          : Real_Float_Vector (All_Data'Range);
      Errors_x_Grad   : Real_Float_Matrix ( 1 .. 1, All_Data'Range);
      Delta_Matrix    : Real_Float_Matrix (1 .. 1, All_Data'Range (2));
      Delta_Weights   : Real_Float_Vector (Weights'Range);
      New_Weights     : Real_Float_Vector (Weights'Range);
      Current_Loss    : Float;
      Iteration       : Natural := 0;
      Step            : Natural;
      Descend         : Boolean;
      Done            : Boolean := False;  --  Stop near a local minimum
   begin
      Assert (Weights'Length = All_Data'Length (2), Routine_Name &
                "Invalid Weights length");
      Put_Line (Routine_Name);

      while not Done loop
         Iteration := Iteration + 1;
         if Maths.Random_Float < 0.01 then
            --           if Count mod 100 = 0 then
            Put_Line ("**** Iteration " & Integer'Image (Iteration) & " ****");
            Put_Line ("Learning Rate: " & Float'Image (Learn_Rate) &
                        "  Loss: " & Float'Image (Loss (Weights, All_Data, Labels)));
            Print_Float_Vector ("Weights", Weights);
--              Put_Line ("**********************");
            New_Line;
         end if;

         Y := F_All_Data * Weights;  --  h
         --  transform Y to the range 0 .. 1.0 using the sigmoid function
         Y_Sig := Sigmoid (Y);

         --  F_Labels are the expected solutions, 0 or 1
         Errors := F_Labels - Y_Sig;
         --  Compute the gradient of the loss function
         --  delta_w is the change in the weights suggested by the gradient
         --  Delta_Weight_i = error_i * derivative of the sigmoid *  activation_i
         --                   derivative of the sigmoid = exp(-h) * y**2
         --                   activation_i = alldat_i
         --  Delta_Weight = np.add.reduce
         --  (np.reshape((labs-y) * np.exp(-h)*y**2,(len(y),1)) * alldat)
         --  np.exp(-h)*y**2 is the derivative (gradient) of the sigmoid function
         --  add.reduce appears to do matrix multiplication of
         --  (error * sigmoid gradient) and alldat

         Y_Sig_Sq := Y_Sig ** 2;
         Errors_x_Grad := To_Real_Float_Matrix (Mult_3 (Errors, Exp (-Y), Y_Sig_Sq), 2);
         Delta_Matrix := Errors_x_Grad * F_All_Data;
         --           Print_Float_Matrix ("Delta_Matrix", Delta_Matrix);
         Delta_Weights := Sum_Each_Column (Delta_Matrix);
         --           Print_Float_Vector ("Delta_Weights", Delta_Weights);

         Current_Loss := Loss (Weights, All_Data, Labels);
         Learn_Rate := 2.0 * Learn_Rate;

         New_Weights := Weights + Learn_Rate * Delta_Weights;
         --           Put_Line ("Learn_Rate: " & Float'Image (Learn_Rate));
         --           Put_Line ("Current_Loss: " & Float'Image (Current_Loss));
         --           Print_Float_Vector ("New_Weights", New_Weights);
         --           Put_Line ("Next Loss: " &
         --                       Float'Image (Loss (New_Weights, All_Data, Labels)));

         Step := 0;
         Descend := True;
         while Descend loop
            Step := Step + 1;
            Learn_Rate := Learn_Rate / 2.0;
            Done := Learn_Rate * Max (abs (Delta_Weights)) < 0.0001;
            if not Done then
               New_Weights := Weights + Learn_Rate * Delta_Weights;
            end if;
            Descend := not Done and
              Loss (New_Weights, All_Data, Labels) >= Current_Loss;
         end loop;
--           Put_Line ("*******" & Integer'Image (Step) &
--                       " gradient steps completed");

         if not Done then
            Weights := New_Weights;
         end if;
      end loop;

      New_Line;
      Put_Line ("Total iterations:" & Integer'Image (Iteration));
      Put_Line ("Learn_Rate: " & Float'Image (Learn_Rate));
      Print_Float_Vector ("Delta_Weights", Delta_Weights);
      Put_Line ("Final loss:" & Float'Image (Loss (Weights, All_Data, Labels)));
      Put_Line (Routine_Name & "finished");
      New_Line;

   end Fit;

   --  -------------------------------------------------------------------------

   function Loss (Weights : Real_Float_Vector; All_Data : Integer_Matrix;
                  Labels  : Integer_Array) return Float is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "ML.Loss ";
      H      : constant Real_Float_Vector := Dot (All_Data, Weights);
      --  transform using the sigmoid function
      Y      : constant Real_Float_Vector := 1.0 / (1.0 + Exp (-H));
      Errors : Real_Float_Vector (Labels'Range);
   begin
      --  take the difference between the labels and the output of the
      --  sigmoid squared, then sum over all instances to get the
      --  total loss.
      Errors := (To_Real_Float_Vector (Labels) - Y) ** 2;

      return Sum (Errors);

   end Loss;

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

end ML;
