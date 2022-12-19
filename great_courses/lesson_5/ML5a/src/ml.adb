
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;

package body ML is

   function Mult (L, M, R : Real_Float_Vector) return Real_Float_Vector;

   --  -------------------------------------------------------------------------

   procedure Fit (Weights : in out Real_Float_Vector; Data : Integer_Matrix;
                  Labels  : Integer_Array) is
      use Real_Float_Arrays;
      Routine_Name    : constant String := "ML.Fit ";
      F_Data          : constant Real_Float_Matrix :=
                          To_Real_Float_Matrix (Data);
      F_Labels        : constant Real_Float_Vector :=
                          To_Real_Float_Vector (Labels);
      Step_Size       : Float := 0.1;
      --  Y1 = h = np.matmul(alldat,w) dot product
      --  Y1i = w1xi1 + w2x21 + ... wnxn1 + w_offset
      Y1              : Real_Float_Vector (Data'Range);
      Exp_Y1          : Real_Float_Vector (Data'Range);
      Y               : Real_Float_Vector (Data'Range);
      Errors          : Real_Float_Vector (Data'Range);
      Delta_Weights   : Real_Float_Vector (Weights'Range);
      New_Weights     : Real_Float_Vector (Weights'Range);
      Current_Loss    : Float;
      --  Stop searching near a local minimum
      Done            : Boolean := False;
   begin
      Assert (Weights'Length = Data'Length (2), Routine_Name &
                "Invalid Weights length");
      while not Done loop
         --   take a peek at the weights, the learning
         --   rate, and the current loss
         if Maths.Random_Float < 0.01 then
            null;
            --           Put_Line (w, Alpha, Loss (w, Data, Labels));
         end if;
         --  The next few lines compute the gradient
         --  Delta_Weight is the change in  weights suggested by the gradient

         Y1 := F_Data * Weights;  --  h
         --  transform using the sigmoid function
         Exp_Y1 := Exp (-Y1);
         Y := 1.0 / (1.0 + Exp_Y1);
         --  delta_w is the change in the weights suggested by the gradient
         --  Delta_Weight = np.add.reduce
         --  (np.reshape((labs-y) * np.exp(-h)*y**2,(len(y),1)) * alldat)
         Errors := (F_Labels - Y);
         Delta_Weights := Mult (Errors, Exp_Y1, Y) ** 2 * F_Data;

         --  Get new weights by taking a step of size alpha and updating
         Current_Loss := Loss (Weights, Data, Labels);
         Step_Size := 2.0 * Step_Size;
         New_Weights := Weights + Step_Size * Delta_Weights;

         while Loss (New_Weights, Data, Labels) >= Current_Loss and not Done loop
            Step_Size := Step_Size / 2.0;
            Done := Step_Size * Max (abs (Delta_Weights)) < 0.0001;
            if Done then
               Put_Line ("Step_Size: " & Float'Image (Step_Size));
               Print_Float_Vector ("Delta_Weights", Delta_Weights);
            else
               New_Weights := Weights + Step_Size * Delta_Weights;
               if not done then
                  Weights := New_Weights;
               end if;
            end if;
         end loop;

      end loop;

   end Fit;

   --  -------------------------------------------------------------------------

   function Loss (Weights : Real_Float_Vector; Data   : Integer_Matrix;
                  Labels : Integer_Array) return Float is
      use Real_Float_Arrays;
      H      : constant Real_Float_Vector := To_Real_Float_Matrix (Data) * Weights;
      --  transform using the sigmoid function
      Exp_H  : constant Real_Float_Vector := Exp (-H);
      Y      : constant Real_Float_Vector := 1.0 / (1.0 + Exp_H);
      Errors : Real_Float_Vector (Labels'Range);
   begin
      --  take the difference between the labels and the output of the
      --  sigmoid squared, then sum over all instances to get the
      --  total loss.
      Errors := (To_Real_Float_Vector (Labels) - Y) ** 2;

      return Sum (Errors);

   end Loss;

   --  -------------------------------------------------------------------------

   function Mult (L, M, R : Real_Float_Vector) return Real_Float_Vector is
      Routine_Name : constant String := "ML.Mult ";
      Result       : Real_Float_Vector := L;
   begin
      Assert (L'Length = M'Length and L'Length = R'Length, Routine_Name &
                "vectors have different lengths.");
      for index in L'Range loop
         Result (index) := Result (index) * M (index) * R (index);
      end loop;

      return Result;

   end Mult;

   --  -------------------------------------------------------------------------

end ML;
