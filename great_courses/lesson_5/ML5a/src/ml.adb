
with Ada.Assertions; use Ada.Assertions;
with Maths;

package body ML is

   function Mult (L, R : Real_Float_Vector) return Real_Float_Vector;

   --  -------------------------------------------------------------------------

   function Fit (Weights : Real_Float_Vector; Data: Integer_Matrix;
                 Labels  : Integer_Array) return Real_Float_Vector is
      use Real_Float_Arrays;
      Data_F          : constant Real_Float_Matrix :=
                          To_Real_Float_Matrix (Data);
      Labels_F        : constant Real_Float_Vector :=
                          To_Real_Float_Vector (Labels);
      Current_Weights : Real_Float_Vector := Weights;
      Alpha           : Float := 0.1;
      --  h = np.matmul(alldat,w) dot product
      H               : Real_Float_Vector (Weights'Range);
      Exp_H           : Real_Float_Vector (Weights'Range);
      Y               : Real_Float_Vector (Weights'Range);
      Diff            : Real_Float_Vector (Weights'Range);
      Delta_Weights   : Real_Float_Vector (Weights'Range);
      New_Weights     : Real_Float_Vector (Weights'Range);
      Current_Loss    : Float;
      --  Stop searching near a local minimum
      Done            : Boolean := False;
   begin
      while not Done loop
         --   take a peek at the weights, the learning
         --   rate, and the current loss
         if Maths.Random_Float < 0.01 then
            null;
            --           Put_Line (w, Alpha, Loss (w, Data, Labels));
         end if;
         --  The next few lines compute the gradient
         --  Delta_Weight is the change in  weights suggested by the gradient
         H := Data_F * Current_Weights;  --  h
         --  transform using the sigmoid function
         Exp_H := Exp (-H);
         Y := 1.0 / (1.0 + Exp_H);
         --  Delta_Weight = np.add.reduce
         --  (np.reshape((labs-y) * np.exp(-h)*y**2,(len(y),1)) * alldat)
         --  if we take a step of size alpha and update
         Diff := (Y - Labels_F);
         Delta_Weights := Mult (Diff, Exp_H);
         Delta_Weights := Delta_Weights * (Y ** 2);
--           Delta_Weights := Delta_Weights * (Y ** 2) * Data_F;

         Current_Loss := Loss (Data, Current_Weights, Labels);
         Alpha := 2.0 * Alpha;
         New_Weights := Current_Weights + Alpha * Delta_Weights;

         while Loss (Data, New_Weights, Labels) >= Current_Loss and not Done loop
            Alpha := Alpha / 2.0;
            Done := Alpha * max (abs (Delta_Weights)) < 0.0001;
            if Done then
               null;
               --  print(Alpha, Delta_Weights)
            else
               New_Weights := Current_Weights + Alpha * Delta_Weights;
               if not done then
                  Current_Weights := New_Weights;
               end if;
            end if;
         end loop;
      end loop;
      return (Current_Weights);

   end Fit;

   --  -------------------------------------------------------------------------

   function Loss (Data   : Integer_Matrix; Weights : Real_Float_Vector;
                  Labels : Integer_Array) return Float is
      use Real_Float_Arrays;
      H     : constant Real_Float_Vector := To_Real_Float_Matrix (Data) * Weights;
      --  transform using the sigmoid function
      Exp_H : constant Real_Float_Vector := Exp (-H);
      Y     : constant Real_Float_Vector := 1.0 / (1.0 + Exp_H);
      Diff  : Real_Float_Vector (Labels'Range);
   begin
      --  take the difference between the labels and the output of the
      --  sigmoid squared, then sum over all instances to get the
      --  total loss.
      Diff := (Y - To_Real_Float_Vector (Labels)) ** 2;

      return Sum (Diff);

   end Loss;

   --  -------------------------------------------------------------------------

   function Mult (L, R : Real_Float_Vector) return Real_Float_Vector is
      Routine_Name : constant String := "ML.Mult ";
      Result       : Real_Float_Vector := L;
   begin
      Assert (L'Length = R'Length, Routine_Name &
                "vectors have different lengths.");
      for index in L'Range loop
            Result (index) := Result (index)  * R (index);
      end loop;

      return Result;

   end Mult;

   --  -------------------------------------------------------------------------

end ML;
