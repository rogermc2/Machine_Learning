
with Ada.Assertions; use Ada.Assertions;
with Maths;

package body ML is

   procedure Mult (Data    : in out Integer_Matrix;
                   Weights : Real_Float_Vector);

   --  -------------------------------------------------------------------------

   function Fit (Weights : Real_Float_Vector; Data: Integer_Matrix;
                 Labels  : Integer_Array) return Real_Float_Vector is
      Current_Weights : Real_Float_Vector := Weights;
      Alpha           : Float := 0.1;
      Weighted_Data   : Integer_Matrix := Data;
      Y               : Real_Float_Matrix (Data'Range, Data'Range (2));
      Diff            : Real_Float_Matrix (Data'Range, Data'Range (2));
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
            --           Put_Line (w, Alpha, Loss (w,Data, Labels));
         end if;
         --  The next few lines compute the gradient
         --   important right now.
         --  Delta_Weight is the change in the weights
         --   suggested by the gradient
         Mult (Weighted_Data, Current_Weights);
         --  transform using the sigmoid function
         Y := 1.0 / (1.0 + Exp (To_Real_Float_Matrix (-Weighted_Data)));
         --      Delta_Weight = np.add.reduce(np.reshape((labs-y) * np.exp(-h)*y**2,(len(y),1)) * alldat)
         --  if we take a step of size alpha and update
         Diff := (Y - To_Real_Float_Vector (Labels)) ** 2;

         Current_Loss := Loss (Data, Current_Weights, Labels);
         Alpha := 2.0 * Alpha;
         New_Weights := Current_Weights + Alpha * Delta_Weights;
         while Loss (Data, New_Weights, Labels) >= Current_Loss and not Done loop
            Alpha := Alpha / 2.0;
            Done := Alpha * max (abs (Delta_Weights)) < 0.0001;
            if Done then
               null;
               --                 print(Alpha, Delta_Weights)
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
      Weighted_Data : Integer_Matrix := Data;
      Y             : Real_Float_Matrix (Data'Range, Data'Range (2));
      Diff          : Real_Float_Matrix (Data'Range, Data'Range (2));
   begin
      Mult (Weighted_Data, Weights);
      --  transform using the sigmoid function
      Y := 1.0 / (1.0 + Exp (To_Real_Float_Matrix (-Weighted_Data)));
      --  take the difference between the labels and the output of the
      --  sigmoid squared, then sum over all instances to get the
      --  total loss.
      Diff := (Y - To_Real_Float_Vector (Labels)) ** 2;

      return Sum (Diff);

   end Loss;

   --  -------------------------------------------------------------------------

   procedure Mult (Data    : in out Integer_Matrix;
                   Weights : Real_Float_Vector) is
      Routine_Name : constant String := "Support_5A.Mult ";
   begin
      Assert (Weights'Length = Data'Length (2), Routine_Name &
                "Weights length is different to number of Data columns.");
      for row in Data'Range loop
         for col in Data'Range (2) loop
            Data (row, col) :=
              Integer (Weights (col) * Float (Data (row, col)));
         end loop;
      end loop;

   end Mult;

   --  -------------------------------------------------------------------------


end ML;
