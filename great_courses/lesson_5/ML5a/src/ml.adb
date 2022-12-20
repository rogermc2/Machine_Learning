
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;

package body ML is

   function Mult (L, M, R : Real_Float_Vector) return Real_Float_Vector;
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
      Step_Size       : Float := 0.1;
      --  Y1 = h = np.matmul(alldat,w) dot product
      --  Y1i = w1xi1 + w2x21 + ... wnxn1 + w_offset
      Y1              : Real_Float_Vector (Data'Range);
      Exp_Y1          : Real_Float_Vector (Data'Range);
      Y               : Real_Float_Vector (Data'Range);
      Errors          : Real_Float_Vector (Data'Range);
      Mult_Vec        : Real_Float_Vector (Data'Range);
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
         --   take a peek at the weights, the learning
         --   rate, and the current loss
         if Maths.Random_Float < 0.01 then
            null;
            --           Put_Line (w, Alpha, Loss (w, Data, Labels));
         end if;
         --  The next few lines compute the gradient
         --  Delta_Weight is the change in  weights suggested by the gradient

         Y1 := Dot (Data, Weights);  --  h
         Print_Float_Vector ("Y1", Y1, 276120, 276130);
         --  transform using the sigmoid function
         Exp_Y1 := Exp (-Y1);
         Y := 1.0 / (1.0 + Exp_Y1);
         --  delta_w is the change in the weights suggested by the gradient
         --  Delta_Weight = np.add.reduce
         --  (np.reshape((labs-y) * np.exp(-h)*y**2,(len(y),1)) * alldat)
         Errors := (F_Labels - Y);
         Mult_Vec := Mult (Errors, Exp_Y1, Y) ** 2;
         Put_Line ("Max Mult_Vec" &
                     Float'Image (Max (Mult_Vec)));
         Put_Line ("Mult_Vec length" & Integer'Image (Mult_Vec'Length));
         Put_Line ("F_Data length" & Integer'Image (F_Data'Length));
         Put_Line ("Delta_Weights length" &
                     Integer'Image (Delta_Weights'Length));
         Put_Line ("Max Mult_Vec" & Float'Image (Max (Mult_Vec)));
         Put_Line ("Min Mult_Vec" & Float'Image (Min (Mult_Vec)));
         Put_Line ("Max F_Data" & Float'Image (Max (F_Data)));
         Put_Line ("Min F_Data" & Float'Image (Min (F_Data)));
         Put_Line ("255 * F_Data length" & Integer'Image (255 * F_Data'Length));
         Put_Line ("Max Mult_Vec matrix" &
                     Float'Image (Max (Vec_To_1xN_Matrix (Mult_Vec))));
         declare
            Mat : constant Real_Float_Matrix :=
                    Vec_To_1xN_Matrix (Mult_Vec) * F_Data;
         begin
            Print_Matrix_Dimensions ("Vec_To_1xN_Matrix (Mult_Vec) ",
                                     Vec_To_1xN_Matrix (Mult_Vec) );
            Put_Line ("Max Mat: " & Float'Image (Max (Mat)));
            Put_Line ("Min Mat: " & Float'Image (Min (Mat)));
            Put_Line ("Mat (1, 1): " & Float'Image (Mat (1, 1)));
            Put_Line ("Mat (1, 2): " & Float'Image (Mat (1, 2)));
            Put_Line ("Mat (1, 3): " & Float'Image (Mat (1, 3)));
            Put_Line ("Mat (1, 4): " & Float'Image (Mat (1, 4)));
            Print_Float_Matrix ("Mat", Mat);
            for index in Mat'Range (2) loop
               Delta_Weights (index) := Mat (1, index);
            end loop;
         end;
--           for row in F_Data'Range loop
--              Sum := 0.0;
--              for col in F_Data'Range (2) loop
--                 Sum := Sum + Mult_Vec (row) * F_Data (row, col);
--              end loop;
--              Delta_Weights (row) := Sum;
--           end loop;
         --           Print_Float_Vector ("Mult (Errors, Exp_Y1, Y)",
         --                               Mult (Errors, Exp_Y1, Y));
         Print_Float_Vector ("Delta_Weights", Delta_Weights);

         --  Get new weights by taking a step of size alpha and updating
         Current_Loss := Loss (Weights, Data, Labels);
         Step_Size := 2.0 * Step_Size;
         New_Weights := Weights + Step_Size * Delta_Weights;
         Put_Line ("Step_Size: " & Float'Image (Step_Size));
         Put_Line ("Current_Loss: " & Float'Image (Current_Loss));
         Print_Float_Vector ("New_Weights", New_Weights);
         Put_Line ("Next Loss: " &
                     Float'Image (Loss (New_Weights, Data, Labels)));

         while Loss (New_Weights, Data, Labels) >= Current_Loss and
           not Done loop
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
         Done := Step_Size * Max (abs (Delta_Weights)) < 0.0001;

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

   function Vec_To_1xN_Matrix (Vec : Real_Float_Vector)
                                return Real_Float_Matrix is
      Routine_Name : constant String := "ML.Vec_To_1xN_Matrix ";
      Result : Real_Float_Matrix (1 .. 1, Vec'Range);
   begin
      for index in Vec'Range loop
         Assert (Vec (index)'Valid, Routine_Name & "invalid Vec value at (" &
                Integer'Image (index) & ")");
         Result (1, index) := Vec (index);
         Assert (Result (1, index)'Valid, Routine_Name & "invalid Result at (" &
                Integer'Image (1) & Integer'Image (index) & ")");
      end loop;

      return Result;

   end Vec_To_1xN_Matrix;

   --  -------------------------------------------------------------------------

end ML;
