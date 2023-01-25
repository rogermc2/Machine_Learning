
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;
with Support_5A;

package body ML is

   function Mult_3 (L, M, R : Real_Float_Vector) return Real_Float_Vector;
   function Row_Multiply (L : Real_Float_Vector; R : Real_Float_Matrix)
                          return Real_Float_Matrix;
   function Sigmoid (H : Real_Float_Vector) return Real_Float_Vector;

   --  -------------------------------------------------------------------------

   procedure Display_Forest (Image_File_Name : String) is
      use Support_5A;
      Image_Data : constant Unsigned_8_Array_3D :=
                     Get_Picture (Image_File_Name);
   begin
      null;
   end Display_Forest;

   --  -------------------------------------------------------------------------

   procedure Fit (Weights : in out Real_Float_Vector; All_Data : Integer_Matrix;
                  Labels  : Integer_Array; Verbose : Boolean := False) is
      --  All_Data is a n x 4 matrix of pixel r, g, b + offset
      use Real_Float_Arrays;
      Routine_Name    : constant String := "ML.Fit ";
      F_All_Data      : constant Real_Float_Matrix :=
                          To_Real_Float_Matrix (All_Data);
      F_Labels        : constant Real_Float_Vector :=
                          To_Real_Float_Vector (Labels);
      Learn_Rate      : Float := 0.1;
      --  Y = w1 r + w2 g + w3 b + offset
      Y               : Real_Float_Vector (All_Data'Range);
      Y_Sig           : Real_Float_Vector (All_Data'Range);
      Y_Sig_Sq        : Real_Float_Vector (All_Data'Range);
      Errors          : Real_Float_Vector (All_Data'Range);
      Errors_x_Grad   : Real_Float_Vector (All_Data'Range);
      Delta_Matrix    : Real_Float_Matrix (All_Data'Range, All_Data'Range (2));
      Delta_Weights   : Real_Float_Vector (Weights'Range);
      New_Weights     : Real_Float_Vector (Weights'Range);
      Current_Loss    : Float := Float'Safe_Last;
      Iteration       : Natural := 0;
      Descend         : Boolean;
      Done            : Boolean := False;  --  Stop near a local minimum
   begin
      Assert (Weights'Length = All_Data'Length (2), Routine_Name &
                "Invalid Weights length");
      Put_Line ("Fitting data.");

      while not Done loop
         Iteration := Iteration + 1;

         Y := F_All_Data * Weights;  --  h
         --  transform Y to the range 0 .. 1.0 using the sigmoid function
         Y_Sig := Sigmoid (Y);

         --  F_Labels are the expected solutions, 0 or 1
         Errors := F_Labels - Y_Sig;

         Y_Sig_Sq := Y_Sig ** 2;
         Errors_x_Grad := Mult_3 (Errors, Exp (-Y), Y_Sig_Sq);
         Delta_Matrix := Row_Multiply (Errors_x_Grad, F_All_Data);
         --  Sum_Each_Column is equivalent to numpy add.reduce
         Delta_Weights := Sum_Each_Column (Delta_Matrix);

         Current_Loss := Loss (Weights, All_Data, Labels);
         Learn_Rate := 2.0 * Learn_Rate;

         New_Weights := Weights + Learn_Rate * Delta_Weights;

         if Maths.Random_Float < 0.01 then
            if Verbose then
               Put_Line ("**** Iteration " & Integer'Image (Iteration) &
                           " ****");
               Put_Line ("Learning Rate: " & Float'Image (Learn_Rate) &
                           "  Loss: " & Float'Image (Current_Loss));
               Print_Float_Vector ("Weights", Weights);
               New_Line;
            else
               Put ("*");
            end if;
         end if;

         Descend := True;
         while Descend loop
            Learn_Rate := Learn_Rate / 2.0;
            Done := Learn_Rate * Max (abs (Delta_Weights)) < 0.0001;
            if not Done then
               New_Weights := Weights + Learn_Rate * Delta_Weights;
            end if;
            Descend := not Done and
              Loss (New_Weights, All_Data, Labels) >= Current_Loss;
         end loop;

         if not Done then
            Weights := New_Weights;
         end if;

      end loop;

      New_Line;
      Put_Line ("Total iterations:" & Integer'Image (Iteration));
      Put_Line ("FinaL Learn Rate: " & Float'Image (Learn_Rate) &
                  "  Final loss:" &
                  Float'Image (Loss (Weights, All_Data, Labels)));
      New_Line;

   end Fit;

   --  -------------------------------------------------------------------------

   function Loss (Weights : Real_Float_Vector; All_Data : Integer_Matrix;
                  Labels  : Integer_Array) return Float is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "ML.Loss ";
      H      : constant Real_Float_Vector :=
                 To_Real_Float_Matrix (All_Data) * Weights;
      --  transform using the sigmoid function
      Y      : constant Real_Float_Vector := 1.0 / (1.0 + Exp (-H));
      Errors : Real_Float_Vector (Labels'Range);
   begin
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

   function Row_Multiply (L : Real_Float_Vector; R : Real_Float_Matrix)
                          return Real_Float_Matrix is
      Routine_Name : constant String := "ML.Row_Multiply ";
      Result       : Real_Float_Matrix (R'Range, R'Range (2));
   begin
      Assert (L'Length = R'Length, Routine_Name &
                "vectors have different lengths.");
      for row in R'Range loop
         for col in R'Range (2) loop
            Result (row, col) := L (row) * R (row, col);
         end loop;
      end loop;

      return Result;

   end Row_Multiply;

   --  -------------------------------------------------------------------------

   function Sigmoid (H : Real_Float_Vector) return Real_Float_Vector is
      use Real_Float_Arrays;
   begin

      return 1.0 / (1.0 + Exp (-H));

   end Sigmoid;

   --  -------------------------------------------------------------------------

end ML;
