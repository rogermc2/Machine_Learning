
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Load_Dataset;
with Multilayer_Perceptron;
with ML_Types;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Printing;
with Stochastic_Optimizers;

--  Test_Alpha tests that larger alpha yields weights closer to zero.
procedure Test_Alpha is
   use NL_Types;
   use NL_Types.Float_Package;
   use NL_Types.Float_List_Package;
   use Real_Float_Arrays;
   use Multilayer_Perceptron;
   use Stochastic_Optimizers;

   --   np.arange(2) means array([0, 1])
   subtype Alpha_Values is Natural range 0 .. 1;

   Routine_Name  : constant String := "Test_Alpha ";
   Data          : constant Load_Dataset.Digits_Data_Record :=
                     Load_Dataset.Load_Digits ("../../digits.csv");
   Features      : constant Real_Float_Matrix :=
                     To_Real_Float_Matrix (Data.Features);
   X             : Real_Float_Matrix (1 .. 100, 1 .. Data.Num_Features);
   Y             : Integer_Matrix (1 .. 100, 1 .. 1) :=
                     (others => (others => 0));
   Layer_Sizes   : ML_Types.Integer_List;
   Coeffs_Sum    : NL_Types.Float_List;
   Alpha_Vectors : NL_Types.Float_List_2D;
   aClassifier   : MLP_Classifier;

   function Absolute_Sum (M : Real_Float_Matrix) return Float is
      Sum : Float := 0.0;
   begin
      for row in M'Range loop
         for col in M'Range (2) loop
            Sum := Sum + abs (M (row, col));
         end loop;
      end loop;

      return Sum;

   end Absolute_Sum;

   --  -------------------------------------------------------------------------

begin
   Put_Line (Routine_Name);
   for row in 1 .. 100 loop
      for col in 1 .. Data.Num_Features loop
         X (row, col) := Features (row, col);
      end loop;
      Y (row, 1) := Data.Target (row);
   end loop;
   Printing.Print_Matrix_Dimensions ("X", X);
   Printing.Print_Matrix_Dimensions ("Y", Y);

   Layer_Sizes.Append (10);
   for alpha_value in Alpha_Values'Range loop
      aClassifier := C_Init
        (Alpha => Float (alpha_value), Random_State => 1,
         Layer_Sizes => Layer_Sizes);
      Fit (aClassifier, X, Y);

      Coeffs_Sum.Clear;
      for index in 1 .. 2 loop
         declare
            Params : constant Parameters_Record :=
                       aClassifier.Attributes.Params.Element (index);
         begin
            --                  Printing.Print_Parameters ("Params", Params, 1, 4);
            Coeffs_Sum.Append (Absolute_Sum (Params.Coeff_Gradients));
         end;
      end loop;

      Alpha_Vectors.Append (Coeffs_Sum);
   end loop;

   New_Line;
   for index in Alpha_Vectors.First_Index .. Alpha_Vectors.Last_Index loop
      Printing.Print_Float_List
        ("Alpha vector" & Integer'Image (index),
         Alpha_Vectors.Element (index));
   end loop;

   --  Larger alpha should yield weights closer to zero
   for index in Alpha_Vectors.First_Index .. Alpha_Vectors.Last_Index - 1 loop
      Assert (Alpha_Vectors (index) > Alpha_Vectors (index + 1),
              "Alpha_Vectors (" & Integer'Image (index) &
                ") <= Alpha_Vectors (" & Integer'Image (index + 1) & ")");
   end loop;

end Test_Alpha;
