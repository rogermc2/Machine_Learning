
--  with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Neural;
with Load_Dataset;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Printing;
with Stochastic_Optimizers;

procedure Test_Alpha is
   --     use  Ada.Containers;
   use Real_Float_Arrays;
   use Multilayer_Perceptron;
   use Stochastic_Optimizers;

   Routine_Name  : constant String := "Test_Alpha ";
   Data          : constant Load_Dataset.Data_Record :=
                     Load_Dataset.Load_Digits;
   Features      : constant Real_Float_Matrix :=
                     To_Real_Float_Matrix (Data.Features);
   X             : Real_Float_Matrix (1 .. 100, 1 .. Data.Num_Features);
   Y             : constant Integer_Array (1 .. 100) :=
                     To_Integer_Array (Data.Target) (1 .. 100);
   Layer_Sizes   : NL_Types.Integer_List;
   Classes       : NL_Types.Integer_List;
   aClassifier   : MLP_Classifier;
   Params        : Parameters_List;

   --  -------------------------------------------------------------------------

begin
   Put_Line (Routine_Name);
   for row in 1 .. 100 loop
      for col in 1 .. Data.Num_Features loop
         X (row, col) := Features (row, col);
      end loop;
   end loop;

   Layer_Sizes.Append (10);
   for alpha_value in 1 .. 3 loop
      aClassifier := C_Init
        (Solver => Stochastic_Optimizers.Sgd_Solver,
         Alpha => Float (alpha_value),
         Hidden_Layer_Sizes => Layer_Sizes);
   end loop;

   Init_Optimizer (aClassifier);
   Params := aClassifier.Attributes.Params;

   --     Printing.Print_Float_Matrix_Formated ("Coeffs (2) errors", Coeff2_Error, 3);

end Test_Alpha;
