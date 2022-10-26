
with Ada.Assertions; use Ada.Assertions;
with Ada.Numerics.Float_Random;
with Ada.Text_IO; use Ada.Text_IO;

with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Samples_Generator;
--  with Test_Support; use Test_Support;

procedure Test_Shuffle is
   use Real_Float_Arrays;
   use Multilayer_Perceptron;

   Routine_Name : constant String := "Test_Shuffle ";
   Weights      :  NL_Types.Float_List;
   Data         : constant Samples_Generator.Classification_Test_Data :=
                    Samples_Generator.Make_Classification
     (Weights => Weights, Shuffle => False);
   X            : constant Real_Float_Matrix := Data.X;
   Y            : constant Integer_Matrix := To_Integer_Matrix (Data.Y);
   Gen_State    : Ada.Numerics.Float_Random.State;
   Layer_Sizes  : NL_Types.Integer_List;
   Classifier1  : MLP_Classifier;
   Classifier2  : MLP_Classifier;

   procedure Test (MLP1, MLP2         : out MLP_Classifier;
                   Shuffle1, Shuffle2 : Boolean) is
   begin
      MLP1 := C_Init
        (Max_Iter => 1, Hidden_Layer_Sizes => Layer_Sizes, Batch_Size => 1,
         Random_State => 0, Shuffle => Shuffle1);
      MLP2 := C_Init
        (Max_Iter => 1, Hidden_Layer_Sizes => Layer_Sizes, Batch_Size => 1,
         Random_State => 0, Shuffle => Shuffle2);

      Init_Optimizer (MLP1);
      Init_Optimizer (MLP2);

      Ada.Numerics.Float_Random.Reset
        (Multilayer_Perceptron.Float_Gen, Gen_State);
      Fit (MLP1, X, Y);

      Ada.Numerics.Float_Random.Reset
        (Multilayer_Perceptron.Float_Gen, Gen_State);
      Fit (MLP2, X, Y);

   end Test;

begin
   Put_Line (Routine_Name);
   Ada.Numerics.Float_Random.Save (Multilayer_Perceptron.Float_Gen, Gen_State);
   Layer_Sizes.Append (1);

   Test (Classifier1, Classifier2, False, False);

   Assert (Classifier2.Attributes.Params (1).Coeff_Gradients =
             Classifier1.Attributes.Params (1).Coeff_Gradients,
           "False, False test failed");
   Put_Line ("Both false test passed");

   Assert (Classifier2.Attributes.Params (1).Coeff_Gradients =
             Classifier1.Attributes.Params (1).Coeff_Gradients,
           "Coeffs (1) Test failed");

   Put_Line (Routine_Name & "Coeffs tests passed");

end Test_Shuffle;
