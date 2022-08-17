
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Connectionist is

   type Adaline_Class (Num_Weights : Positive) is record
      Num_Inputs : Positive;
      Weights    : Real_Float_Vector (1 .. Num_Weights);
   end record;

   procedure Run_Adaline;

end Connectionist;
