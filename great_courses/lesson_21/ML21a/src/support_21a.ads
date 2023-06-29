
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;

package Support_21A is

   type Trans_Tensor is array (Integer range <>, Integer range <>,
                               Integer range <>) of Binary;
   type Float_Tensor is array (Integer range <>, Integer range <>,
                               Integer range <>) of Float;

   function Binarize (Classifier : Python.Module;
                      Num_Rows, Num_Cols, Num_Cats : Positive;
                      Grid_Map : Integer_Matrix) return Trans_Tensor;
   procedure Plot_Policy (Pi : Real_Float_Matrix; Acts : Integer_Matrix;
                          Num_Rows, Num_Cols : Positive);

end Support_21A;
