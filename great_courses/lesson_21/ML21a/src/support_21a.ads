
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;

package Support_21A is

   subtype Actions_Range is Integer range -1 .. 1;
   type Actions_Matrix is array (Integer range <>, Integer range <>) of Actions_Range;

--     type Binary_Tensor is array (Integer range <>, Integer range <>,
--                                 Integer range <>) of Binary;
   type Boolean_Tensor is array (Integer range <>, Integer range <>,
                               Integer range <>) of Boolean;
   type Float_Tensor is array (Integer range <>, Integer range <>,
                               Integer range <>) of Float;
   type Integer_Tensor is array (Integer range <>, Integer range <>,
                               Integer range <>) of Integer;

   function Binarize (Classifier : Python.Module;
                      Num_Rows, Num_Cols, Num_Cats : Positive;
                      Grid_Map : Integer_Matrix) return Boolean_Tensor;
   procedure Plot_Policy (Policy : Real_Float_Matrix; Actions : Actions_Matrix);

end Support_21A;
