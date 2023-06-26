
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Support_21A is

   type Trans_Array is array (Integer range <>, Integer range <>,
                              Integer range <>) of Boolean;

   function Binarize (Num_Rows, Num_Cols, Num_Cats : Positive;
                      Grid_Map : Integer_Matrix) return Trans_Array;

end Support_21A;
