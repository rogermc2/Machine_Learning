
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Support_21A is

   subtype Actions_Range is Integer range -1 .. 1;
   type Actions_Matrix is array (Integer range <>,
                                 Integer range <>) of Actions_Range;

   type Boolean_Tensor is array (Integer range <>, Integer range <>,
                               Integer range <>) of Boolean;
   type Float_Tensor is array (Integer range <>, Integer range <>,
                               Integer range <>) of Float;
   type Integer_Tensor is array (Integer range <>, Integer range <>,
                               Integer range <>) of Integer;

   function Compute_Map_Matrix (Grid_Map : Integer_Matrix; Num_Cats : Positive)
                                return Boolean_Tensor;
   function Compute_Transition_Matrix
     (Num_Rows, Num_Cols, Num_Actions : Positive; Actions : Actions_Matrix;
      Mat_Map                         : Boolean_Tensor) return Boolean_Tensor;
   procedure Plot_Policy
     (Num_Rows, Num_Cols : Positive; Policy : Real_Float_Matrix;
      Actions : Actions_Matrix);
   procedure Print_Actions_Matrix (Name : String; aMatrix : Actions_Matrix);
   procedure Print_Boolean_Tensor
     (Name  : String; Tensor : Boolean_Tensor; Start : Positive := 1;
      Finish : Natural := 0);

end Support_21A;
