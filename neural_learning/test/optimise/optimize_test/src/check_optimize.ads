--  Based on /scipy/scipy/optimize/tests/test_optimize.py
--  CheckOptimize class

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
--  with Optimise; use Optimise;

package Check_Optimize is

   type Check_Data is record
      F            : Real_Float_Matrix (1 .. 5, 1 .. 3) :=
                       ((1.0, 1.0, 1.0),
                        (1.0, 1.0, 0.0),
                        (1.0, 0.0, 1.0),
                        (1.0, 0.0, 0.0),
                        (1.0, 0.0, 0.0));
      K            : Real_Float_Vector (1 .. 3) := (1.0, 0.3, 0.5);
      Start_Params : Integer_Array (1 .. 3) := (others => 0);
      Solution     : Real_Float_Vector (1 .. 3) :=
                       (0.0, -0.524869316, 0.487525860);
      Max_Iter     : Positive := 1000;
      Func_Calls   : Natural := 0;
      Grad_Calls   : Natural := 0;
      Trace        : Real_Vector_List;
   end record;

   function Func (Self : in out Check_Data; X : Real_Float_Vector) return Float;
   function Grad (Self : in out Check_Data; X : Real_Float_Vector)
                  return Real_Float_Vector;

end Check_Optimize;
