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
      K            : Float_Array (1 .. 3) := (1.0, 0.3, 0.5);
      Start_Params : Integer_Array (1 .. 3) := (others => 0);
      Solution     : Float_Array (1 .. 3) :=
                       (0.0, -0.524869316, 0.487525860);
      Max_Iter     : Positive := 1000;
      Func_Calls   : Natural := 0;
      Grad_Calls   : Natural := 0;
   end record;

   procedure Func (Self : in out Check_Data; X : Real_Float_Matrix);

end Check_Optimize;
