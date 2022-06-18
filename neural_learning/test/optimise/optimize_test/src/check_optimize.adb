--  Based on /scipy/scipy/optimize/tests/test_optimize.py
--  CheckOptimize class

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

package body Check_Optimize is

   procedure Func (Self : in out Check_Data; X : Real_Float_Matrix) is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Check_Optimize.Func ";
      Log_PDot : constant Real_Float_Matrix := Self.F * X;
      PDot     : constant Real_Float_Matrix := Exp (Log_PDot);
      PDot_Sum : Float := 0.0;
   begin
      Self.Func_Calls := Self.Func_Calls + 1;
      Assert (Self.Func_Calls < 6000, Routine_Name &
                "too many iterations in optimization routine");
      for row in PDot'Range loop
         for col in PDot'Range (2) loop
            PDot_Sum := PDot_Sum + PDot (row, col);
         end loop;
      end loop;
   end Func;

end Check_Optimize;
