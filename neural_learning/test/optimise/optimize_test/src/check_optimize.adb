--  Based on /scipy/scipy/optimize/tests/test_optimize.py
--  CheckOptimize class

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

package body Check_Optimize is

   function Func (Self : in out Check_Data; X : Real_Float_Vector)
                  return Float is
      use Real_Float_Arrays;
      use Maths.Float_Math_Functions;
      Routine_Name : constant String := "Check_Optimize.Func ";
      Log_PDot     : constant Real_Float_Vector := Self.F * X;
      PDot         : constant Real_Float_Vector := Exp (Log_PDot);
      PDot_Sum     : Float := 0.0;
   begin
      Self.Func_Calls := Self.Func_Calls + 1;
      Assert (Self.Func_Calls < 6000, Routine_Name &
                "too many iterations in optimization routine");
      for row in PDot'Range loop
         PDot_Sum := PDot_Sum + PDot (row);
      end loop;
      Self.Trace.Append (X);

      return Log (PDot_Sum) - Self.K * X;

   end Func;

   --  -------------------------------------------------------------------------

   function Grad (Self : in out Check_Data; X : Real_Float_Vector)
                  return Real_Float_Vector is
      use Real_Float_Arrays;
      use Maths.Float_Math_Functions;
--        Routine_Name : constant String := "Check_Optimize.Grad ";
      Log_PDot     : constant Real_Float_Vector := Self.F * X;
      PDot         : constant Real_Float_Vector := Exp (Log_PDot);
      PDot_Sum     : Float := 0.0;
   begin
      Self.Grad_Calls := Self.Grad_Calls + 1;
      for row in PDot'Range loop
         PDot_Sum := PDot_Sum + PDot (row);
      end loop;

      return Transpose (Self.F) * Exp (Log_PDot - Log (PDot_Sum));

   end Grad;

   --  -------------------------------------------------------------------------

end Check_Optimize;
