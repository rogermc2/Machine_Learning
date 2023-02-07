
with Ada.Numerics;
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;

package body Support_7Aux is

   function Median (Data : Real_Float_Vector) return Float is
      Data_Length : constant Positive := Data'Length;
      Sorted_Data : Real_Float_Vector := Data;
      Min_Index   : Integer;
      Min_Value   : Float;
      theMedian   : Float;
      Swap        : Float;
   begin
      for index in Data'Range loop
         Min_Index := index;
         Min_Value := Sorted_Data (index);

         for index_2 in index + 1 .. Sorted_Data'Last loop
            if Sorted_Data (index_2) < Min_Value then
               Min_Index := index_2;
               Min_Value := Sorted_Data (index_2);
            end if;
         end loop;

         Swap := Sorted_Data (index);
         Sorted_Data (index) := Sorted_Data (Min_Index);
         Sorted_Data (Min_Index) := Swap;
      end loop;

      if Data_Length mod 2 /= 0 then
         theMedian := Data (Data_Length / 2 + 1);
      else
         theMedian := Data (Data_Length / 2) + Data (Data_Length / 2 + 1) / 2.0;
      end if;

      return theMedian;

   end Median;
   --  -------------------------------------------------------------------------

   function Shoot (Angle : Float) return Float is
      use Ada.Numerics;
      use Maths.Float_Math_Functions;
      Vel_Angle : constant Float := Angle / 180.0 * Pi;
      Force     : constant Float := 2.3;
      X_Target  : constant Float := 20.0;
      Time_Step : constant Float := 0.001;
      Gravity   : constant Float := -1.0;
      Y_Accel   : constant Float := Gravity * Time_Step;
      X_Pos     : Float := 10.0;
      Y_Pos     : Float := 50.0;
      X_Vel     : Float := Force * Cos (Vel_Angle);
      Y_Vel     : Float := Force * Sin (Vel_Angle);
   begin
      while Y_Pos > 0.0 loop
         X_Pos := X_Pos + X_Vel * Time_Step;
         Y_Pos := Y_Pos + Y_Vel * Time_Step;

         if X_Pos <= 0.0 then
            X_Vel := -X_Vel;
         end if;
         Y_Vel := Y_Vel + Y_Accel;
      end loop;

      return abs (X_Pos - X_Target);

   end Shoot;

   --  -------------------------------------------------------------------------

   function Show (Angle : Float) return XY_Data is
      use Ada.Numerics;
      use Maths.Float_Math_Functions;
      Vel_Angle : constant Float := Angle / 180.0 * Pi;
      Force     : constant Float := 2.3;
      Time_Step : constant Float := 0.001;
      Gravity   : constant Float := -1.0;
      Y_Accel   : constant Float := Gravity * Time_Step;
      X_Pos     : Float := 10.0;
      Y_Pos     : Float := 50.0;
      X_Vel     : Float := Force * Cos (Vel_Angle);
      Y_Vel     : Float := Force * Sin (Vel_Angle);
      Xs_Ys     : XY_Data;
   begin
      Xs_Ys.Xs.Append (X_Pos);
      Xs_Ys.Ys.Append (Y_Pos);

      while Y_Pos > 0.0 loop
         X_Pos := X_Pos + X_Vel * Time_Step;
         Y_Pos := Y_Pos + Y_Vel * Time_Step;

         if X_Pos <= 0.0 then
            X_Vel := -X_Vel;
         end if;
         Y_Vel := Y_Vel + Y_Accel;
         Xs_Ys.Xs.Append (X_Pos);
         Xs_Ys.Ys.Append (Y_Pos);
      end loop;

      return Xs_Ys;

   end Show;

   --  -------------------------------------------------------------------------

   function Trajectory (Angle : Float) return XY_Data is
   begin
      return Show (Angle);

   end Trajectory;

   --  -------------------------------------------------------------------------

end Support_7Aux;
