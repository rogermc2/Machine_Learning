
with Ada.Numerics;
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;

package body Support_7Aux is

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

end Support_7Aux;
