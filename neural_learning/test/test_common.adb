
package body Test_Common is

   procedure Init is
      use NL_Types;
      I2 : Integer_Array (1 .. 2) := (4, 6);
      I3 : constant Integer_Array (1 .. 3) := (7, 8, 9);
   begin
      Shapes.Append (I2);
      I2 := (6, 8);
      Shapes.Append (I2);
      Shapes.Append (I3);
   end Init;

end Test_Common;
