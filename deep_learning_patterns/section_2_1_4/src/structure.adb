
with Maths;

package body Structure is

   function Connect (Level_A, Level_B : Real_Float_Vector)
                     return Real_Float_Matrix is
      Connection : Real_Float_Matrix (Level_A'Range, Level_B'Range);
   begin
      --  Initialze connection weights
      for row in Level_A'Range loop
         for col in Level_B'Range loop
            --  Random_Float generates a random number in the range  -1.0 .. 1.0
            Connection (row, col) := Maths.Random_Float;
         end loop;
      end loop;
      return Connection;

   end Connect;

end Structure;
