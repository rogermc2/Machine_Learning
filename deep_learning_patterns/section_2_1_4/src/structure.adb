
with Maths;

package body Structure is

   function Connect (Level_A, Level_B : Node) return Real_Float_Matrix is
      Connection : Real_Float_Matrix (Level_A.Level'Range,
                                      Level_B.Level'Range);
   begin
      --  Initialze connection weights
      for row in Connection'Range loop
         for col in Connection'Range (2) loop
            --  Random_Float generates a random number in the range  -1.0 .. 1.0
            Connection (row, col) := Maths.Random_Float;
         end loop;
      end loop;
      return Connection;

   end Connect;

end Structure;
