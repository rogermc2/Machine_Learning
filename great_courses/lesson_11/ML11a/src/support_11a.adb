
package body Support_11A is

   function Assign_Data (Data, Centres : Real_Float_Matrix;
                         Centre_Ids    : out Integer_Array) return Float is
      --        Dim    : constant Positive := Data'Length (2);
      Diff   : array (1 .. Centres'Length, 1 .. Data'Length,
                      Data'Range (2)) of Float;
      Diff2  : array (1 .. Centres'Length, 1 .. Data'Length,
                      Data'Range (2)) of Float;
      Total  : Real_Float_Matrix (1 .. Centres'Length, 1 .. Data'Length) :=
                 (others => (others => 0.0));
      Result : Float;
   begin
      for d3 in Diff'Range loop
         for row in Data'Range loop
            for col in Data'Range (2) loop
               Diff (d3, row, col) := Data (row, col) - Centres (d3, col);
               Diff2 (d3, row, col) := Diff (d3, row, col) ** 2;
            end loop;
         end loop;
      end loop;

      for d3 in Diff2'Range loop
         for row in Total'Range loop
            for col in Total'Range (2) loop
               Total (row, col) := Total (row, col) + Diff2  (d3, row, col);
            end loop;
         end loop;
      end loop;

      return Result;

   end Assign_Data;

   --  ------------------------------------------------------------------------

end Support_11A;
