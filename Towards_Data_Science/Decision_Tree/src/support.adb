--  Ref: https://github.com/random-forests/tutorials/blob/master/decision_tree.py

with Ada.Text_IO; use Ada.Text_IO;

package body Support is

   --  -----------------------------------------------------------------------

   function Class_Counts (Rows : Support.Rows_Vector)
                          return Support.Count_Package.Map is
      use Rows_Package;
      use Count_Package;
      Counts         : Count_Package.Map;
      Count_Cursor   : Count_Package.Cursor;
      aRow           : Row_Data;
      Label          : Label_Type;
      Count          : Natural;
   begin
      for index in Rows.First_Index .. Rows.Last_Index loop
         aRow := Rows.Element (index);
         Label := aRow.Fruit;
--           Put_Line ("Class_Counts Label " & Label_Type'Image (Label));
         if not Counts.Contains (Label) then
            Counts.Insert (Label, 0);
         end if;
         Count_Cursor := Counts.Find (Label);
         Count := Counts.Element (Label);
         Counts.Replace_Element (Count_Cursor, Count + 1);
--           Put_Line ("Label replaced "  & Label_Type'Image (Label) &
--                     " " & Integer'Image (Counts.Element (Label)));
      end loop;
      return Counts;
   end Class_Counts;

   --  ---------------------------------------------------------------------------

   function Gini (Rows : Rows_Vector) return Float is
      use Count_Package;
      Counts    : Count_Package.Map;
      Rows_Size : constant Float := Float (Rows.Length);
      Impurity  : Float := 1.0;
      procedure Calc_Impurity (Curs : Count_Package.Cursor) is
         Label_Probability : Float range 0.0 .. 1.0;
      begin
         Label_Probability := Float (Element (Curs)) / Rows_Size;
         Impurity := Impurity - Label_Probability ** 2;
      end Calc_Impurity;
   begin
      Counts := Class_Counts (Rows);
      Counts.Iterate (Calc_Impurity'Access);
      return Impurity;
   end Gini;

   --  ---------------------------------------------------------------------------
   --  Uncertainty is the uncertainty of the starting node minus
   --  the weighted impurity of two child nodes
   function Information_Gain (Left, Right : Rows_Vector;
                              Current_Uncertainty : Float) return float is
      Left_Length : constant Float := Float (Left.Length);
      P           : constant Float := Left_Length /
        (Left_Length + Float (Right.Length));
   begin
      return Current_Uncertainty -
        P * Gini (Left) - (1.0 - P) * Gini (Right);
   end Information_Gain;

   --  ---------------------------------------------------------------------------

   procedure Print_Question (Self : Question_Type) is
      C : constant Feature_Type := Self.Feature;
   begin
      case C is
         when Colour_Feature =>
               Put_Line ("Is " & To_String (Header (1)) & " = " & " " &
                          Colour_Type'Image (Self.Colour_Value));
         when Diameter_Feature =>
               Put_Line ("Is " & To_String (Header (2)) & " >= " & " " &
                         Integer'Image (Self.Diameter_Value));
      end case;
   end Print_Question;

   --  --------------------------------------------------------------------------

   procedure Print_Rows (Label : String; Rows : Rows_Vector) is
      use Rows_Package;
      aRow : Row_Data;
   begin
      Put_Line (Label);
      for index in Rows.First_Index .. Rows.Last_Index loop
         aRow := Rows.Element (index);
         Put ("(" & Colour_Type'Image (aRow.Colour) & " " &
                     Integer'Image (aRow.Diameter)  & " " &
                Label_Type'Image (aRow.Fruit) & ")");
         if index /= Rows.Last_Index then
            Put (", ");
         end if;
      end loop;
      New_Line;
   end Print_Rows;

   --  --------------------------------------------------------------------------

   function To_Vector (Rows : Row_Array) return Rows_Vector is
      New_Vector : Rows_Vector;
   begin
      for index in Rows'Range loop
         New_Vector.Append (Rows (index));
      end loop;
      return New_Vector;
   end To_Vector;

   --  --------------------------------------------------------------------------

end Support;
