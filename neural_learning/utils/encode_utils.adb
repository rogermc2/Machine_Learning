
--  Adapted from scikit-learn/scikit-learn.git sklearn/utils/_encode.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Printing;

package body Encode_Utils is

   --     package Bool_Sets is new Ada.Containers.Ordered_Sets (Boolean);
   --     package Float_Sets is new Ada.Containers.Ordered_Sets (Float);

   function Encode_Check_Unknown
     (Values : Integer_Array; Uniques : Integer_Array)
      return Integer_Array;

   --  -------------------------------------------------------------------------

   function Encode (Values : Integer_Array) return Integer_Array is
      Sorted_Values : Integer_Array := Values;
   begin
      Integer_Array_Sort (Sorted_Values);
      return Unique (Sorted_Values);

   end Encode;

   --  -------------------------------------------------------------------------
   --  Values : values to encode.
   --  Uniques : unique values in Values; Uniques needs to be sorted.
   --  Check_Unknown : if True check Values for values that are not in Uniques
   --  and raise an error.
   function Encode (Values        : Integer_Array;
                    Uniques       : Integer_Array;
                    Check_Unknown : Boolean := True) return Natural_Array is
      Result : Natural_Array (1 .. Values'Length);
   begin
      Result := Map_To_Integer (Values, Uniques);
      if Check_Unknown then
         declare
            Diff : constant Integer_Array :=
                     Encode_Check_Unknown (Values, Uniques);
         begin
            if Diff'Last > Diff'First then
               New_Line;
               Put ("Encode_Error: Encode_Utils.Encode Values contains ");
               Put_Line ("previously unseen labels.");
               Printing.Print_Integer_Array ("Unique list", Uniques);
               Printing.Print_Integer_Array ("Unseen labels", Diff);
               raise Encode_Error;
            end if;
         end;
      end if;

      return Result;

   end Encode;

   --  -------------------------------------------------------------------------

   function Encode_Check_Unknown
     (Values : Integer_Array; Uniques : Integer_Array)
      return Integer_Array is
      No_Inverse  : Natural_Array (1 .. Values'Length);
      Unique_Vals : constant Integer_Array :=
                      Encode_Utils.Unique (Values, No_Inverse);
      aVal        : Integer;
      Found       : Boolean;
      Diff_List   : NL_Types.Integer_List;
   begin
      for index in Unique_Vals'Range loop
         aVal := Unique_Vals (index);
         for u in Uniques'Range loop
            Found := Uniques (u) = aVal;
            if Found then
               Diff_List.Append (aVal);
            end if;
         end loop;
      end loop;

      return To_Integer_Array (Diff_List);

   end Encode_Check_Unknown;

   --  -------------------------------------------------------------------------
   --  Map each value based on its position in uniques.
   function Map_To_Integer (Values  : Integer_Array;
                            Uniques : Integer_Array)
                            return Natural_Array is
      Routine_Name  : constant String := "Encode_Utils.Map_To_Integer ";
      Result        : Natural_Array (1 .. Values'Length) := (others => 0);
      aValue        : Integer;
      Found         : Boolean;
      Uniques_Index : Positive;
   begin
      for index in Values'Range loop
         aValue := Values (index);
         Found := False;
         Uniques_Index := Uniques'First;
         while Uniques_Index <= Uniques'Last and not Found loop
            Found := aValue = Uniques (Uniques_Index);
            Uniques_Index := Uniques_Index + 1;
         end loop;
         Assert (Found, Routine_Name & "error, Value not found in Uniques" &
                   Integer'Image (aValue));
         Result (index) := aValue;
      end loop;

      return Result;

   end Map_To_Integer;

   --  -------------------------------------------------------------------------

   function Unique (Values : Natural_Array) return Natural_Array is
      use NL_Types.Natural_Sorting;
      aValue    : Natural;
      Uniq_List : NL_Types.Natural_List;
   begin
      for index in Values'Range loop
         aValue := Values (index);
         if not Uniq_List.Contains (aValue) then
            Uniq_List.Append (aValue);
         end if;
      end loop;

      Sort (Uniq_List);

      return To_Natural_Array (Uniq_List);

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : Integer_Array) return Integer_Array is
      use Int_Sets;
      use NL_Types.Integer_Sorting;
      Int_Value       : Integer;
      Unique_Integers : Int_Sets.Set;
      Ints_Curs       : Int_Sets.Cursor;
      Uniq_List       : NL_Types.Integer_List;
   begin
      for index in Values'Range loop
         Unique_Integers.Include (Values (index));
      end loop;

      Ints_Curs := Unique_Integers.First;
      while Int_Sets.Has_Element (Ints_Curs) loop
         Int_Value := Int_Sets.Element (Ints_Curs);
         Uniq_List.Append (Int_Value);
         Int_Sets.Next (Ints_Curs);
      end loop;

      Sort (Uniq_List);

      return To_Integer_Array (Uniq_List);

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : Boolean_Array) return Boolean_Array is
      use Bool_Sets;
      use NL_Types.Boolean_Sorting;
      Value        : Boolean;
      Unique_Bools : Bool_Sets.Set;
      Curs         : Bool_Sets.Cursor;
      Uniq_List    : NL_Types.Boolean_List;
   begin
      for index in Values'Range loop
         Unique_Bools.Include (Values (index));
      end loop;

      Curs := Unique_Bools.First;
      while Bool_Sets.Has_Element (Curs) loop
         Value := Bool_Sets.Element (Curs);
         Uniq_List.Append (Value);
         Bool_Sets.Next (Curs);
      end loop;

      Sort (Uniq_List);

      return To_Boolean_Array (Uniq_List);

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : Integer_Array; Inverse : out Natural_Array)
                    return Integer_Array is
      use Int_Sets;
      use NL_Types.Integer_Sorting;
      --        Routine_Name : constant String := "Encode_Utils.Unique ";
      Unique_Integers : Int_Sets.Set;
      Ints_Curs       : Int_Sets.Cursor;
      Uniq_List       : NL_Types.Integer_List;
   begin
      for index in Values'Range loop
         Unique_Integers.Include (index);
      end loop;

      Ints_Curs := Unique_Integers.First;
      while Int_Sets.Has_Element (Ints_Curs) loop
         Uniq_List.Append (Int_Sets.Element (Ints_Curs));
         Int_Sets.Next (Ints_Curs);
      end loop;

      Sort (Uniq_List);
      declare
         Unique_Ints : constant Integer_Array := To_Integer_Array (Uniq_List);
      begin
         Inverse := Map_To_Integer (Values, Unique_Ints);
         return Unique_Ints;
      end;

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : Integer_Array) return NL_Types.Integer_List is
      use Int_Sets;
      use NL_Types.Integer_Sorting;
      Int_Value       : Integer;
      Unique_Integers : Int_Sets.Set;
      Ints_Curs       : Int_Sets.Cursor;
      Uniq_List       : NL_Types.Integer_List;
   begin
      for row in Values'Range loop
         Unique_Integers.Include (Values (row));
      end loop;

      Ints_Curs := Unique_Integers.First;
      while Int_Sets.Has_Element (Ints_Curs) loop
         Int_Value := Int_Sets.Element (Ints_Curs);
         Uniq_List.Append (Int_Value);
         Int_Sets.Next (Ints_Curs);
      end loop;

      Sort (Uniq_List);

      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : NL_Types.Integer_List)
                    return NL_Types.Integer_List is
      use Int_Sets;
      use NL_Types.Integer_Sorting;
      Int_Value       : Integer;
      Unique_Integers : Int_Sets.Set;
      Ints_Curs       : Int_Sets.Cursor;
      Uniq_List       : NL_Types.Integer_List;
   begin
      for index in Values.First_Index .. Values.Last_Index loop
         Unique_Integers.Include (Values (index));
      end loop;

      Ints_Curs := Unique_Integers.First;
      while Int_Sets.Has_Element (Ints_Curs) loop
         Int_Value := Int_Sets.Element (Ints_Curs);
         Uniq_List.Append (Int_Value);
         Int_Sets.Next (Ints_Curs);
      end loop;

      Sort (Uniq_List);

      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : Integer_Matrix) return NL_Types.Integer_List is
      use Int_Sets;
      use NL_Types.Integer_Sorting;
      Int_Value       : Integer;
      Unique_Integers : Int_Sets.Set;
      Ints_Curs       : Int_Sets.Cursor;
      Uniq_List       : NL_Types.Integer_List;
   begin
      for row in Values'Range loop
         for col in Values'Range (2) loop
            Unique_Integers.Include (Values (row, col));
         end loop;
      end loop;

      Ints_Curs := Unique_Integers.First;
      while Int_Sets.Has_Element (Ints_Curs) loop
         Int_Value := Int_Sets.Element (Ints_Curs);
         Uniq_List.Append (Int_Value);
         Int_Sets.Next (Ints_Curs);
      end loop;

      Sort (Uniq_List);

      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : Integer_Matrix) return Integer_Array is
   begin

      return To_Integer_Array (Unique (Values));

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : Integer_Matrix) return Int_Sets.Set is
      use Int_Sets;
      Unique_Integers : Int_Sets.Set;
   begin
      for row in Values'Range loop
         for col in Values'Range (2) loop
            Unique_Integers.Include (Values (row, col));
         end loop;
      end loop;

      return Unique_Integers;

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : NL_Types.Integer_List_Array)
                    return NL_Types.Integer_List is
      use Int_Sets;
      use NL_Types.Integer_Sorting;
      Value_Row   : NL_Types.Integer_List;
      Unique_Ints : Int_Sets.Set;
      Int_Curs    : Int_Sets.Cursor;
      Uniq_List   : NL_Types.Integer_List;
   begin
      for row in Values'Range loop
         Value_Row := Values (row);
         for col in Value_Row.First_Index .. Value_Row.Last_Index loop
            Unique_Ints.Include (Value_Row (col));
         end loop;
      end loop;

      Int_Curs := Unique_Ints.First;
      while Has_Element (Int_Curs) loop
         Uniq_List.Append (Element (Int_Curs));
         Next (Int_Curs);
      end loop;

      Sort (Uniq_List);

      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : Real_Float_Matrix) return NL_Types.Float_List is
      use Float_Sets;
      use NL_Types.Float_Sorting;
      Unique_Floats : Float_Sets.Set;
      Float_Curs    : Float_Sets.Cursor;
      Uniq_List     : NL_Types.Float_List;
   begin
      for row in Values'Range loop
         for col in Values'Range (2) loop
            Unique_Floats.Include (Values (row, col));
         end loop;
      end loop;

      Float_Curs := Unique_Floats.First;
      while Has_Element (Float_Curs) loop
         Uniq_List.Append (Element (Float_Curs));
         Next (Float_Curs);
      end loop;

      Sort (Uniq_List);

      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

end Encode_Utils;
