
--  Adapted from scikit-learn/scikit-learn.git sklearn/utils/_encode.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Printing;

package body Encode_Utils is

   --     package Bool_Sets is new Ada.Containers.Ordered_Sets (Boolean);
   --     package Float_Sets is new Ada.Containers.Ordered_Sets (Float);

   function Encode_Check_Unknown
     (Values : Integer_Array; Uniques : NL_Types.Integer_List)
      return NL_Types.Integer_List;

   --  -------------------------------------------------------------------------

   function Encode (Values : Integer_Array) return NL_Types.Integer_List is
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
                    Uniques       : NL_Types.Integer_List;
                    Check_Unknown : Boolean := True) return Natural_Array is
      Diff   : NL_Types.Integer_List;
      Result : Natural_Array (1 .. Values'Length);
   begin
      Result := Map_To_Integer (Values, Uniques);
      if Check_Unknown then
         Diff := Encode_Check_Unknown (Values, Uniques);
         if not Diff.Is_Empty then
            New_Line;
            Put ("Encode_Error: Encode_Utils.Encode Values contains ");
            Put_Line ("previously unseen labels.");
            Printing.Print_Integer_List ("Unique list", Uniques);
            Printing.Print_Integer_List ("Unseen labels", Diff);
            raise Encode_Error;
         end if;
      end if;

      return Result;

   end Encode;

   --  -------------------------------------------------------------------------

   function Encode_Check_Unknown
     (Values : Integer_Array; Uniques : NL_Types.Integer_List)
      return NL_Types.Integer_List is
      No_Inverse  : Natural_Array (1 .. Values'Length);
      Unique_Vals : constant NL_Types.Integer_List :=
                      Encode_Utils.Unique (Values, No_Inverse);
      aVal        : Integer;
      Found       : Boolean;
      Diff        : NL_Types.Integer_List;
   begin
      for index in Unique_Vals.First_Index .. Unique_Vals.Last_Index loop
         aVal := Unique_Vals (index);
         for u in Uniques.First_Index .. Uniques.Last_Index loop
            Found := Uniques (u) = aVal;
            if Found then
               Diff.Append (aVal);
            end if;
         end loop;
      end loop;

      return Diff;
   end Encode_Check_Unknown;

   --  -------------------------------------------------------------------------
   --  Map each value based on its position in uniques.
   function Map_To_Integer (Values  : Integer_Array;
                            Uniques : NL_Types.Integer_List)
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
         Uniques_Index := Uniques.First_Index;
         while Uniques_Index <= Uniques.Last_Index and not Found loop
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

   function Unique (Values : Natural_Array) return NL_Types.Natural_List is
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
      return Uniq_List;

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
      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : Integer_Array; Inverse : out Natural_Array)
                    return NL_Types.Integer_List is
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
      Inverse := Map_To_Integer (Values, Uniq_List);

      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

end Encode_Utils;
