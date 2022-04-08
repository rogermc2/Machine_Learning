
--  Adapted from scikit-learn/scikit-learn.git sklearn/utils/_encode.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Printing;

package body Encode_Utils is

--     package Bool_Sets is new Ada.Containers.Ordered_Sets (Boolean);
--     package Float_Sets is new Ada.Containers.Ordered_Sets (Float);

   function Encode_Check_Unknown
     (Values : Integer_Array; Uniques : Integer_Array) return Integer_Array;

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
   function Encode (Values        : Integer_Array; Uniques : Integer_Array;
                    Check_Unknown : Boolean := True) return Natural_Array is
      Diff   : Integer_Array := Values;
      Result : Natural_Array (1 .. Values'Length);
   begin
      Result := Map_To_Integer (Values, Uniques);
      if Check_Unknown then
         Diff := Encode_Check_Unknown (Values, Uniques);
         if not Diff.Is_Empty then
            New_Line;
            Put ("Encode_Error: Encode_Utils.Encode Values contains ");
            Put_Line ("previously unseen labels.");
            Printing.Print_Integer_Array ("Unique list", Uniques);
            Printing.Print_Integer_Array ("Unseen labels", Diff);
            raise Encode_Error;
         end if;
      end if;

      return Result;

   end Encode;

   --  -------------------------------------------------------------------------

   function Encode_Check_Unknown
     (Values : Integer_Array; Uniques : Integer_Array) return Integer_Array is
      No_Inverse  : Natural_Array;
      Unique_Vals : constant Integer_Array :=
                      Encode_Utils.Unique (Values, No_Inverse);
      aVal        : Integer;
      Diff        : Integer_Array;
   begin
      for index in Unique_Vals.First_Index .. Unique_Vals.Last_Index loop
         aVal := Unique_Vals.Element (index);
         if not Uniques.Contains (aVal) then
            Diff.Append (aVal);
         end if;
      end loop;

      return Diff;
   end Encode_Check_Unknown;

   --  -------------------------------------------------------------------------
   --  Map each value based on its position in uniques.
   function Map_To_Integer (Values, Uniques : Integer_Array)
                             return Natural_Array is
      use Integer_Package;
      Values_Curs  : Integer_Package.Cursor := Values.First;
      Uniques_Curs : Integer_Package.Cursor := Uniques.First;
      Result       : Natural_Array;
      aValue       : Integer;
   begin
      Result.Set_Length (Values.Length);
      for index in Result.First_Index .. Result.Last_Index loop
         Result (index) := 0;
      end loop;

      while Has_Element (Values_Curs) loop
         aValue := Element (Values_Curs);
         Uniques_Curs := Uniques.Find (aValue);
         Assert (Uniques_Curs /= No_Element,
            "Encode_Utils.Map_To_Integer error, Value not found in Uniques" &
            Integer'Image (aValue));

         Result (To_Index (Values_Curs)) := To_Index (Uniques_Curs);

         Next (Values_Curs);
      end loop;

      return Result;

   end Map_To_Integer;

   --  -------------------------------------------------------------------------

   function Unique (Values : Natural_Array) return Natural_Array is
      use Natural_Package;
      use Natural_Sorting;
      Values_Curs : Natural_Package.Cursor := Values.First;
      aValue      : Natural;
      Uniq_List   : Natural_Array;
   begin
      while Has_Element (Values_Curs) loop
         aValue := Element (Values_Curs);
         if not Uniq_List.Contains (aValue) then
            Uniq_List.Append (aValue);
         end if;
         Next (Values_Curs);
      end loop;

      Sort (Uniq_List);
      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : Integer_Array) return Integer_Array is
      use Int_Sets;
      use Integer_Package;
      use Integer_Sorting;
      Values_Curs     : Integer_Package.Cursor := Values.First;
      Int_Value       : Integer;
      Unique_Integers : Int_Sets.Set;
      Ints_Curs       : Int_Sets.Cursor;
      Uniq_List       : Integer_Array;
   begin
      while Has_Element (Values_Curs) loop
         Unique_Integers.Include (Element (Values_Curs));
         Next (Values_Curs);
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
                     return Integer_Array is
      use Int_Sets;
      use Integer_Package;
      use Integer_Sorting;

--        Routine_Name : constant String := "Encode_Utils.Unique ";
      Values_Curs       : Integer_Package.Cursor := Values.First;
      Unique_Integers   : Int_Sets.Set;
      Ints_Curs         : Int_Sets.Cursor;
      Uniq_List         : Integer_Array;
   begin
      while Has_Element (Values_Curs) loop
         Unique_Integers.Include (Element (Values_Curs));
         Next (Values_Curs);
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
