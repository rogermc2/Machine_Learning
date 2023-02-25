
--  Adapted from scikit-learn/scikit-learn.git sklearn/utils/_encode.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Neural_Utilities;
--  with Printing;

package body Encode_Utils is

   package Bool_Sets is new Ada.Containers.Ordered_Sets (Boolean);
   function Encode_Check_Unknown
     (Values : ML_Types.Value_Data_List; Uniques : ML_Types.Value_Data_List)
      return ML_Types.Value_Data_List;
   package Float_Sets is new Ada.Containers.Ordered_Sets (Float);
   package UB_String_Sets is new
     Ada.Containers.Ordered_Sets (Unbounded_String);

   function Encode_Check_Unknown
     (Values : Integer_Array; Uniques : Integer_Array)
      return Integer_Array;
   function Map_To_Integer (Values  : ML_Types.Value_Data_List;
                            Uniques : ML_Types.Value_Data_List)
                            return NL_Types.Natural_List;

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
               --                 Printing.Print_Integer_Array ("Unique list", Uniques);
               --                 Printing.Print_Integer_Array ("Unseen labels", Diff);
               raise Encode_Error;
            end if;
         end;
      end if;

      return Result;

   end Encode;

   --  -------------------------------------------------------------------------

   function Encode (Values : ML_Types.Value_Data_List)
                    return ML_Types.Value_Data_List is
      Sorted_Values : ML_Types.Value_Data_List := Values;
      Uniques       : ML_Types.Value_Data_List :=
                        ML_Types.Value_Data_Package.Empty_Vector;
   begin

      ML_Types.Value_Data_Sorting.Sort (Sorted_Values);

      --        Printing.Print_Value_Data_List
      --          ("Encode_Utils.Encode Uniques", Uniques);
      Uniques := Unique (Values);
      return Uniques;

   end Encode;

   --  -------------------------------------------------------------------------
   --  Values : values to encode.
   --  Uniques : unique values in Values; Uniques needs to be sorted.
   --  Check_Unknown : if True check Values for values that are not in Uniques
   --  and raise an error.
   function Encode (Values        : ML_Types.Value_Data_List;
                    Uniques       : ML_Types.Value_Data_List;
                    Check_Unknown : Boolean := True)
                    return NL_Types.Natural_List is
      Diff          : ML_Types.Value_Data_List;
      Result        : NL_Types.Natural_List;
   begin
      Result := Map_To_Integer (Values, Uniques);
      if Check_Unknown then
         Diff := Encode_Check_Unknown (Values, Uniques);
         if not Diff.Is_Empty then
            New_Line;
            Put ("Encode_Error: Encode_Utils.Encode Values contains ");
            Put_Line ("previously unseen labels.");
            --              Printing.Print_Value_Data_List ("Unique list", Uniques);
            --              Printing.Print_Value_Data_List ("Unseen labels", Diff);
            raise Encode_Error;
         end if;
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
      Diff_List   : ML_Types.Integer_List;
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

   function Encode_Check_Unknown
     (Values : ML_Types.Value_Data_List; Uniques : ML_Types.Value_Data_List)
      return ML_Types.Value_Data_List is
      use ML_Types;
      No_Inverse  : NL_Types.Natural_List :=
                      NL_Types.Natural_Package.Empty_Vector;
      Unique_Vals : constant Value_Data_List :=
                      Encode_Utils.Unique (Values, No_Inverse);
      aVal        : Value_Record;
      Diff        : Value_Data_List;
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
   --  Map each value based on its position in uniques.
   function Map_To_Integer (Values  : ML_Types.Value_Data_List;
                            Uniques : ML_Types.Value_Data_List)
                            return NL_Types.Natural_List is
      use ML_Types;
      use Value_Data_Package;
      Values_Curs  : Value_Data_Package.Cursor := Values.First;
      Uniques_Curs : Value_Data_Package.Cursor := Uniques.First;
      Result       : NL_Types.Natural_List;
      aValue       : Value_Record;
   begin
      Result.Set_Length (Values.Length);
      for index in Result.First_Index .. Result.Last_Index loop
         Result (index) := 0;
      end loop;

      while Has_Element (Values_Curs) loop
         aValue := Element (Values_Curs);
         Uniques_Curs := Uniques.Find (aValue);
         if Uniques_Curs = No_Element then
            Put_Line ("Encode_Utils.Map_To_Integer error");
            Neural_Utilities.Print_Value_Record
              ("Value record not found in Uniques",
               aValue);
         else
            Result (To_Index (Values_Curs)) := To_Index (Uniques_Curs);
         end if;
         Next (Values_Curs);
      end loop;

      return Result;

   end Map_To_Integer;

   --  -------------------------------------------------------------------------

   function Unique (Values : Binary_Matrix) return ML_Types.Integer_List is
      use Int_Sets;
      use ML_Types.Integer_Sorting;
      Int_Value       : Natural;
      Unique_Integers : Int_Sets.Set;
      Ints_Curs       : Int_Sets.Cursor;
      Uniq_List       : ML_Types.Integer_List;
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
      use ML_Types.Integer_Sorting;
      Int_Value       : Integer;
      Unique_Integers : Int_Sets.Set;
      Ints_Curs       : Int_Sets.Cursor;
      Uniq_List       : ML_Types.Integer_List;
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

   function Unique (Values : ML_Arrays_And_Matrices.Boolean_Array)
                    return NL_Types.Natural_List is
      use NL_Types.Natural_Package;
      use NL_Types.Natural_Sorting;
      Value       : Natural;
      Uniq_List   : NL_Types.Natural_List;
   begin
      for index in Values'Range loop
         if Values (index) then
            Value := 1;
         else
            Value := 0;
         end if;
         if not Uniq_List.Contains (Value) then
            Uniq_List.Append (Value);
         end if;
      end loop;

      Sort (Uniq_List);
      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : Integer_Array; Inverse : out Natural_Array)
                    return Integer_Array is
      use Int_Sets;
      use ML_Types.Integer_Sorting;
      --        Routine_Name : constant String := "Encode_Utils.Unique ";
      Unique_Integers : Int_Sets.Set;
      Ints_Curs       : Int_Sets.Cursor;
      Uniq_List       : ML_Types.Integer_List;
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

   function Unique (Values : Integer_Array) return ML_Types.Integer_List is
      use Int_Sets;
      use ML_Types.Integer_Sorting;
      Int_Value       : Integer;
      Unique_Integers : Int_Sets.Set;
      Ints_Curs       : Int_Sets.Cursor;
      Uniq_List       : ML_Types.Integer_List;
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

   function Unique (Values : ML_Types.Integer_List)
                    return ML_Types.Integer_List is
      use Int_Sets;
      use ML_Types.Integer_Sorting;
      Int_Value       : Integer;
      Unique_Integers : Int_Sets.Set;
      Ints_Curs       : Int_Sets.Cursor;
      Uniq_List       : ML_Types.Integer_List;
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

   function Unique (Values : ML_Types.Integer_List; Inverse : out Natural_Array)
                    return ML_Types.Integer_List is
      use Int_Sets;
      use ML_Types.Integer_Sorting;
      Int_Value       : Integer;
      Unique_Integers : Int_Sets.Set;
      Ints_Curs       : Int_Sets.Cursor;
      Uniq_List       : ML_Types.Integer_List;
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
      declare
         Unique_Ints : constant Integer_Array := To_Integer_Array (Uniq_List);
      begin
         Inverse := Map_To_Integer (To_Integer_Array (Values), Unique_Ints);
      end;

      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

   function Unique (List : Integer_Array_List) return ML_Types.Integer_List is
      use Int_Sets;
      use ML_Types.Integer_Sorting;
      --  Routine_Name    : constant String :=
      --                      "Encode_Utils.Unique Integer_Array_List ";
      Int_Value       : Integer;
      Unique_Integers : Int_Sets.Set;
      Ints_Curs       : Int_Sets.Cursor;
      Uniq_List       : ML_Types.Integer_List;
   begin
      for index in List.First_Index .. List.Last_Index loop
         declare
            Values : constant Integer_Array := List (index);
         begin
            for col in Values'Range loop
               Unique_Integers.Include (Values (col));
            end loop;
         end;
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

   function Unique (Values : Integer_Matrix) return ML_Types.Integer_List is
      use Int_Sets;
      use ML_Types.Integer_Sorting;
      --        Routine_Name    : constant String :=
      --                            "Encode_Utils.Unique Integer_Matrix ";
      Int_Value       : Integer;
      Unique_Integers : Int_Sets.Set;
      Ints_Curs       : Int_Sets.Cursor;
      Uniq_List       : ML_Types.Integer_List;
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

   function Unique (Values : ML_Types.Array_Of_Integer_Lists)
                    return ML_Types.Integer_List is
      use Int_Sets;
      use ML_Types.Integer_Sorting;
      Value_Row   : ML_Types.Integer_List;
      Unique_Ints : Int_Sets.Set;
      Int_Curs    : Int_Sets.Cursor;
      Uniq_List   : ML_Types.Integer_List;
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

   function Unique (Values : ML_Arrays_And_Matrices.Integer_Array)
                    return NL_Types.Natural_List is
      use NL_Types.Natural_Package;
      use NL_Types.Natural_Sorting;
      Uniq_List   : NL_Types.Natural_List;
   begin
      for index in Values'Range loop
         if not Uniq_List.Contains (Values (index)) then
            Uniq_List.Append (Values (index));
         end if;
      end loop;

      Sort (Uniq_List);
      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : ML_Arrays_And_Matrices.Natural_Array)
                    return NL_Types.Natural_List is
      use NL_Types.Natural_Package;
      use NL_Types.Natural_Sorting;
      Uniq_List   : NL_Types.Natural_List;
   begin
      for index in Values'Range loop
         if not Uniq_List.Contains (Values (index)) then
            Uniq_List.Append (Values (index));
         end if;
      end loop;

      Sort (Uniq_List);
      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : NL_Types.Natural_List)
                    return NL_Types.Natural_List is
      use NL_Types.Natural_Package;
      use NL_Types.Natural_Sorting;
      Values_Curs : NL_Types.Natural_Package.Cursor := Values.First;
      aValue      : Natural;
      Uniq_List   : NL_Types.Natural_List;
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

   function Unique (Values : Unbounded_String_Array)
                    return ML_Types.Unbounded_List is
      use UB_String_Sets;
      use ML_Types.Unbounded_Sorting;
      Value             : Unbounded_String;
      Unique_UB_Strings : UB_String_Sets.Set;
      UB_Curs           : UB_String_Sets.Cursor;
      Uniq_List         : ML_Types.Unbounded_List;
   begin
      for row in Values'Range loop
         Unique_UB_Strings.Include (Values (row));
      end loop;

      UB_Curs := Unique_UB_Strings.First;
      while UB_String_Sets.Has_Element (UB_Curs) loop
         Value := UB_String_Sets.Element (UB_Curs);
         Uniq_List.Append (Value);
         UB_String_Sets.Next (UB_Curs);
      end loop;

      Sort (Uniq_List);

      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : Unbounded_String_Array_List)
                    return ML_Types.Unbounded_List is
      use UB_String_Sets;
      use ML_Types.Unbounded_Sorting;
      --  Routine_Name    : constant String :=
      --                      "Encode_Utils.Unique Unbounded_String_Array_List ";
      UB_Value        : Unbounded_String;
      Unique_Strings  : UB_String_Sets.Set;
      UB_Curs         : UB_String_Sets.Cursor;
      Uniq_List       : ML_Types.Unbounded_List;
   begin
      for index in Values.First_Index .. Values.Last_Index loop
         declare
            UB_Array : constant Unbounded_String_Array := Values (index);
         begin
            for col in UB_Array'Range loop
               Unique_Strings.Include (UB_Array (col));
            end loop;
         end;
      end loop;

      UB_Curs := Unique_Strings.First;
      while UB_String_Sets.Has_Element (UB_Curs) loop
         UB_Value := UB_String_Sets.Element (UB_Curs);
         Uniq_List.Append (UB_Value);
         UB_String_Sets.Next (UB_Curs);
      end loop;

      Sort (Uniq_List);

      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : Unbounded_String_Matrix)
                    return ML_Types.Unbounded_List is
      use UB_String_Sets;
      use ML_Types.Unbounded_Sorting;
      Value             : Unbounded_String;
      Unique_UB_Strings : UB_String_Sets.Set;
      UB_Curs           : UB_String_Sets.Cursor;
      Uniq_List         : ML_Types.Unbounded_List;
   begin
      for row in Values'Range loop
         for col in Values'Range (2) loop
            Unique_UB_Strings.Include (Values (row, col));
         end loop;
      end loop;

      UB_Curs := Unique_UB_Strings.First;
      while UB_String_Sets.Has_Element (UB_Curs) loop
         Value := UB_String_Sets.Element (UB_Curs);
         Uniq_List.Append (Value);
         UB_String_Sets.Next (UB_Curs);
      end loop;

      Sort (Uniq_List);

      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values : ML_Types.Value_Data_List)
                    return ML_Types.Value_Data_List is
      use ML_Types;
      use Int_Sets;
      use Value_Data_Package;
      use Value_Data_Sorting;
      Values_Curs       : Value_Data_Package.Cursor := Values.First;
      aValue            : Value_Record;
      Bool_Value        : Value_Record (Boolean_Type);
      Float_Value       : Value_Record (Float_Type);
      Int_Value         : Value_Record (Integer_Type);
      UB_String_Value   : Value_Record (UB_String_Type);
      Unique_Booleans   : Bool_Sets.Set;
      Unique_Floats     : Float_Sets.Set;
      Unique_Integers   : Int_Sets.Set;
      Unique_UB_Strings : UB_String_Sets.Set;
      Booleans_Curs     : Bool_Sets.Cursor;
      Floats_Curs       : Float_Sets.Cursor;
      Ints_Curs         : Int_Sets.Cursor;
      UB_Strings_Curs   : UB_String_Sets.Cursor;
      Uniq_List         : Value_Data_List;
   begin
      while Has_Element (Values_Curs) loop
         aValue := Element (Values_Curs);
         case aValue.Value_Kind is
         when Boolean_Type =>
            Unique_Booleans.Include (aValue.Boolean_Value);
         when Float_Type =>
            Unique_Floats.Include (aValue.Float_Value);
         when Integer_Type =>
            Unique_Integers.Include (aValue.Integer_Value);
         when UB_String_Type =>
            Unique_UB_Strings.Include (aValue.UB_String_Value);
         end case;
         Next (Values_Curs);
      end loop;

      Booleans_Curs := Unique_Booleans.First;
      while Bool_Sets.Has_Element (Booleans_Curs) loop
         Bool_Value.Boolean_Value := Bool_Sets.Element (Booleans_Curs);
         Uniq_List.Append (Bool_Value);
         Bool_Sets.Next (Booleans_Curs);
      end loop;

      Floats_Curs := Unique_Floats.First;
      while Float_Sets.Has_Element (Floats_Curs) loop
         Float_Value.Float_Value := Float_Sets.Element (Floats_Curs);
         Uniq_List.Append (Float_Value);
         Float_Sets.Next (Floats_Curs);
      end loop;

      Ints_Curs := Unique_Integers.First;
      while Int_Sets.Has_Element (Ints_Curs) loop
         Int_Value.Integer_Value := Int_Sets.Element (Ints_Curs);
         Uniq_List.Append (Int_Value);
         Int_Sets.Next (Ints_Curs);
      end loop;

      UB_Strings_Curs := Unique_UB_Strings.First;
      while UB_String_Sets.Has_Element (UB_Strings_Curs) loop
         UB_String_Value.UB_String_Value :=
           UB_String_Sets.Element (UB_Strings_Curs);
         Uniq_List.Append (UB_String_Value);
         UB_String_Sets.Next (UB_Strings_Curs);
      end loop;

      Sort (Uniq_List);

      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

   function Unique (Values         : ML_Types.Value_Data_List;
                    Inverse        : out NL_Types.Natural_List)
                    return ML_Types.Value_Data_List is
      use ML_Types;
      use Int_Sets;
      use Value_Data_Package;
      use Value_Data_Sorting;
      Values_Curs       : Value_Data_Package.Cursor := Values.First;
      aValue            : Value_Record;
      Bool_Value        : Value_Record (Boolean_Type);
      Float_Value       : Value_Record (Float_Type);
      Int_Value         : Value_Record (Integer_Type);
      UB_String_Value   : Value_Record (UB_String_Type);
      Unique_Booleans   : Bool_Sets.Set;
      Unique_Floats     : Float_Sets.Set;
      Unique_Integers   : Int_Sets.Set;
      Unique_UB_Strings : UB_String_Sets.Set;
      Booleans_Curs     : Bool_Sets.Cursor;
      Floats_Curs       : Float_Sets.Cursor;
      Ints_Curs         : Int_Sets.Cursor;
      UB_Strings_Curs   : UB_String_Sets.Cursor;
      Uniq_List         : Value_Data_List;
   begin
      while Has_Element (Values_Curs) loop
         aValue := Element (Values_Curs);
         case aValue.Value_Kind is
         when Boolean_Type =>
            Unique_Booleans.Include (aValue.Boolean_Value);
         when Float_Type =>
            Unique_Floats.Include (aValue.Float_Value);
         when Integer_Type =>
            Unique_Integers.Include (aValue.Integer_Value);
         when UB_String_Type =>
            Unique_UB_Strings.Include (aValue.UB_String_Value);
         end case;
         Next (Values_Curs);
      end loop;
      New_Line;

      Booleans_Curs := Unique_Booleans.First;
      while Bool_Sets.Has_Element (Booleans_Curs) loop
         Bool_Value.Boolean_Value := Bool_Sets.Element (Booleans_Curs);
         Uniq_List.Append (Bool_Value);
         Bool_Sets.Next (Booleans_Curs);
      end loop;

      Floats_Curs := Unique_Floats.First;
      while Float_Sets.Has_Element (Floats_Curs) loop
         Float_Value.Float_Value := Float_Sets.Element (Floats_Curs);
         Uniq_List.Append (Float_Value);
         Float_Sets.Next (Floats_Curs);
      end loop;

      Ints_Curs := Unique_Integers.First;
      while Int_Sets.Has_Element (Ints_Curs) loop
         Int_Value.Integer_Value := Int_Sets.Element (Ints_Curs);
         Uniq_List.Append (Int_Value);
         Int_Sets.Next (Ints_Curs);
      end loop;

      UB_Strings_Curs := Unique_UB_Strings.First;
      while UB_String_Sets.Has_Element (UB_Strings_Curs) loop
         UB_String_Value.UB_String_Value :=
           UB_String_Sets.Element (UB_Strings_Curs);
         Uniq_List.Append (UB_String_Value);
         UB_String_Sets.Next (UB_Strings_Curs);
      end loop;

      Sort (Uniq_List);
      Inverse := Map_To_Integer (Values, Uniq_List);

      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

end Encode_Utils;
