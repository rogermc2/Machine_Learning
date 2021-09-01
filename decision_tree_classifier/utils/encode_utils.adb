
--  Adapted from scikit-learn/scikit-learn.git sklearn/utils/_encode.py

with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
with Utilities;

package body Encode_Utils is

   package Bool_Sets is new Ada.Containers.Ordered_Sets (Boolean);
   package Float_Sets is new Ada.Containers.Ordered_Sets (Float);
   package Int_Sets is new Ada.Containers.Ordered_Sets (Integer);
   package UB_String_Sets is new Ada.Containers.Ordered_Sets (Unbounded_String);

   function Encode_Check_Unknown
     (Values : ML_Types.Value_Data_List; Uniques : ML_Types.Value_Data_List)
       return ML_Types.Value_Data_List;

   --  -------------------------------------------------------------------------

   function Encode_Check_Unknown
     (Values : ML_Types.Value_Data_List; Uniques : ML_Types.Value_Data_List)
       return ML_Types.Value_Data_List is
      use ML_Types;
      No_Inverse  : Natural_List :=
                      Classifier_Types.Natural_Package.Empty_Vector;
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

   function Encode (Values : ML_Types.Value_Data_List)
                     return ML_Types.Value_Data_List is
      Sorted_Values : ML_Types.Value_Data_List := Values;
      Uniques       : ML_Types.Value_Data_List :=
                        ML_Types.Value_Data_Package.Empty_Vector;
   begin

      ML_Types.Value_Data_Sorting.Sort (Sorted_Values);

      Classifier_Utilities.Print_Value_List
        ("Encode_Utils.Encode Uniques", Uniques);
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
                     return Natural_List is
      Diff          : ML_Types.Value_Data_List;
      Result        : Natural_List;
   begin
      Result := Map_To_Integer (Values, Uniques);
      if Check_Unknown then
         Diff := Encode_Check_Unknown (Values, Uniques);
         if not Diff.Is_Empty then
            New_Line;
            Put ("Encode_Error: Encode_Utils.Encode Values contains ");
            Put_Line ("previously unseen labels.");
            Classifier_Utilities.Print_Value_List  ("Unique list", Uniques);
            Classifier_Utilities.Print_Value_List ("Unseen labels", Diff);
            raise Encode_Error;
         end if;
      end if;

      return Result;

   end Encode;

   --  -------------------------------------------------------------------------
   --  Map each value based on its position in uniques.
   function Map_To_Integer (Values  : ML_Types.Value_Data_List;
                            Uniques : ML_Types.Value_Data_List)
                             return Natural_List is
      use ML_Types;
      use Value_Data_Package;
      Values_Curs  : Value_Data_Package.Cursor := Values.First;
      Uniques_Curs : Value_Data_Package.Cursor := Uniques.First;
      Result       : Natural_List;
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
            Utilities.Print_Value_Record
              ("Value record not found in Uniques",
               aValue);
         else
            Result (To_Index (Values_Curs)) := To_Index (Uniques_Curs) - 1;
         end if;
         Next (Values_Curs);
      end loop;

--        Put_Line ("Encode_Utils.Map_To_Integer done");
      return Result;

   end Map_To_Integer;

   --  -------------------------------------------------------------------------

   function Unique (Values : Natural_List) return Natural_List is
      use Natural_Package;
      use Natural_Sorting;
      Values_Curs : Natural_Package.Cursor := Values.First;
      aValue      : Natural;
      Uniq_List   : Natural_List;
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
                    Inverse        : out Natural_List)
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
      Inverse.Clear;

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
      Inverse := Map_To_Integer (Values, Uniq_List);
      return Uniq_List;

   end Unique;

   -------------------------------------------------------------------------

end Encode_Utils;
