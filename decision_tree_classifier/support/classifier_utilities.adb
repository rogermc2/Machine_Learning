
with Ada.Containers.Ordered_Sets;
with Ada.Text_IO; use Ada.Text_IO;

package body Classifier_Utilities is

   package Bool_Sets is new Ada.Containers.Ordered_Sets (Boolean);
   package Float_Sets is new Ada.Containers.Ordered_Sets (Float);
   package Int_Sets is new Ada.Containers.Ordered_Sets (Integer);
   package UB_String_Sets is new Ada.Containers.Ordered_Sets (Unbounded_String);

   --  -------------------------------------------------------------------------
   --  Bin_Count counts the number of occurrences of each value in Numbers.
   --  Each bin gives the number of occurrences of its index value in Numbers.
   function Bin_Count (Numbers : Natural_List) return Natural_List is
      use Ada.Containers;
      use Natural_Package;
      aNumber    : Natural := 0;
      Max_Number : Natural := 0;
      Bins       : Natural_List;
   begin
      for index in Numbers.First_Index .. Numbers.Last_Index loop
         aNumber := Numbers.Element (index);
         if aNumber > Max_Number then
            Max_Number := aNumber;
         end if;
      end loop;

      Bins.Set_Length (Count_Type (Max_Number + 1));
      for index in Bins.First_Index .. Bins.Last_Index loop
         Bins (index) := 0;
      end loop;

      for index in Numbers.First_Index .. Numbers.Last_Index loop
         aNumber := Numbers.Element (index);
         Bins (aNumber + 1) := Bins (aNumber + 1) + 1;
      end loop;

      return Bins;

   end Bin_Count;

   --  -------------------------------------------------------------------------

   function Bin_Count (Numbers : ML_Types.Value_Data_List)
                       return Natural_List is
      use Ada.Containers;
      use ML_Types;
      use Natural_Package;
      aNumber    : Natural := 0;
      Max_Number : Natural := 0;
      Bins       : Natural_List;
   begin
      for index in Numbers.First_Index .. Numbers.Last_Index loop
         if Numbers.Element (index).Value_Kind /= Integer_Type then
            raise Value_Error with
              "Classifier_Utilities.Bin_Count called with non-integer data "
              & Data_Type'Image (Numbers.Element (index).Value_Kind);
         end if;

         aNumber := Numbers.Element (index).Integer_Value;
         if aNumber > Max_Number then
            Max_Number := aNumber;
         end if;
      end loop;

      Bins.Set_Length (Count_Type (Max_Number + 1));
      for index in Bins.First_Index .. Bins.Last_Index loop
         Bins (index) := 0;
      end loop;

      for index in Numbers.First_Index .. Numbers.Last_Index loop
         aNumber := Numbers.Element (index).Integer_Value;
         Bins (aNumber + 1) := Bins (aNumber + 1) + 1;
      end loop;

      return Bins;

   end Bin_Count;

   --  -------------------------------------------------------------------------

   procedure Clear (anArray : in out ML_Types.Value_Data_Array) is
   begin
      for index in anArray'Range loop
         anArray (index).Float_Value := 0.0;
      end loop;
   end Clear;

   --  -------------------------------------------------------------------------

   function Dot (L : Weight_List;
                 R : Natural_List) return Float is
      use Float_Package;
      Result : Float := 0.0;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result := Result + L.Element (index) * Float (R.Element (index));
      end loop;

      return Result;

   end Dot;

   --  -------------------------------------------------------------------------

   function Get_Column (Data       : ML_Types.List_Of_Value_Data_Lists;
                        Data_Index : Positive)
                        return ML_Types.Value_Data_List is
      use ML_Types;
      use Value_Lists_Data_Package;
      theList : Value_Data_List;
   begin
      for index in Data.First_Index .. Data.Last_Index loop
         theList.Append (Data.Element (Data_Index));
      end loop;
      return theList;

   end Get_Column;

   --  -------------------------------------------------------------------------

   function Search_Sorted_Value_List (List_A, List_B : ML_Types.Value_Data_List)
                                      return Integer_List is
      use ML_Types;
      use Integer_Package;
      use Value_Data_Package;
      use Value_Data_Sorting;
      Item    : Value_Record;
      Index_A : Positive;
      theList : Integer_List;
   begin
      if not Is_Sorted (List_A) then
         raise Value_Error with
           "Search_Sorted called with unsorted list.";
      end if;

      for index_B in List_B.First_Index .. List_B.Last_Index loop
         Item := List_B.Element (index_B);
         Index_A := List_A.Find_Index (Item);
         if Index_A /= Value_Data_Package.No_Index then
            theList.Append (Index_A);
         end if;
      end loop;
      return theList;
   end Search_Sorted_Value_List;

   --  -------------------------------------------------------------------------

   function To_Array (L : Integer_List) return Integer_Array is
      New_Array : Integer_Array (1 .. Integer (L.Length));
      A_Index   : Integer := 0;
   begin
      for index in L.First_Index .. L.Last_Index loop
         A_Index := A_Index + 1;
         New_Array (A_Index) := L.Element (index);
      end loop;
      return New_Array;
   end To_Array;

   --  -------------------------------------------------------------------------

   function To_Float_List (A : Float_Array) return Float_List is
      A_List : Float_List;
   begin
      for index in A'Range loop
         A_List.Append (A (index));
      end loop;
      return A_List;
   end To_Float_List;

   --  -------------------------------------------------------------------------

   function Unique (Nums : Integer_List) return Integer_List is
      use Int_Sets;
      use Integer_Package;
      Unique_Set : Int_Sets.Set;
      Int_Curs   : Integer_Package.Cursor := Nums.First;
      Set_Curs   : Int_Sets.Cursor;
      Nums_List  : Integer_List;
   begin
      while Has_Element (Int_Curs) loop
         Unique_Set.Include (Element (Int_Curs));
         Next (Int_Curs);
      end loop;

      Set_Curs := Unique_Set.First;
      while Has_Element (Set_Curs) loop
         Nums_List.Append (Element (Set_Curs));
         Next (Set_Curs);
      end loop;
      return Nums_List;
   end Unique;

   --  -------------------------------------------------------------------------

   function Unique_Values (Values         : ML_Types.Value_Data_List;
                           Inverse        : out Natural_List;
                           Return_Inverse : Boolean := False)
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
      if Return_Inverse then
         Values_Curs := Values.First;
         while Has_Element (Values_Curs) loop
            aValue := Element (Values_Curs);
            Inverse.Append (Uniq_List.Find_Index (aValue));
            Next (Values_Curs);
         end loop;
      end if;

      return Uniq_List;

   end Unique_Values;

   -------------------------------------------------------------------------

   function Unique_Integer_Array (Nums : ML_Types.Value_Data_Array)
                                     return Integer_Array is
      use Int_Sets;
      Unique_Set : Int_Sets.Set;
      Set_Curs   : Int_Sets.Cursor;
   begin
      for index in Nums'Range loop
         Unique_Set.Include (Nums (index).Integer_Value);
      end loop;

      declare
         Unique_Array : Integer_Array (1 .. Integer (Unique_Set.Length));
         Unique_Index : Integer := 0;
      begin
         Set_Curs := Unique_Set.First;
         while Has_Element (Set_Curs) loop
            Unique_Index := Unique_Index + 1;
            Unique_Array (Unique_Index) := Element (Set_Curs);
            Next (Set_Curs);
         end loop;
         return Unique_Array;
      end;
   end Unique_Integer_Array;

   --  -------------------------------------------------------------------------

   function Unique_Integer_Array (Nums : Integer_Array) return Integer_Array is
      use Int_Sets;
      Unique_Set : Int_Sets.Set;
      Set_Curs   : Int_Sets.Cursor;
   begin
      for index in Nums'Range loop
         Unique_Set.Include (Nums (index));
      end loop;

      declare
         Unique_Array : Integer_Array (1 .. Integer (Unique_Set.Length));
         Unique_Index : Integer := 0;
      begin
         Set_Curs := Unique_Set.First;
         while Has_Element (Set_Curs) loop
            Unique_Index := Unique_Index + 1;
            Unique_Array (Unique_Index) := Element (Set_Curs);
            Next (Set_Curs);
         end loop;
         return Unique_Array;
      end;
   end Unique_Integer_Array;

   --  -------------------------------------------------------------------------

   generic
      type Index_Type is (<>);
      type Vector_Type is  array (Index_Type) of aliased Float;
   procedure Print_Floats_Vector (Name : String; aVector : Vector_Type);

   procedure Print_Floats_Vector (Name : String; aVector : Vector_Type) is
   begin
      if Name = "" then
         Put ("  ");
      else
         Put (Name & ":  ");
      end if;
      for Index in aVector'Range loop
         Put (Float'Image (aVector (Index)) & "   ");
      end loop;
      New_Line;
   end Print_Floats_Vector;

   --  -------------------------------------------------------------------

   generic
      type Index_Type is (<>);
      type Vector_Type is  array (Index_Type) of aliased Integer;
   procedure Print_Integer_Vector (Name : String; aVector : Vector_Type);

   procedure Print_Integer_Vector (Name : String; aVector : Vector_Type) is
   begin
      if Name = "" then
         Put ("  ");
      else
         Put (Name & ":  ");
      end if;
      for Index in aVector'Range loop
         Put (Integer'Image (aVector (Index)) & "   ");
      end loop;
      New_Line;
   end Print_Integer_Vector;

   --  -------------------------------------------------------------------

   procedure Print_Integer_Array (Name : String; anArray : Integer_Array) is
   begin
      Put_Line (Name & ": ");
      for Index in anArray'First .. anArray'Last loop
         Put_Line (Integer'Image (Index) & ":  " &
                     Integer'Image (anArray (Index)));
      end loop;
      New_Line;
   end Print_Integer_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Float_Array (Name          : String; anArray : Float_Array;
                                Start, Finish : Integer) is
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      if Start >= anArray'First and then Finish <= anArray'Last then
         for Index in Start .. Finish loop
            Put (Float'Image (anArray (Index)) & "  ");
            Count := Count + 1;
            if Count > 4 then
               New_Line;
               Count := 1;
            end if;
         end loop;
      else
         Put_Line ("Print_Float_Array called with invalid start or finish index.");
      end if;
      New_Line;
   end Print_Float_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Float_List (Name : String; theList : Float_List) is
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      for Index in theList.First_Index .. theList.Last_Index loop
         Put (Integer'Image (Index) & ": " &
                Float'Image (theList.Element (Index)) & "   ");
         Count := Count + 1;
         if Count > 4 then
            New_Line;
            Count := 1;
         end if;
      end loop;
   end Print_Float_List;

   --  ------------------------------------------------------------------------

   procedure Print_Natural_List (Name : String; theList : Natural_List) is
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      for Index in theList.First_Index .. theList.Last_Index loop
         Put (Natural'Image (theList.Element (Index)) & "   ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;
   end Print_Natural_List;

   --  ------------------------------------------------------------------------

   procedure Print_Value_List (Name    : String;
                               theList : ML_Types.Value_Data_List) is
      use ML_Types;
      Value : Value_Record;
      Count : Integer := 1;
   begin
      Put (Name & ": ");
      for Index in theList.First_Index .. theList.Last_Index loop
         Value := theList.Element (Index);
         case Value.Value_Kind is
            when Boolean_Type => Put (Boolean'Image (Value.Boolean_Value));
            when Float_Type => Put (Float'Image (Value.Float_Value));
            when Integer_Type => Put (Integer'Image (Value.Integer_Value));
            when UB_String_Type => Put (To_String (Value.UB_String_Value));
         end case;
         Put ("   ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;
   end Print_Value_List;

   --  ------------------------------------------------------------------------

   procedure Print_Weights (Name : String; Data : Weight_List) is
      aWeight : Float;
      Count   : Integer := 1;
   begin
      Put_Line (Name & ": ");
      for Index in Data.First_Index .. Data.Last_Index loop
         aWeight := Data.Element (Index);
         Put (Float'Image (aWeight) & "   ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;
   end Print_Weights;

   --  ------------------------------------------------------------------------

end Classifier_Utilities;
